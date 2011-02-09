;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ToDo: script-mainに見せる出力先は、close対策として、vportを使う
;;;       (closeされそうになったら、実際にはflushしてshutdownだけする)
;;;       socketなので、closeされてしまうと、入力の方も閉じられてしまう為。
;;;       そして、socketの完全切断がプロセス動作が一通り完了したという事を
;;;       伝える役目も兼ねている為、動作がちぐはぐになってしまう。

;;; ToDo: standard-input-port等の隔離は本来なら、execする側で行うべき気がする

;;; ToDo: isolate-stdio-portは、/dev/nullを使うのではなく、
;;;       vportを使って実装する

;;; note: 処理中にエラーがあった場合は、SIGCHLDによって親にエラーを通知する。
;;; note: 正常にsocket作成まで完了した事を示すには、
;;;       stdoutに改行を流す事でそれを親に通知する。
;;;       (親はパイプ起動している、という前提)
;;;       エラー終了時には改行は送らない事。

;;; note: .stderrと.exitファイルの削除はクライアント側に任せる
;;;       (クライアントが参照するタイミングが不明な為)

(define-module speedygosh.server-estp
  (use gauche.interactive)
  (use gauche.net)
  (use gauche.selector)
  (use gauche.fcntl)
  (use gauche.vport)
  (use gauche.uvector)
  (use gauche.parameter)
  (use srfi-1)
  (use speedygosh)

  (export
    speedygosh-server-boot
    ))
(select-module speedygosh.server-estp)

;; import from speedygosh
(define powered-by-speedygosh
  (with-module speedygosh powered-by-speedygosh))
(define terminate-request
  (with-module speedygosh terminate-request))
(define speedygosh-termination-handlers
  (with-module speedygosh speedygosh-termination-handlers))


(define *debug-log*
  (or
    ;"/tmp/sglog"
    ))
(define-macro (debugf formatter . args)
  (if *debug-log*
    `(with-output-to-file
       *debug-log*
       (lambda ()
         (format #t ,formatter ,@args))
       :if-exists :append)
    #t))

(define last-env (make-parameter #f))

;; note: */dev/null*を適当なファイルに変更する事で、デバッグしやすくなる
;;       (stdoutやstderrに流れてしまったものが読めるので。
;;        今のままだと/dev/nullに消えてしまうのでprintデバッグすらやりにくい)
(define */dev/null* "/dev/null")
(define (isolate-stdio-port thunk)
  (let ((in (open-input-string ""))
        (out (open-output-file */dev/null*)))
    (with-input-from-port
      in
      (lambda ()
        (with-output-to-port
          out
          thunk)))))


(define (load-script-to-module script-file)
  ;; ここでscript-fileを評価する際に、stdin/stdoutに余計なデータが流れ込まない
  ;; ようにする必要があるが、エラー時にそれをstderrには流したいので、
  ;; stdin/stdoutのみフィルタリングして評価する。
  ;; ……と思ったが、load-script-to-moduleを呼び出す外側で既に
  ;; isolate-stdio-portを呼んであったので、ここでは省略する。
  ;;
  ;; 元々は、無名モジュールを使っていたが、Gaucheのスクリプトは
  ;; userモジュールにロードされる、という約束だったので、
  ;; userモジュールをそのまま使う事に
  (let1 script-module (find-module 'user)
    ;; 予め、*program-name* と *argv* を、userからimportしておく必要がある
    ;(with-module user
    ;  (export *program-name* *argv*))
    ;(eval '(import user) script-module)
    ;; スクリプトを読み込む。エラー例外になったら、そのままエラーを投げる。
    ;; エラー内容はstderrに流す。
    (guard (e (else
                ;; なんか上手くスタックトレースを表示してくれないので、
                ;; 自前で表示させる
                (report-error e)
                (exit)))
      (load script-file :paths '(".") :environment script-module))
    script-module))


;; note: socketが生成できなかったらエラー例外を投げる事
(define (make-session-socket-or-error session-path-prefix)
  (let1 socket-path (string-append session-path-prefix ".sock")
    ;; まず、事前に、既にsocketが無い事を確認する意味も含めてunlinkしておく
    (sys-unlink socket-path)
    (make-server-socket 'unix socket-path)))


(define (lock-session! session-path-prefix)
  (let* ((lock-file (string-append session-path-prefix ".lock"))
         (lock-port (open-output-file lock-file))
         (flock (make <sys-flock>)))
    (set! (ref flock 'type) F_WRLCK)
    (set! (ref flock 'whence) 0) ; SEEK_SET = 0
    (set! (ref flock 'start) 0)
    (set! (ref flock 'len) 0)
    (if (sys-fcntl lock-port F_SETLK flock)
      lock-port
      #f)))

(define (make-error-vport path)
  ;; 初めて書き込まれた段階でファイルをopenするvportを返す
  ;; (要するに、全く使われなかったらfileを作らないvportを作る)
  (let* ((output-port-true #f)
         (vport (make <buffered-output-port>)))
    (set! (ref vport 'flush)
      (lambda (buf flag)
        (let1 len (u8vector-length buf)
          (if (zero? len)
            len
            (begin
              (unless output-port-true
                (set! output-port-true
                  (open-output-file path
                                    :buffering :none)))
              (write-block buf output-port-true)
              len)))))
    (set! (ref vport 'close)
      (lambda ()
        (when output-port-true
          (close-output-port output-port-true))))
    (set! (port-buffering vport) :none)
    vport))

(define (incomplete-filter string-list)
  (map
    (cut string-incomplete->complete <> :omit)
    string-list))

(define (estp-receive socket-in)
  ;; note: この時点で、不完全文字列のフィルタリングも行う事
  ;; 入力に問題があった場合は(values #f #f)を返す
  ;; ToDo: ESTP/0.1, ESTP/0.3にも対応させる事
  (guard (e (else
              (debugf "estp error: ~s\n" e)
              (values #f #f)))
    (let1 protocol-symbol (read socket-in)
      (debugf "dump protocol: ~s\n" protocol-symbol)
      (unless (eq? protocol-symbol 'ESTP/0.2)
        (error "invalid protocol"))
      (let* ((env (read socket-in))
             (argv (read socket-in)))
        (debugf "dump env: ~s\n" env)
        (debugf "dump argv: ~s\n" argv)
        (values (map incomplete-filter env) (incomplete-filter argv))))))

(define (with-env env argv thunk)
  ;; NB: 環境変数には、初期設定として、
  ;;     サーバを起動したspeedygoshコマンドの環境変数がコピーされて
  ;;     既に設定されている。
  ;;     但し、サーバを起動したspeedygoshコマンドが、常に一番最初に
  ;;     アクセスしてくる仕組みに(少なくとも現在は)なっている為、
  ;;     envのkeyを全てsys-unsetenvしてしまえば、environは空になる。
  ;;     二回目以降もそれを繰り返せば、以前の環境変数が残ってしまって
  ;;     次回のプロセス実行時に影響を与える、というような心配は無い。
  ;;     尚、下にも書いているが、PATHだけは残すようにしておく。
  (dynamic-wind
    (lambda ()
      ;; 新しい環境変数をセットする
      (last-env env)
      (for-each
        (lambda (name+val)
          (sys-setenv (car name+val) (cadr name+val) #t))
        env))
    (lambda ()
      ;; note: *program-name* と *argv* をfluid-let しなくてはならない
      (with-module user
        (fluid-let ((*program-name* (guard (e (else *program-name*))
                                      (car argv)))
                    (*argv* (guard (e (else *argv*))
                              (cdr argv))))
          (thunk))))
    (lambda ()
      ;; セットした環境変数を消す
      (for-each
        (lambda (name+val)
          (sys-unsetenv (car name+val)))
        env))))

(define (script-main-execute script-module script-main argv)
  (eval
    ;`(with-module ,script-module (,script-main (quote ,argv)))
    `(,script-main (quote ,argv))
    script-module))




(define (server-start socket
                      script-module
                      script-main
                      session-path-prefix
                      timeout
                      maxruns
                      errorlog)
  (define (return-code-set! return-code)
    (unless (zero? return-code)
      (with-output-to-file
        (string-append
          session-path-prefix ".exit")
        (cut write return-code))))

  (let ((selector (make <selector>))
        (error-flag #f) ; script-mainでエラーが起きたら#tにする
        (lock-port #f) ; 単にすぐにgcさせない為だけに用意
        (runs 0) ; 実行回数
        )
    (selector-add!
      selector
      (socket-fd socket)
      (lambda (fd flag)
        (debugf "client connected\n")
        (let1 client-socket (socket-accept socket)
          (let ((in  (socket-input-port client-socket :buffering :full))
                (out (socket-output-port client-socket))
                (err (or
                       (and
                         errorlog
                         (not (string=? errorlog ""))
                         (guard (e (else #f))
                           (open-output-file errorlog
                                             :if-exists :append
                                             :buffering :none)))
                       (make-error-vport (string-append
                                           session-path-prefix ".stderr"))))
                )
            ;; まず、ESTPからenvとargvを取得する。
            (receive (env argv) (estp-receive in)
              (if (not env)
                (socket-disconnect/ignore-error client-socket)
                (with-error-handler
                  (lambda (e)
                    (debugf "error occured: ~a\n" (ref e 'message))
                    (set! error-flag #t)
                    (report-error e)
                    ;; エラーが起きたらそのままプロセス終了するので、
                    ;; その準備を行う。
                    ;; 但し、クライアントとの通信の切断は
                    ;; 一番最後にしなくてはならない。
                    ;; (ロック関連のrace conditionを避ける為)
                    ;; ここでは、unix domain socket経由という前提で、
                    ;; client-socketは最後まで明示的に切断せずに、
                    ;; そのままプロセス終了する事を
                    ;; 通信の切断の代わりとする。
                    ;; (tcpとかだと多分これはまずい)
                    (return-code-set! 70))
                  (lambda ()
                    (debugf "communicate start\n")
                    (with-input-from-port
                      in
                      (lambda ()
                        (with-output-to-port
                          out
                          (lambda ()
                            (with-error-to-port
                              err
                              (lambda ()
                                (with-env
                                  env argv
                                  (lambda ()
                                    (let1 return-code (x->integer
                                                        (script-main-execute
                                                          script-module
                                                          script-main
                                                          argv))
                                      (debugf "execute done\n")
                                      (inc! runs)
                                      (return-code-set! return-code))))))))))
                    ;; 終了処理を行う
                    ;; ToDo: ここで閉じてすぐに次の接続が来た場合、
                    ;;       次で終了する予定だった時にrace conditionが
                    ;;       発生しそうな気がするので、あとでチェックして直す事
                    (close-output-port err)
                    (close-output-port out)
                    (close-input-port in)
                    (socket-disconnect/ignore-error client-socket))))))))
      '(r))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (let loop ()
          (debugf "selecting\n")
          (let1 counter (selector-select selector `(,timeout 0))
            ;(debugf "runs=~d maxruns=~d cond=~s\n" runs maxruns (< runs maxruns))
            (cond
              (error-flag #f) ; エラーが起きたならすぐ終了
              ;; NB: ここでrace conditionが発生する可能性がある。
              ;;     しかし、エラー例外の時まで面倒を見ても仕方が無い気も
              ;;     するので、とりあえずこのままとする
              ((and
                 (not (zero? counter)) ; 処理を行った。
                 (< runs maxruns) ; まだ最大処理回数に達してない。
                 (not (terminate-request)) ; サーバ終了フラグが立っていない。
                 )
               (loop)) ; 普通に処理。次の接続を待つ
              (else
                ;; タイムアウトしたので、サーバ終了処理を行う
                ;; もうクライアントがつないでこないように、ロックする
                ;; (race condition防止の意味もあるので必要)
                (set! lock-port (lock-session! session-path-prefix))
                ;; ロックに成功したらそのままdynamic-windの終了処理へ。
                (if lock-port
                  ;; ユーザが設定した終了ハンドラを実行する
                  (unless (null? (speedygosh-termination-handlers))
                    ;; 最後に利用した環境変数を再セットする
                    (for-each
                      (lambda (name+val)
                        (sys-setenv (car name+val) (cadr name+val) #t))
                      (last-env))
                    ;; ハンドラを順に全部実行
                    (for-each
                      (lambda (x)
                        (x))
                      (speedygosh-termination-handlers)))
                  ;; ロック失敗。おそらくクライアントが接続したので再実行
                  (begin
                    (set! timeout 0) ; 但し、もう待たなくて良い
                    (loop))))))))
      (lambda ()
        ;; 一応、シグナルが流れた時も、ここで必ずcleanup処理が行われる。
        ;; ToDo: しかし、シグナルが流れた時はerror-flagを#tにしたい。
        (selector-delete! selector #f #f #f) ; selectorをクリアする
        (server-shutdown socket session-path-prefix lock-port error-flag)))))

(define-syntax ignore-error
  (syntax-rules ()
    ((_ . bodies)
     (guard (e (else e)) . bodies))))

(define (socket-disconnect/ignore-error socket)
  (ignore-error
    (socket-shutdown socket 2))
  (ignore-error
    (socket-close socket)))

;; note:
;; - error-flagが#fの時は、以下の動作を行う
;; -- selectorのクリア
;; -- socketを閉じ、unlinkする
;; -- ロックファイルの削除(ロックしている前提の為)
;; - error-flagが#tの時は、以下の動作を行う
;; -- selectorのクリア
;; -- socketを閉じ、unlinkする
;; -- ロックはクライアント側から行われている筈なので、削除しない事
(define (server-shutdown socket session-path-prefix lock-port error-flag)
  (debugf "server shutdown\n")
  ;; selectorをクリアする
  ;; socketを閉じ、unlinkする
  (socket-disconnect/ignore-error socket)
  (sys-unlink (string-append session-path-prefix ".sock"))
  ;; 非エラー終了(=タイムアウト)なら、一時ファイルの削除も念の為行う
  (unless error-flag
    (sys-unlink (string-append session-path-prefix ".exit"))
    (sys-unlink (string-append session-path-prefix ".stderr")))
  ;; 最後に、非エラー終了(=タイムアウト)なら、自分でロックファイルを削除する
  (unless error-flag
    (sys-unlink (string-append session-path-prefix ".lock"))
    ;; 一応、ロックファイルもcloseしておく
    (when lock-port
      (ignore-error
        (close-output-port lock-port)))))

;;; 動作概要:
;;; - speedygoshコマンドから、以下のような引数で起動が行われる。
;;;   gosh -b -uspeedygosh.server-estp -Espeedygosh-main -Eexit -- \
;;;   script-file session-path-prefix timeout maxruns errorlog |
;;;   尚、これらの値は不完全文字列である可能性がある。
;;;   script-nameは、*program-name*で取得可能。
;;;   また、*argv*には、(session-path-prefix timeout)が入る。
;;;   このプロセスのstdoutは親プロセスにパイプ接続されており、
;;;   これが切断される事が、サーバ動作の準備完了の合図となる。
(define (speedygosh-server-boot)
  (set-signal-handler! SIGPIPE #f) ; SIGPIPEはあらゆる局面で無視する
  (isolate-stdio-port ; stdioは捨てる(しかしまだ切断はしない)
    (lambda ()
      (let ((script-file (with-module user *program-name*))
            (session-path-prefix (car (with-module user *argv*)))
            (timeout (x->number (list-ref (with-module user *argv*) 1 90)))
            (maxruns (x->number (list-ref (with-module user *argv*) 2 1024)))
            (errorlog (list-ref (with-module user *argv*) 3 #f))
            )
        ;; ToDo: 引数のチェック
        (parameterize ((powered-by-speedygosh #t)
                       (terminate-request #f)
                       (speedygosh-termination-handlers '())
                       (last-env #f)
                       )
          ;; まずスクリプトを読み込み、そのmain関数を取得する
          ;; 尚、この際にstderrにエラー内容が流されるかも知れない為、
          ;; まだこの段階ではstdin/stdout/stderrの切り離しは行わない。
          (let* ((script-module (load-script-to-module script-file))
                 (script-main (global-variable-ref script-module 'main #f)))
            (unless script-main
              (error "cannot found proc of main" script-file))
            ;; socketを生成する
            (let1 socket (make-session-socket-or-error session-path-prefix)
              ;; socketの生成に成功したので、準備段階としてはもう
              ;; エラーになる事は無い筈なので、stderrも捨てる
              (with-error-to-port
                (current-output-port) ; stdoutを流用する
                (lambda ()
                  ;; socketを生成し、socket生成が正常に完了した事を示す為に、
                  ;; 改行を送ってから、親とのパイプを切断して通知する。
                  ;; ついでにstdinとstderrも閉じておく
                  ;; (開いたままだと親プロセスの終了に支障がある)
                  (close-input-port (standard-input-port))
                  (newline (standard-output-port)) ; 合図として必要
                  (close-output-port (standard-output-port))
                  (close-output-port (standard-error-port))
                  ;; ToDo: standard-input-port等が空のままだと、
                  ;;       sys-system実行時等での入出力先が問題になる為、
                  ;;       何らかの手段を用意したいが、そもそも可能なのか？
                  ;; サーバ待ち受けを開始する
                  (debugf "server start\n")
                  (server-start socket
                                script-module
                                script-main
                                session-path-prefix
                                timeout
                                maxruns
                                errorlog
                                ))))))))))


(provide "speedygosh/server-estp")

