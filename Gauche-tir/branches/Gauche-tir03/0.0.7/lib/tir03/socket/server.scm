;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; socket server module

;;; WARN: このモジュールはobsoleteです。
;;;       このモジュールの新しい実体はtir03.socket.cth-serverになる予定です。

;;; ToDo: selected-ccidをキューにする

;;; ToDo: pauseの引数追加と、内部テーブル追加
;;; ToDo: コア部分をsocket/partcont-server.scm等にでも分割し、
;;;       read時に継続保持する部分だけをここに残す

;;; ToDo: DoS対策

;;; ToDo: readerのbufferを、文字列ではなく、文字列のlistとして保持する
;;;       (read-lineやread-blockについては、その方が効率が良い為)

;;; ToDo: 最大クライアント接続数制限

;;; ToDo: graceful shutdown(クライアントが全て処理を終えるまで待つ)を実装する

;;; ToDo: しっかり動くようなら、モジュール名を変えて、
;;;       単体モジュールとして公開しよう。

;;; ToDo: メモリリークのチェック

;;; このモジュールは、gauche.netとgauche.selectorによって接続を待ち受け、
;;; call/ccによってコルーチン化されたsocket serverを実現する。
;;; client-handlerは、read時にブロックした場合は、クライアントから
;;; 入力が届くまでは待たされ、その間に、他のクライアントの処理等が実行される。
;;; client-handlerが正常終了/エラー終了したなら、自動的に通信は切断される。
;;; 尚、client-handlerの返り値は捨てられる。
;;; read時にこっそり継続を保持して他のコルーチンに処理を渡したりしている。
;;; その為、socketからreadする際には、継続のextentが
;;; 切れないような位置でのみreadするように気を付けなくてはならない。
;;; (が、通常は、意識しなくても大丈夫な筈。)
;;; ToDo: ↑の説明を、もっとちゃんと書く。

;;; note: socket-server:start内部では、以下の内容をstderrに流す。
;;;       - :verbose-modeに#tを渡した時のデバッグログ
;;;       - client-handlerがエラーを投げて終了した時のstack trace

;;; note: 各種シグナル受信時の動作
;;; - SIGTERM : 現在実行中の子プロセスは途中終了/安全なポイントでserver終了。
;;; - SIGHUP : 安全なポイントまで来てからserver終了。
;;; - SIGINT : 安全なポイントまで来てからserver終了。
;;; - SIGPIPE : 常に無視(どのsocketが原因でSIGPIPEが来たのか判別できない為)。
;;;             client-handlerは、portから#<eof>を読んだり、
;;;             write時にエラー例外が出るかどうかで、接続の有無を判断する事。

;;; 使用上の注意:
;;; - 協調スレッド動作の為、一箇所でデッドロックや無限ループ等が発生すると、
;;;   サーバ全体が停止するので注意。
;;;   必ず終了処理を入れるようにする事。
;;; - ソケット通信の仕様上、通信や処理が中断される可能性がある。
;;;   具体的には、readした結果が#<eof>だったり、シグナル(SIGPIPE)が流れたり、
;;;   write時にerror例外が発生する可能性がある。
;;;   可能な限り、処理のトランザクションの安全を確保しようとする必要がある。



(define-module tir03.socket.server
  (use gauche.net)
  (use gauche.parameter)
  (use gauche.selector)

  (use srfi-2)

  (use tir03.socket.server.partcont)

  (export
    <socket-server>
    socket-server:start
    socket-server:shutdown

    socket-server:client-disconnect
    socket-server:pause

    ;; came from gauche.net
    <sockaddr>
    <sockaddr-in>
    <sockaddr-un>
    sockaddr-family
    sockaddr-name
    sockaddr-addr
    sockaddr-port
    ))
(select-module tir03.socket.server)


;;; --------
;;; class


(define-class <socket-server> ()
  (
   (sockaddr
     :accessor sockaddr-of
     :init-keyword :sockaddr
     :init-value #f) ; bind/listenするsockaddrを指定
   (select-interval
     :accessor select-interval-of
     :init-keyword :select-interval
     :init-value 1000000) ; default 1 sec

   ;; これは、クライアントから接続があった時に、以下のようなprocを指定する。
   ;; (proc ccid sockaddr socket-reader socket-writer)
   ;; ccidは、接続毎に連番で振られる数値。
   ;; socket-readerは、後述(ToDo: 説明を書きましょう)。
   ;; socket-writerは、後述(ToDo: 説明を書きましょう)。
   ;; このprocが終了/エラーを投げると、socketは自動的に閉じられる。
   ;; ※このprocは、必ず終了させる事！無限ループさせてはいけない！
   ;; ※socketの性質上、どの段階で接続が切断されるかは分からない為、
   ;;   上記のportに対してread/writeする際には必ず、#<eof>が返ってきたり、
   ;;   途中までしかreadできなかったり、writeしようとしたらエラーが発生したり、
   ;;   SIGPIPEが流れたり、といった事に備えなくてはならない！
   (client-handler
     :accessor client-handler-of
     :init-keyword :client-handler
     :init-value #f)
   ;; サーバのshutdown直前に呼ばれるフック。
   (shutdown-hook
     :accessor shutdown-hook-of
     :init-keyword :shutdown-hook
     :init-value #f)

   ;; selectorが反応せずにtimeoutした時に呼ばれるthunkを指定する。
   ;; これは、主に、各クライアントのタイムアウト判定等を行う際に使用できる。
   (idle-callee
     :accessor idle-callee-of
     :init-keyword :idle-callee
     :init-value #f)

   ;; for debug
   (verbose-mode
     :accessor verbose-mode-of
     :init-keyword :verbose-mode
     :init-value #f)

   ;; internal slot
   (server-socket
     :accessor server-socket-of
     :init-value #f)
   (selector
     :accessor selector-of
     :init-value (make <selector>))
   (client-counter ; client毎にuniqueなid数値を発行する為のカウンタ。
     :accessor client-counter-of
     :init-value 0)
   (session-table
     :accessor session-table-of
     :init-form (make-hash-table 'eqv?)) ; 毎回新規に生成するので、:init-form。
   ;; ↑keyはccid(client-counterの数値)。
   (shutdown-request ; コレが#tなら、serverの終了を開始する。
     :accessor shutdown-request-of
     :init-value #f)
   (selected-ccid ; select後に指定される、次に実行すべきccid。
     :accessor selected-ccid-of
     :init-value #f)
   ))


(define-method initialize ((self <socket-server>) initargs)
  (next-method)
  ;; check slot
  (unless (sockaddr-of self)
    (errorf "~a must be need to :sockaddr" (class-name (class-of self))))
  (unless (client-handler-of self)
    (errorf "~a must be need to :client-handler" (class-name (class-of self))))
  )


;;; --------


(define-condition-type <caught-sigterm> <error> #f)

(define socket-server (make-parameter #f)) ; methodのselfを省略した時用
(define in-client-handler? (make-parameter #f)) ; シグナル受信時の動作判定用
(define return/client-handler-cont (make-parameter #f))
(define client-session (make-parameter #f))
(define client-ccid (make-parameter #f))

;; 最小スリープ秒数を事前に求めておく
(define pause-sleeptime-default
  (if (zero? (values-ref (sys-gettimeofday) 1))
    0 ; osがgettimeofdayをサポートしていない。諦めて0とする
    (let* ((time1 (get-floatepoch))
           (time2 (get-floatepoch))
           )
      (* (- time2 time1) 4)))) ; 適当に、影響の少ない範囲で決定する

;;; --------
;;; internal utility


(define-syntax ignore-error
  (syntax-rules ()
    ((_ . bodies)
     (guard (e (else e)) . bodies))))


(define (verbosef self template . args)
  (when (ref self 'verbose-mode)
    (apply
      format
      (current-error-port)
      (string-append
        "~a: "
        template
        "\n")
      self
      args)))


(define (socket-disconnect/ignore-error socket)
  (ignore-error
    (socket-shutdown socket 2))
  (ignore-error
    (socket-close socket)))


(define socket-server-opened? server-socket-of)


(define (get-floatepoch)
  (receive (epoch micro-epoch) (sys-gettimeofday)
    (+ epoch
       (* micro-epoch 0.000001))))


;;; --------
;;; for session manipulate
(define (make-session . keywords)
  (let-keywords* keywords ((cont #f)
                           (buffer "")
                           (client-socket #f)
                           (wakeup-epoch (get-floatepoch))
                           (select-handler #f)
                           )
    (list
      cont ; car
      buffer ; cadr
      client-socket ; caddr
      wakeup-epoch ; cadddr
      select-handler ; (cut list-ref <> 4) ; ここ以降はset!不可。
      ;; ToDo: 要素が増えてきたらkeywordsのまま管理するとか、もう少し考え直す
      )))
(define session->cont car)
(define session->buffer cadr)
(define session->client-socket caddr)
(define session->wakeup-epoch cadddr)
(define session->select-handler (cut list-ref <> 4))


;;; --------


(define (socket-server-clean-up self)
  ;; selector
  (selector-delete! (selector-of self) #f #f #f)
  ;; selected-ccid
  (set! (selected-ccid-of self) #f)
  ;; session-table
  (for-each
    (cut hash-table-delete! (session-table-of self) <>)
    (hash-table-keys (session-table-of self)))
  ;; shutdown-request
  (set! (shutdown-request-of self) #f)
  ;; server-socket
  (set! (server-socket-of self) #f)
  )


;;; --------


(define (make-server-accept-handler self server-socket)
  (lambda (fd flag)
    (let ((client-socket (socket-accept server-socket))
          (ccid (let1 new-ccid (+ 1 (client-counter-of self))
                  (set! (client-counter-of self) new-ccid)
                  new-ccid))
          )
      ;; ToDo: このletrecはメモリリークの原因になるかも知れない/あとで確認する
      (letrec ((select-handler
                 (lambda (in flag)
                   ;; wakeup-epochを調べる
                   (let1 waittime (- (session->wakeup-epoch session)
                                     (get-floatepoch))
                     (if (positive? waittime)
                       (begin
                         ;; 今からwaittimeの間だけ、このソケットはselectから
                         ;; 無視される必要がある……
                         ;; どうすればいい？
                         ;; 普通に考えるなら、selectorから一時的に登録を
                         ;; 削除し、waittime後に復帰させる事だが……。
                         ;; ToDo: 実装を考える。
                         )
                       (begin
                         ;; 次に実行するccidに登録
                         (verbosef
                           self "received data ccid ~s" ccid)
                         (set! (selected-ccid-of self) ccid))))))
               (session (make-session
                          :client-socket client-socket
                          :select-handler select-handler
                          ))
               )
        (verbosef self "accepted to ~s (ccid=~s)" client-socket ccid)
        ;; select-handlerをselectorに登録
        (selector-add! (selector-of self)
                       (socket-input-port client-socket :buffering :modest)
                       select-handler
                       '(r))
        ;; session-tableに登録
        (hash-table-put! (session-table-of self) ccid session)
        ;; selected-ccid登録
        (set! (selected-ccid-of self) ccid)))))




(define (socket-server-open self)
  (verbosef self "socket-server opening...")
  (when (server-socket-of self)
    (error "already opened"))
  (let1 server-socket (make-server-socket
                        (sockaddr-of self)
                        :reuse-addr? #t
                        :sock-init (lambda (socket address)
                                     (socket-setsockopt
                                       socket SOL_SOCKET SO_KEEPALIVE 1)))
    ;; 各種スロットをclean up
    (socket-server-clean-up self)
    ;; accept-handlerをselectorに設定
    (selector-add!
      (selector-of self)
      (socket-fd server-socket)
      (make-server-accept-handler self server-socket)
      '(r))
    ;; server-socket保存
    (set! (server-socket-of self) server-socket)
    (verbosef self "socket-server opened.")))


;;; --------

;;; 一度にread-blockするサイズ
(define *bufsize* 4096)
;;; eof
(define *eof* (read-from-string ""))


;;; (value buffer eof?)を返す。
;;; - bufferは常に文字列。eof時には""が入る。
;;; - eof?は、bufferの後がeofかどうかを示す。
(define (reader:load-buffer! sock-in)
  ;; ToDo: 色々と最適化が可能な筈なので、最適化する事
  ;;       - (client-sock-in)が(まだ)読めないなら、old-bufferをそのまま返す
  ;;       - old-bufferが""なら、old-bufferはstring-appendには含めない
  ;;       - 要素が一つだけなら、string-appendは実行しない
  ;;       - loop開始前にclient-socketが有効かチェックした方がいい？
  ;;       - bufferは、文字列ではなく、文字列のlistとして扱うようにする
  (let ((old-buffer (session->buffer (client-session)))
        ;(client-socket (session->client-socket (client-session)))
        (eof? #f))
    (let1 buffer (apply
                   string-append
                   old-buffer
                   (let loop ()
                     (if (with-error-handler
                           (lambda (e) (set! eof? #t) #t)
                           (cut not (byte-ready? sock-in)))
                       '()
                       (let1 block (with-error-handler
                                     (lambda (e) *eof*)
                                     (cut read-block *bufsize* sock-in))
                         (if (eof-object? block)
                           (begin
                             (set! eof? #t)
                             '())
                           (cons
                             block
                             (loop)))))))
      ;; エラーが起こった時に不整合が起こらないように、保存しておく
      (reader:save-buffer! buffer)
      (values buffer eof?))))

(define (reader:save-buffer! buffer)
  (set! (session->buffer (client-session)) buffer))

(define (make-reader internal-reader)
  (lambda (buffer)
    (call-with-input-string
      buffer
      (lambda (p)
        (with-error-handler
          (lambda (e)
            (reader:save-buffer! (get-remaining-input-string p))
            (raise e))
          (lambda ()
            (let1 result (internal-reader p)
              (reader:save-buffer! (get-remaining-input-string p))
              result)))))))


(define (reader:read sock-in)
  (error "sorry, not implemented yet"))

(define (reader:read-line sock-in . opt-allow-byte-string?)
  (receive (buffer next-is-eof?) (reader:load-buffer! sock-in)
    (let1 reading! (make-reader
                     (cut apply read-line <> opt-allow-byte-string?))
      (if (or
            (string-scan buffer #\lf)
            (string-scan buffer #\cr))
        (reading! buffer)
        (if next-is-eof?
          (reading! buffer)
          (begin
            (socket-server:pause)
            (apply reader:read-line sock-in opt-allow-byte-string?)))))))

(define (reader:read-block nbytes sock-in)
  (receive (buffer next-is-eof?) (reader:load-buffer! sock-in)
    (let1 reading! (make-reader
                     (cut read-block nbytes <>))
      (if (<= nbytes (string-size buffer))
        (reading! buffer)
        (if next-is-eof?
          (reading! buffer)
          (begin
            (socket-server:pause)
            (reader:read-block nbytes sock-in)))))))


(define (get-socket-reader symbol sock-in)
  (case symbol
    ((read read-with-shared-structure read/ss)
     (cut reader:read sock-in))
    ((read-line) (lambda opt-allow-byte-string?
                   (apply reader:read-line sock-in opt-allow-byte-string?)))
    ((read-block) (cut reader:read-block <> sock-in))
    ((read-char peek-char read-byte peek-byte char-ready? byte-ready?) ; ToDo
     #f)
    (else #f)))


(define (get-socket-writer symbol sock-out)
  (case symbol
    ((write) (cut write <> sock-out))
    ((display) (cut display <> sock-out))
    ((write-with-shared-structure write/ss write*) (cut write/ss <> sock-out))
    ((print) (lambda expr
               (with-output-to-port
                 sock-out
                 (cut apply print expr))))
    ((newline) (cut newline sock-out))
    ((flush) (cut flush sock-out))
    ((write-char) (cut write-char <> sock-out))
    ((write-byte) (cut write-byte <> sock-out))
    (else #f)))






(define (execute-client-handler self ccid session)
  (let1 client-socket (session->client-socket session)
    (let (
          (sock-in (socket-input-port client-socket :buffering :modest))
          (sock-out (socket-output-port client-socket :buffering :line))
          (sockaddr (socket-address client-socket))
          )
      (define (client-terminate)
        (socket-disconnect/ignore-error client-socket)
        (client-remove self ccid))
      (define (socket-reader symbol . args)
        (let1 reader (or
                       (get-socket-reader symbol sock-in)
                       (errorf "socket-reader: unknown symbol '~s" symbol))
          (apply reader args)))
      (define (socket-writer symbol . args)
        (apply
          (or
            (get-socket-writer symbol sock-out)
            (errorf "socket-writer: unknown symbol '~s" symbol))
          args))
      (define (doit)
        (guard (e ((condition-has-type? e <caught-sigterm>)
                   ;; SIGTERM時は、stack dumpは表示しない
                   (verbosef self "caught SIGTERM")
                   (client-terminate))
                  (else
                    (verbosef self "client-handler error (ccid=~s)" ccid)
                    (report-error e)
                    (client-terminate)))
          (let1 pc (let/cc return
                     ;; client-handler実行中はフラグを立てる
                     (dynamic-wind
                       (cut in-client-handler? #t)
                       (lambda ()
                         (parameterize ((return/client-handler-cont return)
                                        (client-session session)
                                        (client-ccid ccid))
                           ((client-handler-of self) ccid
                                                     sockaddr
                                                     socket-reader
                                                     socket-writer)))
                       (cut in-client-handler? #f))
                     ;; 最後まで実行されたので、disconnect/finalizeする
                     (verbosef self "client-handler finished (ccid=~s)" ccid)
                     (client-terminate)
                     ;; 最後まで実行できた証として、#fを返す
                     #f)
            (when pc
              (set! (session->cont session) pc)))))

      (reset/pc
        (doit)))))




;;; 全てのクライアントをdisconnectしてから継続を実行する。
;;; これが、継続が無くなるまで続けられる。
;;; ToDo: これはgracefulとは呼べないので、名前を変える事
(define (all-client-graceful-disconnect self)
  (let ((server-socket (server-socket-of self))
        (initial-ccids (hash-table-keys (session-table-of self)))
        )
    (unless server-socket
      (error "not opened"))
    (unless (null? initial-ccids)
      (verbosef self "all client graceful disconnecting...")
      (let retry ((ccids initial-ccids))
        (unless (null? ccids)
          (let* ((ccid (car ccids))
                 (session (hash-table-get (session-table-of self) ccid)))
            (socket-disconnect/ignore-error (session->client-socket session))
            (reset/pc
              ((session->cont session)))
            (retry (hash-table-keys (session-table-of self))))))
      (verbosef self "all client graceful disconnected."))))




(define (socket-server-main self)
  (verbosef self "waiting for connection...")
  (with-signal-handlers (
                         ((list SIGHUP SIGINT)
                          =>
                          (lambda (sig)
                            (set! (shutdown-request-of self) #t)
                            (verbosef self "caught signal ~s" sig)))
                         (SIGTERM => (lambda (sig)
                                       (set! (shutdown-request-of self) #t)
                                       (if (in-client-handler?)
                                         (error
                                           <caught-sigterm> "caught SIGTERM")
                                         (verbosef self "caught SIGTERM"))))
                         (SIGPIPE => #f)
                         )
    (lambda ()
      (let (
            (selector (selector-of self))
            (interval (select-interval-of self))
            (callee (idle-callee-of self))
            )
        (let loop ((old-c 0))
          (let1 c (selector-select selector interval)
            (let1 ccid (selected-ccid-of self)
              (when ccid
                (let* ((session (hash-table-get (session-table-of self) ccid))
                       (old-cont (session->cont session)))
                  (if old-cont
                    (reset/pc
                      (old-cont))
                    (execute-client-handler self ccid session))) ; 初回実行
                (set! (selected-ccid-of self) #f))) ; selected-ccidをクリア
            (if (shutdown-request-of self)
              (let1 shutdown-hook (shutdown-hook-of self)
                (when shutdown-hook
                  (shutdown-hook))
                (all-client-graceful-disconnect self)) ; この後、closeされる
              (begin
                (and
                  callee
                  (eqv? old-c c) ; selectがtimeoutしたら#tになる(筈)
                  ;; note: ここでは、敢えて、エラー保護はしない。
                  ;;       (エラー発生=サーバはそのまま終了とする)
                  (callee))
                (loop c)))))))))


;;; --------


(define (remove-accept-handler self)
  (let1 server-socket (server-socket-of self)
    (when (eq? 'listening (socket-status server-socket))
      ;; まず最初に、<sockaddr-un>なら、socket-fileを削除
      ;; (race condition防止の為に、先に削除する)
      (let1 sockaddr (sockaddr-of self)
        (when (eq? 'unix (sockaddr-family sockaddr))
          (let1 sockfile (sockaddr-name sockaddr)
            (verbosef self "unlink to ~s" sockfile)
            (ignore-error (sys-unlink sockfile)))))
      ;; accept-handlerをクリア(fdの関係上、disconnectする前にクリアする)
      (selector-delete! (selector-of self) (socket-fd server-socket) #f #f)
      ;; server-socketを閉じ、新しい接続を受けないようにする
      (socket-disconnect/ignore-error server-socket))))




;;; WARN: socketのdisconnectは、(必要なら)先に自前で行っておく事！
;;; WARN: 継続を最後まで実行する事も、(必要なら)先に自前で行っておく事！
(define (client-remove self ccid)
  (verbosef self "remove to ccid ~s" ccid)
  (let1 session (hash-table-get (session-table-of self) ccid #f)
    (if (not session)
      (verbosef self "cannot found ccid ~s in session-table" ccid)
      (begin
        ;(verbosef self "target: ~s" session)
        ;; selectorから除去
        (selector-delete!
          (selector-of self)
          #f
          (session->select-handler session)
          #f)
        ;; session-tableからレコードを消す
        (hash-table-delete! (session-table-of self) ccid)
        (verbosef self "ccid ~s removed." ccid)))))




;;; socket-server-closeでは、clientの継続は破棄される。
;;; clientの継続を実行したい場合は、先にall-client-graceful-disconnectを呼ぶ。
(define (socket-server-close self)
  (verbosef self "socket-server closing...")
  (unless (server-socket-of self)
    (error "not opened"))
  ;; accept-handlerを停止する
  (remove-accept-handler self)
  ;; 残っているclient-socketを全てdisconnect
  (hash-table-for-each
    (session-table-of self)
    (lambda (ccid session)
      (socket-disconnect/ignore-error (session->client-socket session))
      (client-remove self ccid)))
  ;; clean up
  (socket-server-clean-up self)
  (verbosef self "socket-server closed."))


;;; --------
;;; export methods


;;; socketへのbind/listen/acceptを開始する。
(define-method socket-server:start ((self <socket-server>))
  (parameterize ((socket-server self))
    (socket-server-open self)
    (socket-server-main self)
    (socket-server-close self)))


;;; 動作しているsocket-serverに対して、accept-handlerを除去し、停止要求を送る。
;;; 実際の停止は、安全なポイントに移動してから実行される。
;;; (すぐには停止しない)
(define-method socket-server:shutdown ((self <socket-server>))
  (if (socket-server-opened? self)
    (begin
      (verbosef self "socket-server shutdown request accepted.")
      (remove-accept-handler self)
      (set! (shutdown-request-of self) #t))
    (error "not started")))


;;; 指定されたccidのソケットを切断し、該当クライアントの継続を破棄する。
;;; opt-keep-contに#tを指定する事で、ソケットのみ切断し、継続を残す事も可能。
;;; note: client-handler内部から自分自身のccidに対して呼んだ場合は、
;;;       継続は常に破棄されない。
;;;       (この使用法はあまり推奨されない。
;;;        client-handlerが正常に終了すれば、ソケットは自動的に閉じられる)
(define-method socket-server:client-disconnect ((self <socket-server>)
                                                (ccid <integer>)
                                                . opt-keep-cont)
  (let1 session (hash-table-get (session-table-of self) ccid #f)
    (if (not session)
      (verbosef self "cannot found ccid ~s in session-table" ccid)
      (begin
        (socket-disconnect/ignore-error (session->client-socket session))
        ;; keep-contが#tの時は、disconnectする事で、次のselect時にselectされ、
        ;; その際に継続が呼ばれる。
        ;; keep-contが#fの時は、すぐに(継続を含む)クライアント情報は破棄される
        (unless (get-optional opt-keep-cont #f)
          (if (eqv? ccid (selected-ccid-of self))
            (verbosef self "cannot clean myself (ccid=~s)" ccid)
            (client-remove self ccid)))))))



;;; for utility
(define-method socket-server:shutdown ()
  (if (socket-server)
    (socket-server:shutdown (socket-server))
    (error "not started")))
(define-method socket-server:client-disconnect ((ccid <integer>)
                                                . opt-keep-cont)
  (if (socket-server)
    (apply socket-server:client-disconnect (socket-server) ccid opt-keep-cont)
    (error "not started")))
(define-method socket-server:client-disconnect ()
  (cond
    ((not (socket-server)) (error "not started"))
    ((not (client-ccid)) (error "not in client-handler"))
    (else
      (socket-server:client-disconnect (socket-server) (client-ccid)))))


;;; --------
;;; socket & coroutine utility


;;; client-handler内から、別のコルーチンに制御を渡す。
;;; 引数として秒数(float値ok)を渡すと、その秒数が経過するまでは
;;; pauseを呼び出したコルーチンには制御が戻ってこない。
;;; 協調スレッド内では安易にsleepできない為、これをsleepの代用として使う。
(define (socket-server:pause . opt-floatepoch)
  ;; ToDo: 考えた結果、今のところは、floatepochオプションは
  ;;       一時的に無効にする事にした
  ;(let1 pause-sleeptime (get-optional opt-floatepoch pause-sleeptime-default)
  ;  (set! (session->wakeup-epoch (client-session))
  ;    (+ (get-floatepoch) pause-sleeptime)))
  (let/pc partcont
    ((return/client-handler-cont) partcont)))




;;; --------
;;; came from gauche.net


(define <sockaddr> <sockaddr>)
(define <sockaddr-in> <sockaddr-in>)
(define <sockaddr-un> <sockaddr-un>)
(define sockaddr-family sockaddr-family)
(define sockaddr-name sockaddr-name)
(define sockaddr-addr sockaddr-addr)
(define sockaddr-port sockaddr-port)


;;; --------


(provide "tir03/socket/server")


