;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; "socket server that work with coroutine" module

;;; ToDo: selected-csid-queueとslept-csid-alistは、alistではなく
;;;       hash-tableの方が効率が良いかも知れない

;;; ToDo: 構造を考え直す必要があるような気がする。
;;; - 現在、全てのcsidを持つクライアントは、
;;;   selected-csid-queue、selector-timeout-alist(とselector)、slept-csid-alist
;;;   のいずれか一つに登録される状態にある。
;;;   これらは決して重複しないので、微妙に効率が悪い。
;;;   しかし、selected-csid-queueは順序が重要であり、
;;;   selector-timeout-alistとslept-csid-alistは検索性が重要なので、
;;;   一緒にする事は無理なようにも思える。
;;;   本当に無理かどうか、あとで考える。

;;; note: selector側から見ると、以下のような仕様になる(筈)
;;; - 最初は、server-accept-handlerだけがselectorに登録されており、
;;;   selectのtimeout値には#fが指定された状態でselectされる。
;;; - clientが接続すると、client-handlerが呼び出されるが、
;;;   この時点ではまだselectorにclient-socketの監視は追加されない。
;;;   client-handler内からssc:wait-data-from-socketが呼び出された
;;;   段階でのみ、selectorに監視が追加される。
;;; - ssc:wait-data-from-socketで監視が追加された後にselectされたら、
;;;   その段階でselectorから早速、監視を除去する。
;;; ToDo: 現状のソースは、この仕様の通りになっていないと思われる。
;;;       (クライアントが接続したらすぐselectorに登録してしまう)
;;;       直す必要がある。

;;; ToDo: 現在接続中のcsidの一覧を取得するmethodを提供する
;;;       (session-tableからkeysで取り出せばok)

;;; ToDo: socketから安全に(ブロックしないように)データを読み出す為の
;;;       ユーティリティ関数が必要(あとで書く)

;;; ToDo: DoS対策
;;; ToDo: 最大クライアント接続数制限
;;; ToDo: しっかり動くようなら、モジュール名を変えて、
;;;       単体モジュールとして公開しよう。
;;; ToDo: メモリリークのチェック
;;;       (httpdを実装し、abをかけ、メモリ使用量の変化を見る)

;;; このモジュールは、gauche.netとgauche.selectorによって接続を待ち受け、
;;; partcont.scmによって協調スレッドとして動作するsocket serverを実現する。
;;;
;;; サーバ内のclient-handlerには、生のsocket portが渡される為、
;;; クライアントハンドラは、read時にブロックしてしまわないように
;;; 注意する必要がある。
;;; また、SIGPIPEはデフォルトで無視されるようになっているが、
;;; socket portへの読み書きでEOFやエラーが投げられる事に対して、
;;; 特に対処はしていないので、I/Oの扱いは必ず失敗時の事を考える必要がある。
;;;
;;; 一つのクライアントハンドラから他のクライアントハンドラに制御を渡したり、
;;; sleepを行いたい時には、ssc:sleepを使う事。
;;; 協調スレッドの特性上、そのままsys-sleep等を使うと
;;; サーバ全体が停止してしまう為、この点には注意する事。


;;; note: pcss:start内部では、以下の内容をstderrに流す。
;;; - :verbose-modeに#tを渡した時のデバッグログ
;;; - client-handlerがエラーを投げて終了した時のstack trace

;;; note: 各種シグナル受信時の動作
;;; - SIGTERM : 現在実行中のclient-handlerは途中終了。
;;;             安全なポイントに来てからserverも終了。
;;;             残っている継続退避状態の他のクライアントハンドラは
;;;             続行されない。
;;; - SIGHUP : 現在実行中のclient-handlerはssc:pauseやssc:sleepされるか、
;;;            完全に実行完了するまでは実行され続ける。
;;;            それ以外はSIGTERMと同じ。
;;; - SIGINT : SIGHUPと同じ。
;;; - SIGPIPE : 常に無視(どのsocketが原因でSIGPIPEが来たのか判別できない為)。
;;;             client-handlerは、portから#<eof>を読んだり、
;;;             write時にエラー例外が出るかどうかで、接続の有無を判断する事。

;;; ToDo: graceful-shutdownをサポートしたので、
;;;       シグナルの割り当てを変更してもいいかも
;;;       (しかし、apacheのgracefulはrestartだが、こっちのgracefulは
;;;        shutdownなので、SIGHUPにgracefulを割り当てるのはいいが、
;;;        再起動を期待されても困るような気がする……とは言え、
;;;        このモジュールを使ってgraceful restartを実装するのに楽なように、
;;;        SIGHUPはgraceful-shutdownを割り当てて良いのは確かに思える)

;;; 使用上の注意:
;;; - socketのread時には、決してブロックしないように注意する事。
;;; - 協調スレッド動作の為、一箇所でデッドロックや無限ループ等が発生すると、
;;;   サーバ全体が停止するので注意。
;;;   必ず終了処理を入れるようにする事。
;;;   また、ブロックしてしまうような呼び出しがある場合にも注意する事。
;;; - 同様に、sleep動作の際には必ずssc:sleepを使う事
;;; - ソケット通信の仕様上、通信や処理が中断される可能性がある。
;;;   具体的には、readした結果が#<eof>だったり、
;;;   write時にerror例外が発生する可能性がある。
;;;   可能な限り、処理のトランザクションの安全を確保しようとする必要がある。
;;; - client-handler手続きが最後まで実行し終わったり、エラー例外を
;;;   投げた時には、クライアントとのコネクションは自動的に切断される。



(define-module tir03.socket.server.coroutine
  (use gauche.net)
  (use gauche.parameter)
  (use gauche.selector)
  (use util.queue)

  (use srfi-1)
  (use srfi-2)

  (use tir03.util.partcont)

  (export ; and usage
    ;; class
    <socket-server-coroutine>
    ;; (make <socket-server-coroutine> ...)
    ;; - make引数は下記の定義部分を見る事。

    ;; * ssc:server-というprefixがついているものは、サーバを操作するmethod。
    ssc:server-start
    ;; (ssc:start object-of-socket-server-coroutine)
    ;; - ソケットを作成し、server動作を開始する。
    ;; - 返り値として、○○が返される(あとで仕様を考える)。
    ;;   多分、処理したcsidの数とか、通常shutdownかgraceful-shutdownかとか、
    ;;   そんなのを返す予定。
    ssc:server-shutdown
    ;; (ssc:shutdown object-of-socket-server-coroutine)
    ;; (ssc:shutdown)
    ;; - client-handlerの中から呼ぶ。
    ;;   サーバのshutdown予約を行う。
    ;;   実際のshutdownは、サーバの動作が「安全なポイント」に
    ;;   来た段階で行われる。
    ;;   具体的には、ssc:shutdownを呼び出したclient-handlerが
    ;;   ssc:sleepやssc:pauseを実行したり、最後まで実行された段階で
    ;;   実際のshutdownが行われる。
    ;; - shutdown後は、ssc:start直後へ制御が渡される。
    ;; - 引数は省略可能(client-handler内のみ)。
    ssc:server-shutdown-graceful
    ;; (ssc:shutdown-graceful object-of-socket-server-coroutine)
    ;; (ssc:shutdown-graceful)
    ;; - サーバのgraceful shutdown予約を行い、
    ;;   サーバに対して接続しているクライアントの数が0になった段階で
    ;;   サーバのshutdownを行う。
    ;;   尚、graceful shutdown予約が行われてもサーバのクライアント待ち受けが
    ;;   停止したりはされない。
    ;; - まだ未実装。

    ;; * ssc:server-というprefixがついていないものは、client-handler内で使う。
    ssc:wait-data-from-socket
    ;; (ssc:wait-data-from-socket timeout-second)
    ;; (ssc:wait-data-from-socket)
    ;; - 現在のclient-handlerが受け持っているsocketが、
    ;;   読み出し可能になるか、timeout-second秒が経過するまで待つ。
    ;;   この手続きは真偽値を返す。
    ;;   返り値が#tなら、socketは読み出し可能になった。
    ;;   返り値が#fなら、timeout-second秒が経過したが、
    ;;   socketは読み出し可能にならなかった。
    ;;   timeout-second秒の指定は省略可能で、その場合は#fが採用される。
    ;;   timeout-secondが#fの場合は、読み込み可能になるまで永遠に待ち続ける。
    ;;   (尚、socketが切断された場合は、socketからEOFが読み出し可能になるので、
    ;;    返り値として#tが返される。)
    ;; - 尚、timeout-secondは小数でもok。
    ssc:pause
    ;; (ssc:pause)
    ;; - コルーチン制御コマンド。
    ;;   他のクライアントが存在するなら、そっちに制御を渡す。
    ;;   他のクライアントが存在しないなら、そのまま制御が戻る。
    ssc:sleep
    ;; (ssc:sleep second)
    ;; - コルーチン制御コマンド。
    ;;   second秒が経過するまでは、呼び出したコルーチンには制御が渡されない。
    ;;   second秒が経過した後は、ssc:pauseと同様のタイミングで制御が戻る。
    ;;   sleepの代用。
    ;; - 尚、secondは小数でもok。
    ssc:disconnect
    ;; (ssc:disconnect object-of-socket-server-coroutine csid . opt-drop-cont)
    ;; (ssc:disconnect csid . opt-drop-cont)
    ;; - client-handlerに関連付けられたcsidを指定すると、それらが受け持つ
    ;;   socketをshutdownし、closeする。
    ;; - この手続きは、通常はまず使われる事は無いと思われる。
    ;;   (client-handlerが終了すればsocketは自動的に閉じられる為)
    ;; - opt-drop-contには真偽値を指定する。省略した場合は#fとして扱われる。
    ;; -- opt-drop-contが#fならsocketのshutdownのみが行われる。
    ;;    しかし、socketに関連付けられているclient-handlerはまだ残り、
    ;;    このclient-handlerに次に制御が渡った時には、
    ;;    socketの読み書きをしようとするとEOFやエラーになるので、
    ;;    socketが切断されている事が認識でき、必要ならfinalize処理を
    ;;    実行させるような事が出来る。
    ;; -- opt-drop-contが#tならsocketのshutdownと、client-handlerの
    ;;    途中まで実行され、保持されたままのコルーチンは破棄される。
    ;;    これは、client-handlerに、socketが切断された時のfinalize処理を
    ;;    書いていたとしても実行されない、という事を意味する。
    ;;    どういう事なのか分かっている時にだけ指定する事。
    ;; - 尚、ssc:disconnectを呼ぶ際には、自分自身のcsidを指定する事はできない。
    ;;   この手続きはあくまで、他の任意のクライアントを切断する為のもの。
    ;;   (そもそも、client-handlerのコルーチンが終了すれば、関連付けられていた
    ;;    socketは自動的に切断されるので、そういう呼び出しは不要)

    ;; came from gauche.net
    <sockaddr>
    <sockaddr-in>
    <sockaddr-un>
    sockaddr-family
    sockaddr-name
    sockaddr-addr
    sockaddr-port
    ))
(select-module tir03.socket.server.coroutine)


;;; --------
;;; class


(define-class <socket-server-coroutine> ()
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
   ;; (proc csid client-socket in-port out-port)
   ;; csidは、接続毎に連番で振られる数値。
   ;; in-portは、クライアントからのデータを読み取れるport。
   ;; 読み取り時には、ブロックしてしまわないように注意しなくてはならない。
   ;; out-portは、クライアントにデータを送信できるport。
   ;; このprocが終了/エラーを投げると、socketは自動的に閉じられる。
   ;; ※socketの性質上、どの段階で接続が切断されるかは分からない為、
   ;;   上記のsocketに対してread/writeする際には必ず、#<eof>が返ってきたり、
   ;;   途中までしかreadできなかったり、writeしようとしたらエラーが発生したり、
   ;;   SIGPIPEが流れたり、といった事に備えなくてはならない！
   (client-handler
     :accessor client-handler-of
     :init-keyword :client-handler
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
     :init-form (make <selector>))
   (client-counter ; client毎にuniqueなid数値を発行する為のカウンタ。
     :accessor client-counter-of
     :init-value 0)
   (session-table
     :accessor session-table-of
     :init-form (make-hash-table 'eqv?)) ; keyはcsid(client-counterの数値)。
   (shutdown-request ; コレが#tなら、serverの終了を開始する。
     :accessor shutdown-request-of
     :init-value #f)
   (graceful-shutdown-request ; コレが#tなら、serverのgraceful終了を開始する。
     :accessor graceful-shutdown-request-of
     :init-value #f)
   (selected-csid-queue
     :accessor selected-csid-queue-of
     :init-form (make-queue))
   (selector-timeout-alist ; (csid timeout-epoch)を要素とするlist。
     :accessor selector-timeout-alist-of
     :init-value '())
   (slept-csid-alist ; (csid wakeup-epoch)を要素とするlist。
     :accessor slept-csid-alist-of
     :init-value '())
   ))


(define-method initialize ((self <socket-server-coroutine>) initargs)
  (next-method)
  ;; check slot
  (unless (sockaddr-of self)
    (errorf "~a must be need to :sockaddr" (class-name (class-of self))))
  (unless (client-handler-of self)
    (errorf "~a must be need to :client-handler" (class-name (class-of self))))
  )


;;; --------


(define-condition-type <caught-sigterm> <error> #f)

;; これらのパラメータは基本的に、client-handlerで使う。
(define ssc (make-parameter #f)) ; self
(define in-client-handler? (make-parameter #f)) ; シグナル受信時の動作判定用
(define return/client-handler-cont (make-parameter #f)) ; 継続一時保持用
(define client-session (make-parameter #f)) ; session情報保持用
(define client-csid (make-parameter #f)) ; csid保持用


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


(define ssc:server-opened? server-socket-of)


(define (get-floatepoch)
  (receive (epoch micro-epoch) (sys-gettimeofday)
    (+ epoch
       (* micro-epoch 0.000001))))




;;; --------
;;; for session manipulate
(define (make-session . keywords)
  (let-keywords* keywords ((cont #f) ; 実行途中の継続を保持する
                           (client-socket #f) ; socketそのものを保持する
                           (select-handler #f)
                           ;; remove-clientの時に必要になる。
                           ;; client-handlerとは違う事に注意
                           )
    ;; ToDo: 要素が増えてきたらkeywordsのまま管理するとか、もう少し考え直す
    (list
      cont ; car
      client-socket ; cadr
      select-handler ; caddr
      )))
(define session->cont car)
(define session->client-socket cadr)
(define session->select-handler caddr)





;;;; ToDo: 以下は古いソース





(define (execute-client-handler self csid session)
  (let1 client-socket (session->client-socket session)
    (let (
          (sock-in (socket-input-port client-socket :buffering :modest))
          (sock-out (socket-output-port client-socket :buffering :line))
          (sockaddr (socket-address client-socket))
          )
      (define (client-terminate)
        (remove-client self csid))
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
                    (verbosef self "client-handler error (csid=~s)" csid)
                    (report-error e)
                    (client-terminate)))
          (let1 pc (let/cc return
                     ;; client-handler実行中はフラグを立てる
                     (dynamic-wind
                       (cut in-client-handler? #t)
                       (lambda ()
                         (parameterize ((return/client-handler-cont return)
                                        (client-session session)
                                        (client-csid csid))
                           ((client-handler-of self) csid
                                                     sockaddr
                                                     socket-reader
                                                     socket-writer)))
                       (cut in-client-handler? #f))
                     ;; 最後まで実行されたので、disconnect/finalizeする
                     (verbosef self "client-handler finished (csid=~s)" csid)
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
        (initial-csids (hash-table-keys (session-table-of self)))
        )
    (unless server-socket
      (error "not opened"))
    (unless (null? initial-csids)
      (verbosef self "all client graceful disconnecting...")
      (let retry ((csids initial-csids))
        (unless (null? csids)
          (let* ((csid (car csids))
                 (session (hash-table-get (session-table-of self) csid)))
            (socket-disconnect/ignore-error (session->client-socket session))
            (reset/pc
              ((session->cont session)))
            (retry (hash-table-keys (session-table-of self))))))
      (verbosef self "all client graceful disconnected."))))







;;;; ToDo: 以上は、古いソース



;; 再実行する際に影響が無いように、完全に綺麗にする手続き
(define (ssc:clean-up self)
  ;; note: client-counterだけは初期化せずに再利用する。
  ;; selector
  (selector-delete! (selector-of self) #f #f #f)
  ;; selected-csid-queue
  (dequeue-all! (selected-csid-queue-of self))
  ;; session-table
  (for-each
    (cut hash-table-delete! (session-table-of self) <>)
    (hash-table-keys (session-table-of self)))
  ;; shutdown-request
  (set! (shutdown-request-of self) #f)
  ;; graceful-shutdown-request
  (set! (graceful-shutdown-request-of self) #f)
  ;; server-socket
  (set! (server-socket-of self) #f)
  ;; slept-csid-alist
  (set! (slept-csid-alist-of self) '())
  ;; selector-timeout-alist
  (set! (selector-timeout-alist-of self) '())
  #t)


;;; selectorに登録する為の、client-socket用handlerを返す
(define (make-select-handler self csid)
  (lambda (in flag)
    ;; note: ここ自体では実際の処理は行わず、ただ単に
    ;;       selected-csid-queueにcsidを追加する事だけを行う。
    (verbosef self "received data csid ~s" csid)
    (set! (selected-csid-queue-of self) csid)))

;;; selectorに登録する為の、server-socket用handlerを返す
(define (make-server-accept-handler self server-socket)
  ;; clientがserverに接続すると、selectorからこのproc(↓)が呼ばれる。
  (lambda (fd flag)
    (let ((client-socket (socket-accept server-socket))
          (csid (let1 new-csid (+ 1 (client-counter-of self))
                  (set! (client-counter-of self) new-csid)
                  new-csid))
          )
      (let* ((select-handler (make-select-handler self csid))
             (session (make-session
                        :cont #f
                        :client-socket client-socket
                        :select-handler select-handler)))
        (verbosef self "accepted to ~s (csid=~s)" client-socket csid)
        ;; select-handlerをselectorに登録
        (selector-add! (selector-of self)
                       (socket-input-port client-socket)
                       select-handler
                       '(r))
        ;; session-tableに登録
        (hash-table-put! (session-table-of self) csid session)
        ;; selected-csid-queueに追加
        (enqueue! (selected-csid-queue-of self) csid)))))



(define (ssc:open self)
  (verbosef self "ssc opening...")
  (when (ssc:server-opened? self)
    (error "already opened"))
  (let1 server-socket (make-server-socket
                        (sockaddr-of self)
                        :reuse-addr? #t
                        :sock-init (lambda (socket address)
                                     (socket-setsockopt
                                       socket SOL_SOCKET SO_KEEPALIVE 1)))
    ;; 各種スロットを念の為にclean up
    (ssc:clean-up self)
    ;; server自身のaccept-handlerをselectorに登録
    (selector-add!
      (selector-of self)
      (socket-fd server-socket)
      (make-server-accept-handler self server-socket)
      '(r))
    ;; server-socketをスロットに保存
    (set! (server-socket-of self) server-socket)
    (verbosef self "ssc opened.")))

;;; (slept-csid-alist-of self)と(selector-timeout-alist-of self)を見て、
;;; 適切なselect用microsecondsまたは#fを返す。
;;; (つまり、そのままselector-selectのtimeout値として利用できる)
(define (get-select-microseconds self)
  (let1 epochs (map
                 cadr
                 (append (slept-csid-alist-of self)
                         (selector-timeout-alist-of self)))
    (if (null? epochs)
      #f
      (let ((nearest (apply min epochs))
            (now (get-floatepoch)))
        (clamp
          (- nearest now)
          0)))))

(define (ssc:main self)
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
      ;;;; ToDo: 以下はまだ確認が不十分！
      (let1 selector (selector-of self)
        (let loop ((old-c 0))
          (let* ((select-microseconds (get-select-microseconds self))
                 )
            ...
          (let1 c (selector-select selector interval)
            (let1 csid (selected-csid-of self)
              (when csid
                (let* ((session (hash-table-get (session-table-of self) csid))
                       (old-cont (session->cont session)))
                  (if old-cont
                    (reset/pc
                      (old-cont))
                    (execute-client-handler self csid session))) ; 初回実行
                (set! (selected-csid-of self) #f))) ; selected-csidをクリア
            (if (shutdown-request-of self)
              (let1 server-finalizer (server-finalizer-of self)
                (when server-finalizer
                  (server-finalizer))
                (all-client-graceful-disconnect self)) ; この後、closeされる
              (begin
                (and
                  callee
                  (eqv? old-c c) ; selectがtimeoutしたら#tになる(筈)
                  ;; note: ここでは、敢えて、エラー保護はしない。
                  ;;       (エラー発生=サーバはそのまま終了とする)
                  (callee))
                (loop c)))))))))

(define (ssc:close self)
  (verbosef self "ssc closing...")
  (unless (ssc:server-opened? self)
    (error "not opened"))
  ;; まず、accept-handlerを停止する
  (remove-accept-handler self)
  ;; 残っている全てのclientについて、
  ;; client-socketをdisconnectし、実行途中の継続も破棄する
  (map
    (cut remove-client self <>)
    (hash-table-keys (session-table-of self)))
  ;; clean up
  (ssc:clean-up self)
  (verbosef self "ssc closed."))


;;; この手続きは、server-socketをdisconnectし、
;;; selectorからaccept-handlerを除去する。
;;; しかし、(server-socket-of self)はまだ#fにはしない。
;;; (これはサーバの起動/停止の判定にも使われている為)
(define (remove-accept-handler self)
  (unless (ssc:server-opened? self)
    (error "server-socket not opened"))
  (let1 server-socket (server-socket-of self)
    ;; まず最初に、<sockaddr-un>なら、socket-fileを削除する
    ;; (race condition防止の為に、先に削除する)
    (when (eq? 'listening (socket-status server-socket))
      (let1 sockaddr (sockaddr-of self)
        (when (eq? 'unix (sockaddr-family sockaddr))
          (let1 sockfile (sockaddr-name sockaddr)
            (verbosef self "unlink to ~s" sockfile)
            (ignore-error (sys-unlink sockfile))))))
    ;; accept-handlerをクリア(fdの関係上、disconnectする前にクリアする)
    (selector-delete! (selector-of self) (socket-fd server-socket) #f #f)
    ;; server-socketを切断し、新しい接続を受けないようにする
    (socket-disconnect/ignore-error server-socket)))

;;; この手続きは、csidに結び付けられているsocketをdisconnectし、
;;; csidに結び付けられている継続を破棄する。
(define (remove-client self csid)
  (verbosef self "remove to csid ~s" csid)
  (let1 session (hash-table-get (session-table-of self) csid #f)
    (if (not session)
      (verbosef self "cannot found csid ~s in session-table" csid)
      (begin
        ;; socketを閉じる
        (socket-disconnect/ignore-error (session->client-socket session))
        ;; selectorから除去
        (selector-delete!
          (selector-of self)
          #f
          (session->select-handler session)
          #f)
        ;; session-tableからレコードを消す
        (hash-table-delete! (session-table-of self) csid)
        ;; selected-csid-queueとslept-csid-alistと
        ;; selector-timeout-alistからも消す
        (remove-from-queue! (cut eqv? csid <>) (selected-csid-queue-of self))
        (set!
          (slept-csid-alist-of self)
          (remove (cut eqv? csid <>) (slept-csid-alist-of self)))
        (set!
          (selector-timeout-alist-of self)
          (remove (cut eqv? csid <>) (selector-timeout-alist-of self)))
        (verbosef self "csid ~s removed." csid)))))



;;; --------
;;; export methods

(define-method ssc:server-start ((self <socket-server-coroutine>))
  (parameterize ((ssc self))
    (ssc:open self)
    (begin0
      (ssc:main self)
      (ssc:close self))))

(define-method ssc:server-shutdown ((self <socket-server-coroutine>))
  (if (ssc:server-opened? self)
    (begin
      (verbosef self "server shutdown request accepted.")
      (remove-accept-handler self)
      (set! (shutdown-request-of self) #t))
    (error "server is not started")))
(define-method ssc:server-shutdown ()
  (if (ssc)
    (ssc:shutdown (ssc))
    (error "not in client-handler")))

(define-method ssc:server-shutdown-graceful ((self <socket-server-coroutine>))
  (if (ssc:server-opened? self)
    (begin
      (verbosef self "server graceful shutdown request accepted.")
      ;; (remove-accept-handler self)
      ;; ssc:server-shutdownとは違い、accept-handlerは除去しない
      ;; (graceful終了の直前にremoveする事)
      (set! (graceful-shutdown-request-of self) #t))
    (error "server is not started")))
(define-method ssc:server-shutdown-graceful ()
  (if (ssc)
    (ssc:shutdown-graceful (ssc))
    (error "not in client-handler")))



(define-method ssc:wait-data-from-socket ((timeout-second <number>))
  ;;;; ToDo: あとで
  #f)
(define-method ssc:wait-data-from-socket ()
  (ssc:wait-data-from-socket 0))

(define-method ssc:pause ()
  ;;;; ToDo: あとで
  #f)

(define-method ssc:sleep ((second <number>))
  ;;;; ToDo: あとで
  #f)

(define-method ssc:disconnect ((self <socket-server-coroutine>)
                               (csid <integer>) . opt-drop-cont)
  (let1 session (hash-table-get (session-table-of self) csid #f)
    (if (not session)
      (verbosef self "cannot found csid ~s in session-table" csid)
      (begin
        (socket-disconnect/ignore-error (session->client-socket session))
        (when (get-optional opt-drop-cont #f)
          (if (eqv? csid (selected-csid-of self))
            (verbosef self "cannot clean myself (csid=~s)" csid)
            (remove-client self csid)))))))
(define-method ssc:disconnect ((csid <integer>) . opt-drop-cont)
  (if (ssc)
    (apply ssc:disconnect (ssc) csid opt-keep-cont)
    (error "not in client-handler")))


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


(provide "tir03/socket/server/coroutine")


