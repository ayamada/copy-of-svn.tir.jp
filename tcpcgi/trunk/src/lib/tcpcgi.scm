;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$


;;; ToDo : メイン処理部分はstateモジュール側に移す
;;; ToDo : stateモジュールの名前変更(transaction?)

;;; ToDo : HEADメソッド用フィルタを用意する
;;;        HEADメソッドでは、コンテンツを送り返してはならないが、
;;;        content-lengthは実際の量を返す必要がある。
;;;        永続的接続時にcgiスクリプトがHEADを考慮せずに作られている場合に、
;;;        これが元で永続的接続に異常が発生する可能性が高い。
;;;        - HEADメソッドかどうかを判定する
;;;        - response-bodyからcontent-lengthを測定する
;;;        - ヘッダ部分のみ送信する
;;;        尚、これはparsed-headerではなく、tcpcgiモジュールが行う事。


;;; ToDo : apache風clfでロギングするloggerを提供
;;;        ついでにfull dump loggerも提供(autoloadを使う)

;;; ToDo : 最初に、「ロギングしてから結果を返す」継続を作り、
;;;        各実行フェーズに、その継続を渡し、必要に応じて使ってもらうように
;;;        すれば、綺麗になる
;;;        (現状の返り値による制御は分かりにくい)
;;;        この通りに作り直す事
;;;        しかし、あとで。まず動く状態にしてから。


;;; ToDo : ab -kでの最終段階でのメモリ消費量を確認して、
;;;        メモリリーク的な状態になっていない事を確認する事
;;;        (test caseにでも入れる？？？)


;;; ToDo : tcpcgi:inner:dispatch及びtcpcgi/dispatch.scmが汚いので直したい


;;; ToDo : nphスクリプトの為に、CGI/1.1->HTTP/1.1変換vportを提供
;;;        （chunkedで出力する）

;;; note : 動作中は、(current-error-port)は、cgi-error-portがセットされる
;;;        （stderrに出力された内容はエラーログとして残る）

;;; ToDo : シグナル保護部分を書き直したので、動作確認をする事
;;; ToDo : tcpcgi.supplementの動作確認を行う事。

;;; ToDo : 簡単にエラーテンプレートに一括でcss、ヘッダ、フッタ等を
;;;        設定可能な仕組みを提供する


;;; ToDo : エラー発生時にメールで通知する機能が欲しい




;;; ToDo : 将来は、apacheのように、置いてある*.cgiファイルにアクセスが来たら、
;;;        そのファイルを判別し、それがschemeファイルだったら
;;;        そのまま読み込んで実行するようにする？


;;; ToDo : クライアントへの出力を、なるべく効率良くできるようにしたい
;;;        （具体的には、クライアントへのHTTP/1.1出力時）
;;;        バッファリングモード:noneが最も効率が悪い、気がするが、
;;;        :fullだと、こまめにflushしないとメモリ効率が悪い気がする
;;;        それとも、Nagleアルゴリズムが働いて、効率良く送れてる？
;;;        但し、どちらにせよ、nph, filer動作の時は、:noneにしないと駄目？



;;; ToDo : auth実装

;;; ToDo : rfc2616を見て、クライアントからの接続がHTTP/1.1なら、
;;;        クライアントからのmethodやヘッダがHTTP/1.1を満たしているかを
;;;        チェックし、満たしていないなら、適切なエラーを返すようにする


;;; ToDo : サイズ的なDoS対策
;;;        apacheのRLimit系の設定を真似する


;;; ToDo : fastcgiサーバとして動作できるようにする


(define-module tcpcgi
  (use gauche.charconv)
  (use gauche.parameter)
  (use gauche.selector)
  (autoload gauche.interactive describe)
  (use srfi-1) ; filter-map
  (use srfi-2) ; and-let*
  (use srfi-13) ; string-titlecase
  (use rfc.822)
  (use rfc.uri)
  (use text.tree)
  (use text.html-lite)
  (use util.list)
  (use www.cgi)

  (use tcpcgi.state) ; <tcpcgi.state> copy-all-slot! clone

  ;; get-header-value
  ;; set-temporary-state!
  ;; with-port-buffering
  ;; with-signal-handler
  (use tcpcgi.common)

  (use tcpcgi.request)
  (use tcpcgi.metavariables)
  (use tcpcgi.dispatch)
  (use tcpcgi.execute) ; make-filer-thunk
  (use tcpcgi.parsed-header)
  (use tcpcgi.error-document)
  (use tcpcgi.version) ; tcpcgi-version
  (autoload tcpcgi.supplement
    tcpcgi:display-cgi-metavariables-cgi-thunk
    tcpcgi:display-debug-information-cgi-thunk
    tcpcgi:tcpcgi-process-finish-cgi-thunk
    )
  (export
    tcpcgi
    tcpcgi.state

    <tcpcgi>
    tcpcgi-main

    get-dispatch-vhost
    set-dispatch-vhost!
    get-dispatch-path
    set-dispatch-path!
    get-dispatch-fallback
    set-dispatch-fallback!
    get-error-document
    set-error-document!

    tcpcgi:default-logger
    tcpcgi:debug-logger
    tcpcgi:clf-logger

    tcpcgi:display-cgi-metavariables-cgi-thunk
    tcpcgi:display-debug-information-cgi-thunk
    tcpcgi:tcpcgi-process-finish-cgi-thunk
    ))
(select-module tcpcgi)


;; for parameterize
(define tcpcgi (make-parameter #f))
(define tcpcgi.state (make-parameter #f))



(define-class <tcpcgi> ()
  (
   (dispatch-vhost :accessor dispatch-vhost-of
                   :init-keyword :dispatch-vhost
                   :init-value #f)
   (dispatch-path :accessor dispatch-path-of
                  :init-keyword :dispatch-path
                  :init-value #f)
   (dispatch-fallback :accessor dispatch-fallback-of
                      :init-keyword :dispatch-fallback
                      :init-value #f)

   ;; #fならロギングしない
   (log-port :accessor log-port-of
             :init-keyword :log-port
             :init-form (current-error-port))
   ;; #fならロギングしない
   (logger :accessor logger-of
           :init-keyword :logger
           :init-form tcpcgi:default-logger) ; 引数に<tcpcgi.state>を受け取る

   ;; #fなら/dev/nullに流す
   (cgi-error-port :accessor cgi-error-port-of
                   :init-keyword :cgi-error-port
                   :init-form (current-error-port))

   (server-software :accessor server-software-of
                    :init-keyword :server-software
                    :init-value (format
                                  "~a/~a (Gauche-~a)"
                                  "tcpcgi"
                                  (tcpcgi-version)
                                  (gauche-version)))

   (error-document
     :accessor error-document-of
     :init-keyword :error-document
     :init-value '())

   (use-persistent-connection :accessor use-persistent-connection-of
                              :init-keyword :use-persistent-connection
                              :init-value #t)
   (request-timeout :accessor request-timeout-of
                    :init-keyword :request-timeout
                    :init-value 30)
   (cgi-thunk-timeout :accessor cgi-thunk-timeout-of
                      :init-keyword :cgi-thunk-timeout
                      :init-value 10)
   (keep-alive-timeout :accessor keep-alive-timeout-of
                       :init-keyword :keep-alive-timeout
                       :init-value 5)
   (use-server-header :accessor use-server-header-of
                      :init-keyword :use-server-header
                      :init-value #f)
   (max-requests-per-child :accessor max-requests-per-child-of
                           :init-keyword :max-requests-per-child
                           :init-value 65536) ; #f不可。
   (extra-headers :accessor extra-headers-of
                  :init-keyword :extra-headers
                  :init-value '())

   (convert-incomplete-string-uri
     :accessor convert-incomplete-string-uri-of
     :init-keyword :convert-incomplete-string-uri
     :init-value #t)
   (temporary-file-prefix :accessor temporary-file-prefix-of
                          :init-keyword :temporary-file-prefix
                          :init-value "/tmp/tcpcgi.request.")
   (request-body-on-memory-limit-size
     :accessor request-body-on-memory-limit-size-of
     :init-keyword :request-body-on-memory-limit-size
     :init-value #f)

   ;; 以下は、内部状態保持用スロット
   (dispatch-vhost-instance :init-value #f)
   (dispatch-path-instance :init-value #f)
   (dispatch-fallback-instance :init-value #f)
   (error-document-instance :init-value #f)
   ;; ↑のスロットの意味
   ;; - #f : 未チェック。要生成。
   ;; - #t : チェックしたが、設定無しだった。
   ;; - <tcpcgi.dispatch> : チェックした。生成した。使用可能。


   ;; シグナルキャッチフラグ。コレが#tになったら次の接続は行わない。
   (signaled :init-value #f)
   ))


;(define-method initialize ((self <tcpcgi>) initargs)
;  (next-method)
;  (set!
;    (tcpcgi.parsed-header-of self)
;    (make <tcpcgi.parsed-header>
;      :server (and
;                (use-server-header-of self)
;                (server-software-of self))
;      :extra-headers (extra-headers-of self)
;      ))
;  )


;; accessor-method
;; ToDo : refやset!で普通にアクセスできるようにする
(define-method get-dispatch-vhost ((self <tcpcgi>))
  (dispatch-vhost-of self))
(define-method get-dispatch-path ((self <tcpcgi>))
  (dispatch-path-of self))
(define-method get-dispatch-fallback ((self <tcpcgi>))
  (dispatch-fallback-of self))
(define-method get-error-document ((self <tcpcgi>))
  (error-document-of self))
(define-method set-dispatch-vhost! ((self <tcpcgi>) new-vhost-alist)
  (set! (dispatch-vhost-instance-of self) #f)
  (set! (dispatch-vhost-of self) new-vhost-alist))
(define-method set-dispatch-path! ((self <tcpcgi>) new-path-alist)
  (set! (dispatch-path-instance-of self) #f)
  (set! (dispatch-path-of self) new-path-alist))
(define-method set-dispatch-fallback! ((self <tcpcgi>) new-value)
  (set! (dispatch-fallback-instance-of self) #f)
  (set! (dispatch-fallback-of self) new-value))
(define-method set-error-document! ((self <tcpcgi>) new-alist)
  (set! (error-document-instance-of self) #f)
  (set! (error-document-of self) new-alist))
;; 内部用アクセサメソッド
(define-syntax get-*-instance
  (syntax-rules ()
    ((_ self slot-symbol maker-thunk)
     (let loop ()
       (let1 instance (ref self slot-symbol)
         (case instance
           ('#t #f)
           ('#f (begin
                  (set!
                    (ref self slot-symbol)
                    (or
                      (maker-thunk)
                      #t))
                  (loop)))
           (else instance)))))))
(define-method get-dispatch-vhost-instance ((self <tcpcgi>))
  (get-*-instance
    self
    'dispatch-vhost-instance
    (lambda ()
      (make-dispatch-for-vhost (dispatch-vhost-of self)))))
(define-method get-dispatch-path-instance ((self <tcpcgi>))
  (get-*-instance
    self
    'dispatch-path-instance
    (lambda ()
      (make-dispatch-for-path (dispatch-path-of self)))))
(define-method get-dispatch-fallback-instance ((self <tcpcgi>))
  (get-*-instance
    self
    'dispatch-fallback-instance
    (lambda ()
      (make-dispatch-for-fallback (dispatch-fallback-of self)))))
(define-method get-error-document-instance ((self <tcpcgi>))
  (get-*-instance
    self
    'error-document-instance
    (lambda ()
      (make
        <tcpcgi.error-document>
        :hoge hoge
        ))))



(define (tcpcgi:default-logger state)
  (display "tcpcgi: ")
  (write
    (list
      (sys-time)
      (ref state 'remote-addr)
      (ref state 'remote-port)
      (ref state 'remote-host)
      (ref state 'response-code)
      (ref state 'internal-description)
      (ref state 'request-line)
      (ref state 'request-header)
      ))
  (newline)
  )

(define (tcpcgi:debug-logger state)
  (tcpcgi:default-logger state)
  (describe state) ; for debug
  )

(define (tcpcgi:clf-logger state)
  (let1 request-header (ref state 'request-header)
    (format
      "~a - - [~a] ~s ~a - ~s ~s\n"
      (or
        (ref state 'remote-host)
        (ref state 'remote-addr)
        '-)
      (sys-ctime (sys-time)) ; ToDo : あとで適当に直す事
      (ref state 'request-line)
      (ref state 'response-code)
      (get-header-value "referer" request-header :default '-)
      (get-header-value "user-agent" request-header :default '-)
      )))
;; ****.ne.jp - - [08/Feb/2005:16:40:42 +0900] "GET /banner.png HTTP/1.1" 200 4992 "http://********/index.htm" "Mozilla/4.0 (compatible; MSIE 6.0; Windows 98; .NET CLR 1.1.4322)"



(define (with-timer-handler timeout timeout-thunk thunk)
  ;; ToDo : SIGALRMを使わない方法を考える。無理？
  (let ((old-alarm-time #f)
        (old-signal-handler #f)
        )
    (let/cc return
      (dynamic-wind
        (lambda ()
          (set! old-alarm-time (sys-alarm timeout))
          (set! old-signal-handler (get-signal-handler SIGALRM))
          (set-signal-handler! SIGALRM (lambda (s)
                                         (return (timeout-thunk))))
          )
        thunk
        (lambda ()
          (sys-alarm old-alarm-time)
          (set-signal-handler! SIGALRM old-signal-handler)
          )))))





;;;;;;;;;;;;;;;;;; ここから下は古い










(define (tcpcgi-display-error-document! self response-code)
  (unless (error-document-instance-of self)
    (set!
      (error-document-instance-of self)
      (make <tcpcgi.error-document>
        :ht (apply hash-table 'eqv? (error-document-of self)))))
  (display-error-document (error-document-instance-of self) response-code))


;; response-codeのエラーをHTTP/1.1形式でdisplayする
(define (response-error! self state response-code)
  (define (execute-error-document-thunk thunk)
    (set! (ref state 'dispatched-cgi-target) thunk)
    (and
      thunk
      ;; tcpcgi-mainと同じような順に実行する。
      ;; ココは、tcpcgi-mainから持ってきた。
      (execute-cgi-thunk! self state)
      (parse-cgi/1.1-response self state)
      (make-http-response! self state)
      (send-response! self state)
      ;; 問題が発生したら#fを返すという前提になっている。
      ;; ToDo : この付近は、もう少しスマートに出来る筈
      ))

  (unless (ref state 'connection-close) ; 接続断なら何もしない
    (set! (ref state 'nph) #f) ; 強制的に非nph動作とする、今のところは
    (unless (ref state 'merged-cgi-metavariables)
      ;; エラー状況によっては、merged-cgi-metavariablesが無い場合も
      ;; あるので、生成しておく
      (make-cgi-metavariables! self state))
    (execute-error-document-thunk
      (lambda ()
        (tcpcgi-display-error-document! self response-code)))))



(define (send-response! self state)
  ;; connection-closeが#tなら何も返さない
  (unless (ref state 'connection-close)
    (let ((response-status-line (ref state 'response-status-line))
          (response-header (ref state 'response-header))
          (response-body (ref state 'response-body))
          (response-code (ref state 'response-code))
          )
      ;; response-bodyがあるなら普通に出力、無いならエラーを出力。空文字列ok
      (cond
        ((not response-body) (response-error! self state response-code))
        ((ref state 'http-0.9-flag) (display response-body))
        (else
          (display-http-object
            response-status-line
            response-header
            response-body)))))
  (flush) ; 最後に、念の為、flushしておく
  #t)


;; 以上は、古いコード
;;;;;;;;;;;;;;;;;;;;;





;; 一定時間内にcurrent-input-portが読み込み可能になったかどうか調べる
;; #tなら読み込み可能、#fならタイムアウト
;; timeout-secが#fなら、読み込めるようになるまで待つ
(define (can-read-stdin-until? timeout-sec)
  (let/cc return
    (let1 selector (make <selector>)
      (selector-add!
        selector
        (current-input-port)
        (lambda (input flag)
          (if (eq? flag 'r)
            (return #t)
            (error "cannot read current-input-port")))
        '(r x))
      (selector-select selector (and timeout-sec (* timeout-sec 1000000)))
      #f)))



(define (tcpcgi:inner:receive-request self state)
  (with-error-handler
    (lambda (e)
      ;; おそらく、クライアントから切断された
      (set! (ref state 'error-instance) e) ; 保存
      (set! (ref state 'connection-close) #t) ; 接続終了
      (set!
        (ref state 'internal-description)
        'connection_reset_by_peer)
      #f) ; 異常終了
    (lambda ()
      (and
        ;; check to keep-alive timeout
        (when (and
            (not (= 0 (ref state 'counter))) ; 初回でない事
            (keep-alive-timeout-of self)) ; timeout値が設定されている事
          (or
            (can-read-stdin-until? (keep-alive-timeout-of self)) ; 測定実行
            (begin ; タイムアウトした。
              (set! (ref state 'connection-close) #t) ; 接続終了
              (set! (ref state 'response-code) #f) ; 応答を返さない
              (set! (ref state 'internal-description) 'keep_alive_timeout)
              #f))) ; 異常終了
        ;; 処理続行。
        (and-let* ((keywords
                     (with-timer-handler
                       (request-timeout-of self)
                       (lambda ()
                         (set! (ref state 'connection-close) #t) ; 接続終了
                         (set! (ref state 'response-code) #f) ; 応答を返さない
                         ;; note : ↑rfc2616によると408を返しても良いらしいが、
                         ;;        ↑ブラウザが正しく判断してくれるか微妙
                         (set!
                           (ref state 'internal-description)
                           'request_timeout)
                         #f) ; 異常終了
                       (lambda ()
                         ;; tcpcgi.request内蔵のrequest-body-cachingは使わず、
                         ;; 自前でキャッシュするようにする
                         ;; （request-header取得にはtimeoutを効かせたいが、
                         ;;   request-body取得にはtimeoutを効かせたくない）
                         (receive (keywords error-desc)
                           (stdin->http-request
                             :convert-incomplete-string-uri
                             (convert-incomplete-string-uri-of self)
                             :request-body-caching #f) ; 自前で取り込む
                           (if error-desc
                             (begin
                               (set! (ref state 'connection-close) #t)
                               (set! (ref state 'response-code) 400)
                               (set! (ref state 'request-keywords) keywords)
                               (set!
                                 (ref state 'internal-description)
                                 error-desc)
                               #f) ; 異常終了
                             keywords)))))) ; 正常終了
          ;; request-body-portの取得とstateへの反映
          (set!
            (ref state 'request-keywords)
            (list*
              :request-body-port (stdin->request-body-port
                                   (get-keyword :request-header keywords)
                                   (request-body-on-memory-limit-size-of self)
                                   (temporary-file-prefix-of self))
              keywords))
          #t))))) ; 正常終了





(define (tcpcgi:inner:dispatch self state)
  (let* ((request-keywords (ref state 'request-keywords))
         ;; 必要な値を出しておく
         (parsed-uri-list (get-keyword :parsed-uri-list request-keywords))
         (fallback-host (or
                          (ref state 'server-name)
                          (ref state 'server-addr)))
         (target-host (or
                        (get-header-value
                          "host"
                          (get-keyword :request-header request-keywords))
                        (list-ref parsed-uri-list 2)
                        fallback-host))
         (target-path (list-ref parsed-uri-list 4))
         ;; 最初に、vhostだけ調べておく
         (dispatched-by-vhost (dispatch-from-vhost
                                (get-dispatch-vhost-instance self)
                                target-host
                                target-path))
         ;; vhostはmatchしたがpathはmatchしなかった時に使われる
         (fallback-server-name (if (string? dispatched-by-vhost)
                                 dispatched-by-vhost
                                 fallback-host))
         ;; 本番dispatchを行う
         (dispatched-list (if (list? dispatched-by-vhost)
                            dispatched-by-vhost
                            (or
                              (dispatch-from-path
                                (get-dispatch-path-instance self)
                                target-path)
                              (dispatch-from-fallback
                                (get-dispatch-fallback-instance self))
                              '())))
         ;; dispatched-listは、以下のどれかになる
         ;; - '(executee script-name plain-path-info server-name)
         ;; - '(executee script-name plain-path-info)
         ;; - '(executee)
         ;; - '()
         (dll (length dispatched-list))
         )
    (define (set-state! slot-name list-num fallback)
      (set!
        (ref state slot-name)
        (if (< list-num dll)
          (list-ref dispatched-list list-num)
          fallback)))

    (set-state 'dispatched-executee 0 #f)
    (set-state 'dispatched-script-name 1 "")
    (set-state 'dispatched-plain-path-info 2 target-path)
    (set-state 'dispatched-server-name 3 fallback-server-name)
    (when (null? dispatched-list)
      ;; 404を返して終了。
      (set! (ref state 'response-code) 404)
      (set! (ref state 'internal-description) 'dispatch-failed)
      #f))) ; ディスパッチ成功なら、真値としてundefが返る






(define (tcpcgi:inner:metavariables self state)
  (let* ((plain-path-info (ref state 'dispatched-plain-path-info))
         (path-info (and
                      plain-path-info
                      (uri-decode-string plain-path-info :cgi-decode #t)))
         ;; note : path-infoが存在するなら、path-infoは常に/で始まる。
         (path-translated (and
                            path-info
                            (string-join (sys-getcwd) path-info)))
         (mv (apply
               request->metavariables-alist
               :server-software (server-software-of self)

               :remote-addr (ref state 'remote-addr)
               :remote-port (ref state 'remote-port)
               :remote-host (ref state 'remote-host)
               :server-addr (ref state 'server-addr)
               :server-port (ref state 'server-port)

               :https (ref state 'https)

               :server-name (ref state 'dispatched-server-name)
               :script-name (ref state 'dispatched-script-name)
               :path-info path-info
               :path-translated path-translated

               ;; ToDo : 実装する事
               :auth-type #f
               :remote-user #f

               ;; - request-method
               ;; - request-uri
               ;; - request-protocol
               ;; - parsed-uri-list
               ;; - request-header
               (ref state 'request-keywords)))
         )
    (set!
      (ref state 'cgi-metavariables)
      (let1 old-mv (cgi-metavariables)
        (if (and
              old-mv
              (not (null? old-mv))
          (append-metavariables-alist old-mv mv)
          mv))))
    #t))





(define (tcpcgi:inner:execute self state . opts)
  (define (guard-timeout thunk)
    (with-timer-handler
      (cgi-thunk-timeout-of self)
      (lambda ()
        (set! (ref state 'response-code) 504)
        (set! (ref state 'connection-close) #t)
        (set! (ref state 'internal-description) 'cgi_thunk_timeout)
        #f)
      thunk))
  (define (guard-error thunk)
    (with-error-handler
      (lambda (e)
        (let1 message (ref e 'message)
          (cond
            ((and
               (eq? <system-error> (class-of e))
               ;; "write failed on #<oport (stdout) 0x8083e40>: Broken pipe"
               (#/^write\sfailed\son\s/ message)
               )
             ;; sigpipeだった
             (set! (ref state 'connection-close) #t) ; 接続断
             (set!
               (ref state 'internal-description)
               'connection_reset_by_peer)
             (set! (ref state 'error-instance) e))
            ((#/^unhandled\ssignal\s/ message)
             ;; その他のシグナルだった。多分、終了の合図。
             ;; ToDo : シグナルの種類を確認する
             (set! (ref state 'connection-close) #t) ; 接続断
             (set! (ref state 'internal-description) 'signal_received)
             (set! (ref state 'error-instance) e))
            (else
              ;; それ以外のエラーなら、エラーログに残す
              (set! (ref state 'response-code) 500)
              (set! (ref state 'connection-close) #t) ; 接続断
              (set!
                (ref state 'internal-description)
                'cgi_script_error_exception)
              (set! (ref state 'error-instance) e)
              (report-error e))) ; エラー内容をエラーログに出力する
          #f))
      thunk))

  (let* (
         (second-flag (get-optional opts #f)) ; 遅延実行実施フラグ
         (executee (ref state 'dispatched-executee))
         (lazy-execute-flag (lazy-execute? executee)) ; 遅延実行タイプフラグ
         (always-connection-close (always-connection-close? executee))
         )
    (set! (ref state 'lazy-execute) lazy-execute-flag)
    (when always-connection-close
      (set! (ref state 'connection-close) #t))
    (let1 result (parameterize ((tcpcgi self)
                                (tcpcgi.state state)
                                (cgi-metavariables
                                  (ref state 'cgi-metavariables))
                                )
                   ;; 遅延実行型はタイムアウトもエラー例外も起こらないので、
                   ;; guard無しに実行して良い(メタ変数等は必要)
                   ;; ToDo : guarded-executeが直感的でない(引数が)。直す事
                   (define (guarded-execute e)
                     (guard-timeout
                       (lambda ()
                         (guard-error
                           (lambda ()
                             (with-input-from-port
                               (get-keyword :request-body-port
                                            (ref state 'request-keywords))
                               (lambda ()
                                 (e executee))))))))

                   (if second-execute
                     (if lazy-execute-flag
                       (guarded-execute lazy-execute) ; 遅延実行する
                       #t) ; 何もしない
                     (if lazy-execute-flag
                       (execute executee) ; 遅延実行より先にロギング用情報取得
                       (guarded-execute execute)))) ; 実行して結果を得る
      (cond
        ((boolean? result) #f)
        ((list? result)
         (set! (ref state 'response-code) (car result))
         (set! (ref state 'response-keywords) (cdr result))
         #t)
        ((string? result)
         (set! (ref state 'cgi/1.1-response) result)
         #t)
        (else (error "assertion occured"))))))






(define (tcpcgi:inner:check-persistent-connection self state)
  (define (closing)
    (set! (ref state 'response-connection) "close")
    (set! (ref state 'connection-close) #t)
    #t) ; 常に#tを返す

  (cond
    ;; 何らかの原因で既に接続断が確定している場合はConnection: close
    ((ref state 'connection-close) (closing))
    ;; 設定によって永続的接続を使わない場合もConnection: close
    ((not (use-persistent-connection-of self)) (closing))
    (else
      (or
        (and-let* (
                   (request-keywords (ref state 'request-keywords))
                   (request-header
                     (get-keyword :request-header request-keywords))
                   (line (assoc "connection" request-header))
                   (connection-string (cadr line))
                   )
          (cond
            ;; クライアントがConnection: closeを送ってきた時は、
            ;; Connection: closeをこっちも返す
            ((#/close/ connection-string) (closing))
            ;; クライアントがConnection: Keep-Aliveを送ってきた時は、
            ;; Connection: Keep-Aliveをこっちも返す
            ((string-ci=? "Keep-Alive" connection-string)
             (set! (ref state 'response-connection) "Keep-Alive")
             #t) ; 常に#tを返す
            ;; クライアントがHTTP/1.1の時は、何もしないでそのままとする
            ;; （何もしない＝何も言わずに永続的接続を使う）
            ((string=? "HTTP/1.1"
                       (get-keyword :request-protocol request-keywords))
             #t) ; 常に#tを返す
            ;; そうでないなら、Connection: close
            (else (closing))))
        ;; パラメータ取得に失敗した場合もConnection: close
        (closing)))))







(define (tcpcgi:inner:cgi/1.1->http/1.1 self state)
  (let1 cgi/1.1-response (ref state 'cgi/1.1-response)
    (when cgi/1.1-response
      (if (= 0 (string-size cgi/1.1-response))
        (begin
          (set! (ref state 'response-code) 500)
          (set! (ref state 'connection-close) #t)
          (set!
            (ref state 'internal-description)
            'premature_end_of_script_headers)
          #f)
        (with-error-handler
          (lambda (e)
            (set! (ref state 'response-code) 500)
            (set! (ref state 'connection-close) #t)
            (set! (ref state 'error-instance) e)
            (set!
              (ref state 'internal-description)
              'malformed_header_from_script)
            #f)
          (lambda ()
            (let* ((cgi-response-port (open-input-string cgi/1.1-response))
                   (cgi-response-header (rfc822-header->list
                                          cgi-response-port
                                          :strict? #t))
                   (cgi-response-body-length
                     (string-size
                       (get-remaining-input-string cgi-response-port)))
                   )
              (set! (ref state 'cgi-response-header) cgi-response-header)
              (set! (ref state 'cgi-response-body) cgi-response-body)
              (receive (http/1.1-response-code
                        http/1.1-status-line
                        http/1.1-header
                        http/1.1-body-port)
                ;; ToDo : HEADメソッドかどうかで挙動を変更する
                ;;        尚、methodによってはconnectionを強制的に
                ;;        closeにする必要のあるものがあるかも知れない
                (cgi/1.1->http/1.1
                  cgi-response-header
                  cgi-response-port
                  :connection (ref state 'response-connection)
                  :server (and
                            (use-server-header-of self)
                            (server-software-of self))
                  :content-length cgi-response-body-length
                  :server-name (ref state 'server-name)
                  :server-port (ref state 'server-port)
                  :https (ref state 'https)
                  :extra-header (or (extra-headers-of self) '()))
                (set! (ref state 'response-code) http/1.1-response-code)
                (set! (ref state 'http/1.1-status-line) http/1.1-status-line)
                (set! (ref state 'http/1.1-header) http/1.1-header)
                (set! (ref state 'http/1.1-body-port) http/1.1-body-port)
                #t))))))))





(define (tcpcgi:inner:logging self state)
  (and
    (log-port-of self)
    (logger-of self)
    (with-output-to-port
      (log-port-of self)
      (lambda ()
        ((logger-of self) state)))))





;;;; ここまで確認完了

(define (tcpcgi:inner:response-code->http/1.1 self state)
  (let ((response-code (ref state 'response-code))
        (response-keywords (or (ref state 'response-keywords) '()))
        )
    ;; レスポンスコードに応じたレスポンスを生成して返す
    ;;;;
    #f))




(define (tcpcgi:inner:lazy-execute self state)
  (tcpcgi:inner:execute self state #t))



(define (tcpcgi:inner:send-http/1.1 self state)
  ;; 何も考えずに出力するのみ
  (display-http/1.1-header
    (ref state 'http/1.1-status-line)
    (ref state 'http/1.1-header))
  (copy-port
    (ref state 'http/1.1-body-port)
    (current-output-port))
  (flush)
  #t)






(define-method tcpcgi-main ((self <tcpcgi>)
                            server-addr
                            server-port
                            server-name
                            remote-addr
                            remote-port
                            remote-host ; or #f
                            https) ; boolean
  ;; log-portとcgi-error-portのmodeを一時的に:lineにする
  (define (log-port-with-buffering thunk)
    (with-port-buffering
      (log-port-of self)
      :line
      (lambda ()
        (with-port-buffering
          (cgi-error-port-of self)
          :line
          thunk))))

  ;; 一時的にシグナルから保護
  (define (protection-from-signal thunk)
    ;; SIGPIPE, SIGINT, SIGTERM, SIGHUPから保護する
    (define (signal-handler s)
      (set! (ref self 'signaled) #t))
    ;; ToDo : race condition対策
    (let loop ((left (list SIGPIPE SIGINT SIGTERM SIGHUP)))
      (with-signal-handler
        (car left)
        signal-handler
        (if (null? (cdr left))
          thunk
          (lambda ()
            (loop (cdr left)))))))

  ;; HTTPトランザクション一回分を処理
  (define (execute-one-transaction! state)
    ;; sigpipeのシグナル自体は無効化してあるが、sigpipeの起こったportに
    ;; writeしようとするとエラーが起こるので、それを防ぐ必要がある
    (define (block-sigpipe-error thunk)
      (with-error-handler
        (lambda (e)
          ;; 何にしても、致命的なので、この時点で終了の準備をする
          (set! (ref state 'error-instance) e) ; 記録する
          (set! (ref state 'connection-close) #t) ; 接続断
          (let1 message (ref e 'message)
            (cond
              ((and
                 (eq? <system-error> (class-of e))
                 ;; "write failed on #<oport (stdout) 0x8083e40>: Broken pipe"
                 (#/^write\sfailed\son\s/ message)
                 )
               ;; sigpipeだった
               (set! (ref state 'internal-description) 'client_disconnected))
              ((#/^unhandled\ssignal\s/ message)
               ;; その他のシグナルだった。多分、終了の合図。
               (set! (ref state 'internal-description) 'signal_received))
              (else
                ;; それ以外のエラーなら、エラーログに残す
                (report-error e)))
            #f))
        thunk))

    (block-sigpipe-error
      (lambda ()
        ;; 途中で失敗したら、後の処理はパスする
        (and
          ;; クライアントのhttpリクエストから情報を得る
          (tcpcgi:inner:receive-request self state)
          ;; リクエストのhostとpathを使い、ディスパッチを行う
          (tcpcgi:inner:dispatch self state)
          ;; リクエストからメタ変数を構築
          (tcpcgi:inner:metavariables self state)
          ;; 実行し、CGI/1.1形式の結果を受け取る、または、
          ;; response-codeと、それに付随する情報を受け取る
          (tcpcgi:inner:execute self state)
          ;; 永続的接続が有効かどうかを判定
          (tcpcgi:inner:check-persistent-connection self state)
          ;; CGI/1.1形式の結果からHTTP/1.1形式の出力を生成する
          (tcpcgi:inner:cgi/1.1->http/1.1 self state)
          )))
    ;; ロギングする
    (tcpcgi:inner:logging self state)
    ;; HTTP/1.1形式の結果/エラーを返す。または遅延実行する
    (block-sigpipe-error
      (lambda ()
        (cond
          ;; response-codeがあるなら、それに応じた応答を生成して返す
          ;; response-keywordsも使う
          ;; ToDo : 200が入っている時に、これだとまずい。
          ;; 他のスロットを見て判定するようにする事
          ((ref state 'response-code)
           (tcpcgi:inner:response-code->http/1.1 self state))
          ;; 遅延実行タイプなら、遅延実行を行う
          ((ref state 'lazy-execute) (tcpcgi:inner:lazy-execute self state))
          ;; cgi/1.1-responseがあるなら、レスポンスを返す
          ;; ToDo : 他のスロットで判断すべき気がする
          ((ref state 'cgi/1.1-response)
           (tcpcgi:inner:send-http/1.1 self state))
          (else #f)))))

  (define (tcpcgi-execute! state counter)
    ;; とりあえずリクエスト一回を処理
    (execute-one-transaction! state)
    ;; 永続的接続が有効な場合は、再度実行する
    (when (and
            (not (ref state 'connection-close))
            (ref state 'use-persistent-connection)
            (not (ref self 'signaled))
            )
      (let1 next-counter (+ counter 1)
        (if (< next-counter (max-requests-per-child-of self))
          (tcpcgi-execute!
            (make <tcpcgi.state>
              :counter next-counter
              :server-addr server-addr
              :server-port (x->string server-port)
              :server-name server-name
              :remote-addr remote-addr
              :remote-port remote-port
              :remote-host remote-host
              :https https))
          #t)))) ; 最大回数実行した。必要なら、ロギングしても良い


  ;; 接続してきたクライアントの処理を開始
  (log-port-with-buffering
    (lambda ()
      (protection-from-signal
        (lambda ()
          (with-error-to-port
            (or
              (cgi-error-port-of self)
              (open-output-file "/dev/null" :buffering :none))
            (lambda ()
              (tcpcgi-execute!
                (make <tcpcgi.state>
                  :counter 0
                  :server-addr server-addr
                  :server-port (x->string server-port)
                  :server-name server-name
                  :remote-addr remote-addr
                  :remote-port remote-port
                  :remote-host remote-host
                  :https https)
                0))))))))


(provide "tcpcgi")

