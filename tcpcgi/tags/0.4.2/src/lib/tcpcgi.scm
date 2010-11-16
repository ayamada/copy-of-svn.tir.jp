;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme sw=2 ts=2 et:
;;; $Id$


;;; ToDo : クライアントへの出力を、なるべく効率良くできるようにしたい
;;;        バッファリングモード:noneが最も効率が悪い、気がするが、
;;;        :fullだと、メモリ効率が悪い気がする
;;;        Nagleアルゴリズムが働いて、効率良く送れてる？
;;;        但し、どちらにせよ、nph, filer動作の時は、:noneにしないと駄目

;;; ToDo : nph時、またはfiler時には、すぐに実行せずに、
;;;        ロギングフェーズを終え、クライアントへ結果を返す時点で
;;;        遅延実行するようにする



;;; ToDo : keepalive-timeoutは、SIGALRMではなく、selectで実装する？


;;; ToDo : apacheのServerAliasは、設計的に間違っている。
;;;        同じ内容のコンテンツを返すのでは無く、正しいサーバ名へと302すべき。
;;;        なので、vhost指定時にalistでは無く文字列で別サーバ名を指定して、
;;;        pathをそのままリダイレクトできるような機能を追加する事。
;;;        ついでなので、vhost指定先がalistでは無く単一の場合は、
;;;        noneと同じような扱いとしたいが、どうか。
;;;        やっぱり、↓のpathと同様に:locationキーワードを指定するようにする？
;;; ToDo : 上記と同じように、pathでもリダイレクト設定を簡単に行えるようにする？
;;;        既に文字列はfiler指定として使っているので、
;;;        別の指定方法を考える必要があるが。
;;;        :locationキーワードを使う？その場合の第一パラメータは？
;;;        ついでなので、path指定先がalistでは無く単一の場合は、
;;;        noneと同じような扱いとしたいが、どうか。
;;;        （noneを上書きする？）



;;; ToDo : ディスパッチ回りの変数名のリファクタリングが必要

;;; ToDo : リクエストヘッダ取得/解析部分、メタ変数生成部分を別モジュールに分割

;;; ToDo : check-authentication!実装

;;; ToDo : rfc2616を見て、クライアントからの接続がHTTP/1.1なら、
;;;        クライアントからのmethodやヘッダがHTTP/1.1を満たしているかを
;;;        チェックし、満たしていないなら、適切なエラーを返すようにする

;;; ToDo : パフォーマンス向上の為に、クライアントからの入力をバッファリング
;;;        しないオプションをつける？
;;;        - つけない。persistent-connectionできなくなるから。
;;;        - 代わりに、Content-Lengthが充分に巨大なら、メモリではなく、
;;;          テンポラリファイルにrequest-bodyを書き出すようにする。
;;;        - または、virtual-portかbuffered-portを使って実現する。

;;; ToDo : stateのスロットの初期値が#f前提になっているが、
;;;        初期値不定でも動作するようにした方が良い気がするので、検討する事

;;; ToDo : サイズ的なDoS対策
;;;        apacheのRLimit系の設定を真似する


(define-module tcpcgi
  (use gauche.parameter)
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
  (use tcpcgi.dispatch)
  (use tcpcgi.filer) ; make-filer-thunk
  (use tcpcgi.errordoc) ; default-errordoc-thunk-table fallback-errordoc-thunk
  (use tcpcgi.version) ; tcpcgi-version
  (export
    <tcpcgi>
    tcpcgi-main
    get-vhost-dispatch
    set-vhost-dispatch!
    get-path-dispatch
    set-path-dispatch!
    get-none-dispatch
    set-none-dispatch!
    get-errordoc-alist
    set-errordoc-alist!
    ))
(select-module tcpcgi)




(define-class <tcpcgi> ()
  (
   (vhost-dispatch :accessor vhost-dispatch-of
                   :init-keyword :vhost-dispatch
                   :init-value #f)
   (path-dispatch :accessor path-dispatch-of
                  :init-keyword :path-dispatch
                  :init-value #f)
   (none-dispatch :accessor none-dispatch-of
                  :init-keyword :none-dispatch
                  :init-value #f)
   (vhost-dispatch-instance
     :accessor vhost-dispatch-instance-of
     :init-value #f)
   (path-dispatch-instance
     :accessor path-dispatch-instance-of
     :init-value #f)
   (none-dispatch-instance
     :accessor none-dispatch-instance-of
     :init-value #f)

   ;; #fならロギングしない
   (log-port :accessor log-port-of
             :init-keyword :log-port
             :init-form (current-error-port))
   ;; #fならロギングしない
   (logger :accessor logger-of
           :init-keyword :logger
           :init-form default-logger) ; 引数に<tcpcgi.state>を受け取るロガー

   ;; #fなら/dev/nullに流す
   ;; このログファイルは:buffering :noneで開いておく事
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

   (errordoc-alist
     :accessor errordoc-alist-of
     :init-keyword :errordoc-alist
     :init-value '())
   (errordoc-instance
     :accessor errordoc-instance-of
     :init-form #f)

   (use-persistent-connection :accessor use-persistent-connection-of
                              :init-keyword :use-persistent-connection
                              :init-value #t)
   (request-timeout :accessor request-timeout-of
                    :init-keyword :request-timeout
                    :init-value 30)
   (response-timeout :accessor response-timeout-of
                     :init-keyword :response-timeout
                     :init-value 60)
   (keepalive-timeout :accessor keepalive-timeout-of
                      :init-keyword :keepalive-timeout
                      :init-value 5)
   (use-server-header :accessor use-server-header-of
                      :init-keyword :use-server-header
                      :init-value #f)
   (max-requests-per-child :accessor max-requests-per-child-of
                           :init-keyword :max-requests-per-child
                           :init-value 65536)

   ;; シグナルキャッチフラグ。コレが#tになったら次の接続は行わない。
   (signaled :init-value #f)
   ))


;; accessor-method
;; ToDo : refやset!で普通にアクセスできるようにする
(define-method get-vhost-dispatch ((self <tcpcgi>))
  (vhost-dispatch-of self))
(define-method get-path-dispatch ((self <tcpcgi>))
  (path-dispatch-of self))
(define-method get-none-dispatch ((self <tcpcgi>))
  (none-dispatch-of self))
(define-method get-errordoc-alist ((self <tcpcgi>))
  (errordoc-alist-of self))
(define-method set-vhost-dispatch! ((self <tcpcgi>) new-vhost-alist)
  (set! (vhost-dispatch-instance-of self) #f)
  (set! (vhost-dispatch-of self) new-vhost-alist))
(define-method set-path-dispatch! ((self <tcpcgi>) new-path-alist)
  (set! (path-dispatch-instance-of self) #f)
  (set! (path-dispatch-of self) new-path-alist))
(define-method set-none-dispatch! ((self <tcpcgi>) new-value)
  (set! (none-dispatch-instance-of self) #f)
  (set! (none-dispatch-of self) new-value))
(define-method set-errordoc-alist! ((self <tcpcgi>) new-alist)
  (set! (errordoc-instance-of self) #f)
  (set! (errordoc-alist-of self) new-alist))



(define (default-logger state)
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
  ;(use gauche.interactive) (describe state) ; for debug
  )

(define (tcpcgi-logging self state)
  (and
    (log-port-of self)
    (logger-of self)
    (with-output-to-port
      (log-port-of self)
      (lambda ()
        ((logger-of self) state)))))


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



;; ここから下にある、沢山の!付き関数は、
;; 何か悪い事が起こったら、stateにエラー内容等をセットしてから#fを返す。
;; 正常な場合は、関数名の値をセットしたり等してから#tを返す。


(define (read-request-line! state)
  (define (read-request-line)
    (let1 l (read-line (current-input-port) #t)
      (if (equal? l "")
        (read-request-line)
        l)))

  (let1 request-line (read-request-line)
    (if (eof-object? request-line)
      (begin
        (set! (ref state 'connection-close) #t)
        (set! (ref state 'response-code) 400)
        (set! (ref state 'internal-description) 'empty_request)
        #f)
      (begin
        (set! (ref state 'request-line) request-line)
        #t))))




(define (parse-request-line! state)
  (define (error-return description)
    (set! (ref state 'response-code) 400)
    (set! (ref state 'internal-description) description)
    #f)

  (let* ((mup (string-split
                (ref state 'request-line)
                #\space)) ; method-uri-protocol
         (mup-num (length mup))
         (request-method (car mup)) ; ココまで来れた以上、コレは必ず存在する
         (request-uri (if (= 1 mup-num)
                        "/" ; HTTP/0.9だとこの挙動
                        (cadr mup)))
         (request-protocol (if (< mup-num 3)
                             "HTTP/0.9"
                             (caddr mup)))
         (http-0.9-flag (string=? "HTTP/0.9" request-protocol))
         (parsed-uri-list (call-with-values
                            (lambda ()
                              (uri-parse request-uri))
                            list))
         ;; (list-ref parsed-uri-list 0) ; uri-scheme
         ;; (list-ref parsed-uri-list 1) ; uri-userinfo
         ;; (list-ref parsed-uri-list 2) ; uri-hostname
         ;; (list-ref parsed-uri-list 3) ; uri-port
         ;; (list-ref parsed-uri-list 4) ; uri-path
         ;; (list-ref parsed-uri-list 5) ; uri-query
         ;; (list-ref parsed-uri-list 6) ; uri-fragment
         )
    (set! (ref state 'request-method) request-method)
    (set! (ref state 'request-uri) request-uri)
    (set! (ref state 'request-protocol) request-protocol)
    (set! (ref state 'http-0.9-flag) http-0.9-flag)
    (set! (ref state 'parsed-uri-list) parsed-uri-list)
    ;; ここまで取得した内容を検証し、#tか#fを返す
    (cond
      ((not (<= mup-num 3)) ; パラメータは三つまで
       (error-return 'too_many_request_line_parameter))
      ((not (#/^http\/\d\.\d/i request-protocol)) ; protocolはHTTP/n.m書式
       (error-return 'bad_protocol))
      ((not (#/^\// (list-ref parsed-uri-list 4))) ; pathは/はじまり
       (error-return 'bad_path))
      ((let1 uri-scheme (list-ref parsed-uri-list 0)
         (and
           uri-scheme ; uri-schemeが有り、
           (not (#/^https?$/ uri-scheme)))) ; httpでもhttpsでもない
       (error-return 'bad_uri_scheme))
      (else #t))))




(define (read-request-header! state)
  (set!
    (ref state 'request-header)
    (if (ref state 'http-0.9-flag)
      '()
      (rfc822-header->list (current-input-port)))))




(define (prepare-request-content! state)
  (let* ((request-header (ref state 'request-header))
         (content-type (get-header-value "content-type" request-header))
         (content-length (get-header-value "content-length" request-header))
         )
    (when content-type
      (set! (ref state 'content-type) content-type))
    (when content-length
      (set! (ref state 'content-length) content-length))
    (set!
      (ref state 'request-body)
      (if content-length
        (read-block (string->number content-length))
        "")) ; もしcgiスクリプトが誤って読み出そうとした時の防止用
    #t))




(define (tcpcgi-request-consume! self state)
  ;; クライアントからreadする部分。タイムアウト判定を行う必要がある。
  ;; また、keep-alive判定の為に、(ref state 'first-transaction)によって、
  ;; 挙動を変更する必要がある。
  (and
    ;; first-transactionでないなら、keepaliveでタイムアウトするかをチェック
    (if (ref state 'first-transaction)
      #t
      (with-timer-handler
        (keepalive-timeout-of self)
        (lambda ()
          (set! (ref state 'connection-close) #t) ; 接続終了
          (set! (ref state 'internal-description) 'keepalive_timeout)
          #f)
        (lambda ()
          (with-error-handler
            (lambda (e)
              ;; クライアントからshutdownされたので終了
              (set! (ref state 'connection-close) #t) ; 接続終了
              (set!
                (ref state 'internal-description)
                'connection_reset_by_peer)
              #f)
            (lambda ()
              (peek-byte)))))) ; #t
    (with-timer-handler
      (request-timeout-of self)
      (lambda ()
        (set! (ref state 'connection-close) #t) ; 接続終了
        (set! (ref state 'response-code) 408)
        (set! (ref state 'internal-description) 'request_timeout)
        #f)
      (lambda ()
        ;; 途中で失敗したら、後の処理はパスする
        (and
          ;; request-lineを取得
          (read-request-line! state)

          ;; request-lineからmethod, path, protocol, 0.9flagを取得しチェック
          (parse-request-line! state)

          ;; request-headerを取得
          (read-request-header! state)

          ;; クライアントからPOST値を予め取得
          (prepare-request-content! state)
          )))))





(define (dispatch-none! state dispatcher)
  (and
    (not (boolean? dispatcher))
    (let* ((cgi-list (dump-dispatch.none dispatcher))
           (cgi-target (car cgi-list))
           (keywords (cdr cgi-list))
           (nph (get-keyword :nph keywords #f))
           )
      (set! (ref state 'dispatched-cgi-target) cgi-target)
      (set! (ref state 'nph) nph)
      #t)))

(define (dispatch-path! state dispatcher)
  (and
    (not (boolean? dispatcher)) ; #t, #fは却下（#fを返す）
    (and-let* (
               (uri-list (ref state 'parsed-uri-list))
               (path (list-ref uri-list 4))
               )
      (receive (none-dispatch script-name plain-path-info)
        (dispatch.path->dispatch.none dispatcher path)
        (and
          none-dispatch
          (begin
            (set! (ref state 'dispatched-script-name) script-name)
            (set!
              (ref state 'dispatched-path-info)
              (and
                plain-path-info ; #fなら#fセット
                (not (string=? "" plain-path-info)) ; ""の時は#fをセット
                plain-path-info)) ; ""でないなら、plain-path-infoをセット
            ;; 最後に、dispatch-none情報をセットして終了
            (dispatch-none! state none-dispatch)))))))

(define (dispatch-vhost! state dispatcher)
  (and
    (not (boolean? dispatcher)) ; #t, #fは却下（#fを返す）
    (and-let* (
               (vhost (or
                        ;; Hostヘッダ
                        (and-let* ((host (get-header-value
                                           "host"
                                           (ref state 'request-header)))
                                   ;; port部分が付いているなら除去する
                                   (m (#/^([\w\-\.]+)(?:\:\d+)?$/ host))
                                   )
                          (m 1))
                        ;; または、request-lineのuri-hostname
                        (list-ref (ref state 'parsed-uri-list) 2)
                        ;; または、SERVER_NAME
                        (ref state 'server-name)
                        ))
               )
      (receive (path-dispatch server-name)
        (dispatch.vhost->dispatch.path dispatcher vhost)
        (and
          path-dispatch
          (begin
            (set! (ref state 'dispatched-server-name) server-name)
            ;; 最後に、dispatch-path情報をセットして終了
            (dispatch-path! state path-dispatch)))))))

;; 便宜上、共通化しただけ
(define-syntax set-dispatch-instance!
  (syntax-rules ()
    ((_ self *-dispatch-instance-of *-dispatch-of make-dispatch-*)
     (unless (*-dispatch-instance-of self)
       (set!
         (*-dispatch-instance-of self)
         (let1 src (*-dispatch-of self)
           (or
             (not src) ; srcが#fの時は、チェック済の証として#tをセット
             (make-dispatch-* src) ; インスタンス化。しかし、返り値が#fなら、
             #t))))))) ; チェック済の証として#tをセット

(define (update-none-dispatch-instance! self)
  (set-dispatch-instance!
    self
    none-dispatch-instance-of
    none-dispatch-of
    make-dispatch-none))
(define (update-path-dispatch-instance! self)
  (set-dispatch-instance!
    self
    path-dispatch-instance-of
    path-dispatch-of
    make-dispatch-path))
(define (update-vhost-dispatch-instance! self)
  (set-dispatch-instance!
    self
    vhost-dispatch-instance-of
    vhost-dispatch-of
    make-dispatch-vhost))

(define (tcpcgi-dispatch-none! self state)
  ;; まず先に、内部スロットのインスタンスを更新
  (update-none-dispatch-instance! self)
  ;; stateに値をセットする
  (dispatch-none! state (none-dispatch-instance-of self)))

(define (tcpcgi-dispatch-path! self state)
  ;; まず先に、内部スロットのインスタンスを更新
  (update-path-dispatch-instance! self)
  ;; ディスパッチ処理を実行し、stateに値をセットする
  (dispatch-path! state (path-dispatch-instance-of self)))

(define (tcpcgi-dispatch-vhost! self state)
  ;; まず先に、内部スロットのインスタンスを更新
  (update-vhost-dispatch-instance! self)
  ;; ディスパッチ処理を実行し、stateに値をセットする
  (dispatch-vhost! state (vhost-dispatch-instance-of self)))


;; ToDo : この辺の名前が分かりにくいので、あとでリファクタリングする事
;; - tcpcgi-dispatch-vhost!は、準備を行ってから、dispatch-vhost!を実行する
;; -- dispatch-vhost!は、vhost関連の情報をstateにセットし、
;;    更にvhostからpathを割り出し、dispatch-path!を実行する
;; - tcpcgi-dispatch-path!は、準備を行ってから、dispatch-path!を実行する
;; -- dispatch-path!は、path関連の情報をstateにセットし、
;;    更にpathからnoneを割り出し、dispatch-none!を実行する
;; - tcpcgi-dispatch-none!は、準備を行ってから、dispatch-none!を実行する
;; -- dispatch-none!は、cgi-thunk関連の情報をstateにセットする。
(define (set-dispatched-info! self state)
  (define (return404)
    (set! (ref state 'response-code) 404)
    (set! (ref state 'internal-description) 'cannot_found_cgi_thunk)
    #f) ; この後、処理は続けないので#fを返す

  (and
    (or ; 以下のうち、どれか一つ実行完了すれば良い
      (tcpcgi-dispatch-vhost! self state)
      (tcpcgi-dispatch-path! self state)
      (tcpcgi-dispatch-none! self state)
      (return404)) ; 最後までcgi-thunkが見付からなかったので、404を返して終了
    ;; dispatchは実行完了したが、明示的に404が指定されている場合がある
    (unless (ref state 'dispatched-cgi-target)
      (return404))))



(define (check&set-persistent-connection! self state)
  (and
    (use-persistent-connection-of self) ; 使用するオプションになっている事
    (not (ref state 'connection-close)) ; 接続断でない事
    (not (ref state 'nph)) ; nphでない事
    (not
      (and-let* ((request-header (ref state 'request-header))
                 (line (assoc "connection" request-header))
                 (connection-string (cadr line))
                 )
        ;; リクエストヘッダのconnectionにcloseが含まれていない事
        (#/close/ connection-string)))
    (cond
      ((string-ci=?
         "Keep-Alive"
         (or
           (get-header-value "connection" (ref state 'request-header))
           "")) ; Keep-Alive有効なら、
       (set! (ref state 'use-keep-alive) #t) ; Keep-Alive使用
       (set! (ref state 'use-persistent-connection) #t)) ; 継続接続使用
      ((string=? "HTTP/1.1" (or (ref state 'request-protocol) "")) ; HTTP/1.1
       (set! (ref state 'use-persistent-connection) #t)))) ; 継続接続使用
  #t) ; 常に#tを返す事



(define (check-authentication! self state)
  ;; ToDo : まだ未実装
  #t)


(define *exclude-http-header-regexp*
  #/^(?:(?:Content\-(?:Type|Length))|(?:(?:Proxy\-)?Authorization))$/i)


(define (header-name->env-name header-name)
  (with-string-io
    header-name
    (lambda ()
      (let loop ((r (read-byte)))
        (if (eof-object? r)
          #t
          (begin
            (write-byte
              (cond
                ((<= #x61 r #x7a) (logand #xdf r)) ; a-z -> A-Z
                ((eq? #x2d r) #x5f) ; "-" -> "_"
                (else r)))
            (loop (read-byte))))))))



;; 同じキーの重複のあるrfc822のalistからキーの重複をなくして、
;; 一つのキーに複数の値を持つalistを生成して返す
(define (rfc822-header-merge request-header)
  (let loop ((src-list request-header)
             (result-alist '()))
    (if (null? src-list)
      result-alist
      (let* ((target-line (car src-list))
             (next-list (cdr src-list))
             (key (car target-line))
             (value-list (cdr target-line))
             )
        (loop
          next-list
          (if (assoc key result-alist)
            result-alist ; pass
            (acons
              key
              (apply
                append
                value-list
                (filter-map
                  (lambda (line)
                    (and
                      (string=? key (car line))
                      (cdr line)))
                  next-list))
              result-alist)))))))



(define (get-merged-metavariables-alist under-mv over-mv)
  (cond
    ((or
       (not under-mv)
       (null? under-mv))
     (or over-mv '()))
    ((or
       (not over-mv)
       (null? over-mv))
     (or under-mv '()))
    (else
      (let loop ((src (append
                        (rfc822-header-merge over-mv) ; overで
                        (rfc822-header-merge under-mv) ; underを上書き
                        ))
                 (result '()))
        (if (null? src)
          result
          (loop
            (cdr src)
            (let1 line (car src)
              (if (assoc (car line) result)
                result ; 同名キーが既に存在するなら追加しない
                (cons
                  line
                  result)))))))))



(define (make-cgi-metavariables! self state)
  (define (list-or-empty key value)
    (if value
      `((,key ,value))
      '()))

  (define (make-mv)
    (let ((server-addr (ref state 'server-addr))
          (parsed-uri-list (or
                             (ref state 'parsed-uri-list)
                             (make-list 6 #f)))
          (script-name (ref state 'dispatched-script-name))
          )
      ;; alistを返す
      `(
        ("GATEWAY_INTERFACE" "CGI/1.1")
        ,@(list-or-empty "AUTH_TYPE" (ref state 'auth-type)) ; apache互換の挙動
        ("SERVER_SOFTWARE" ,(server-software-of self))
        ;("DOCUMENT_ROOT" ,document-root) ; apache拡張。ココでは無し
        ;("SERVER_ADMIN" ,server-admin) ; apache拡張。ココでは無し

        ("SERVER_ADDR" ,server-addr)
        ("SERVER_PORT" ,(ref state 'server-port))
        ("SERVER_NAME" ,(or
                          (ref state 'dispatched-server-name)
                          (ref state 'server-name)
                          server-addr)) ; fallback

        ("REMOTE_ADDR" ,(ref state 'remote-addr))
        ("REMOTE_PORT" ,(ref state 'remote-port))
        ;("REMOTE_HOST" ,(or
        ;                  (ref state 'remote-host)
        ;                  ""))
        ;; ↑はCGI/1.1の仕様だが、apacheは↓の挙動
        ,@(list-or-empty "REMOTE_HOST" (ref state 'remote-host))
        ,@(list-or-empty "REMOTE_USER" (ref state 'remote-user))

        ("REQUEST_METHOD" ,(ref state 'request-method))
        ("REQUEST_URI" ,(ref state 'request-uri))
        ("SERVER_PROTOCOL" ,(ref state 'request-protocol))
        ,@(list-or-empty
            "PATH_INFO"
            (and-let* ((plain-path-info
                         (or
                           (ref state 'dispatched-path-info)
                           (and
                             (not script-name) ; まだdispatchしていないなら、
                             (list-ref parsed-uri-list 4))))) ; uri-pathを使用
              (uri-decode-string plain-path-info :cgi-decode #t)))
        ;("PATH_TRANSLATED" ,path-translated) ; ココでは無し
        ("QUERY_STRING" ,(or
                           (list-ref parsed-uri-list 5)
                           ""))
        ("SCRIPT_NAME" ,(or
                          script-name
                          ""))
        ;("SCRIPT_FILENAME" ,(with-module
        ;                      user
        ;                      *program-name*)) ; apache拡張。無し。

        ;; HTTPS
        ,@(list-or-empty "HTTPS" (ref state 'https))

        ;; CONTENT_TYPE, CONTENT_LENGTH
        ,@(list-or-empty "CONTENT_TYPE" (ref state 'content-type))
        ,@(list-or-empty "CONTENT_LENGTH" (ref state 'content-length))

        ;; クライアントのHTTPヘッダから生成するメタ変数。HTTP_*
        ;; 但し、Content-Type, Content-Length,
        ;; Authorization, Proxy-AuthorizationはHTTP_*化しない
        ,@(filter-map
            (lambda (line)
              (if (*exclude-http-header-regexp* (car line))
                #f
                (list
                  (string-append "HTTP_" (header-name->env-name
                                           (car line)))
                  (string-join ; 同じキーのヘッダが複数あるなら連結
                    (cdr line)
                    ", "))))
            ;; rfc822ヘッダは、同じ名前のヘッダが有り得るので、
            ;; それらは予めマージしておく
            (rfc822-header-merge (or
                                   (ref state 'request-header)
                                   '()))))))

  (let1 mv (make-mv)
    (set! (ref state 'plain-cgi-metavariables) mv)
    (set!
      (ref state 'merged-cgi-metavariables)
      (get-merged-metavariables-alist
        mv ; under-mvに
        (cgi-metavariables))) ; over-mvを上書き
    #t))



(define (execute-cgi-thunk! self state)
  ;; ToDo : filer動作の時はタイムアウト値は専用のものを見るようにする事
  (with-timer-handler
    (response-timeout-of self)
    (lambda ()
      (set! (ref state 'response-code) 504)
      (set! (ref state 'internal-description) 'cgi_script_timeout)
      #f)
    (lambda ()
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
               (set! (ref state 'connection-close) #t) ; 接続断なので
               (set!
                 (ref state 'internal-description)
                 'connection_reset_by_peer)
               (set! (ref state 'cgi-error-instance) e))
              ((#/^unhandled\ssignal\s/ message)
               ;; その他のシグナルだった。多分、終了の合図。
               ;; ToDo : シグナルの種類によっては、すぐに接続断しない
               (set! (ref state 'connection-close) #t) ; 接続断？
               (set! (ref state 'internal-description) 'signal_received)
               (set! (ref state 'cgi-error-instance) e))
              (else
                ;; それ以外のエラーなら、エラーログに残す
                (set! (ref state 'response-code) 500)
                (set!
                  (ref state 'internal-description)
                  'cgi_script_error_exception)
                (set! (ref state 'cgi-error-instance) e)
                (report-error e))) ; エラー内容をエラーログに出力する
            #f))
        (lambda ()
          (let* ((dispatched-cgi-target (ref state 'dispatched-cgi-target))
                 (cgi-thunk
                   (cond
                     ((not dispatched-cgi-target)
                      (error "cannot_found_dispatched_cgi_target"))
                     ((string? dispatched-cgi-target)
                      (make-filer-thunk
                        dispatched-cgi-target
                        :error-proc (lambda (response-code)
                                      (tcpcgi-display-errordoc!
                                        self
                                        response-code))))
                     ((procedure? dispatched-cgi-target)
                      dispatched-cgi-target)
                     (else (error "assert exception"))))
                 )
            (parameterize ((cgi-metavariables
                             (ref state 'merged-cgi-metavariables)))
              (if (ref state 'nph)
                (begin
                  (set! (ref state 'internal-description) 'work_under_nph_mode)
                  (set! (ref state 'connection-close) #t)
                  (cgi-thunk)
                  #f) ; nphでは、この後の処理は行わないので#fを返す
                (begin
                  (set!
                    (ref state 'cgi-response)
                    (with-string-io
                      (or
                        (ref state 'request-body)
                        "")
                      cgi-thunk))
                  #t)))))))))



(define (parse-cgi-response! self state)
  (let1 cgi-response (ref state 'cgi-response)
    (and
      (when (= 0 (string-size cgi-response))
        (set! (ref state 'response-code) 500)
        (set!
          (ref state 'internal-description)
          'premature_end_of_script_headers)
        #f)
      (with-error-handler
        (lambda (e)
          (set! (ref state 'response-code) 500)
          (set!
            (ref state 'internal-description)
            'malformed_header_from_script)
          #f)
        (lambda ()
          (let1 cgi-response-port (open-input-string cgi-response)
            (set!
              (ref state 'cgi-response-header)
              (rfc822-header->list
                cgi-response-port
                :strict? #t))
            (set!
              (ref state 'cgi-response-body)
              (get-remaining-input-string cgi-response-port))
            #t))))))



(define get-header-value cgi-get-parameter)



(define (status-string->status-number status)
  (and-let* ((m (#/^(\d+)\s/ status)))
    (string->number (m 1))))




(define (make-full-uri mv location)
  (receive (uri-scheme
            uri-userinfo
            uri-hostname
            uri-port
            uri-path
            uri-query
            uri-fragment)
    (uri-parse location)
    ;; uri-schemeが無い時のみ補完する
    (if uri-scheme
      location
      (let* ((https (get-header-value "HTTPS" mv))
             (scheme (if https
                       "https"
                       "http"))
             (default-port (if https
                             "443"
                             "80"))
             (port (get-header-value "SERVER_PORT" mv))
             (host (or
                     (get-header-value "SERVER_NAME" mv)
                     (get-header-value "SERVER_ADDR" mv)
                     (sys-gethostname)))
             )
        (uri-compose
          :scheme scheme
          :userinfo uri-userinfo
          :host host
          :port (and
                  port
                  (not (string=? default-port port))
                  port)
          :path uri-path
          :query uri-query
          :flagment uri-fragment)))))


(define (make-location-html-string full-uri)
  (tree->string
    (html:html
      (html:head
        (html:title "302 Found"))
      (html:body
        (html:h1 "Found")
        (html:p "The document has moved "
                (html:a :href full-uri
                        (html-escape-string full-uri))
                ".")))))


(define (make-http-response! self state)
  (let* ((cgi-response-header (ref state 'cgi-response-header))
         (cgi-response-body (ref state 'cgi-response-body))
         (status (get-header-value "status" cgi-response-header))
         (location (get-header-value "location" cgi-response-header))
         (content-type (get-header-value "content-type" cgi-response-header))
         (use-persistent-connection (ref state 'use-persistent-connection))
         (use-keep-alive (ref state 'use-keep-alive))
         )
    (define (pre-response-header->http-header src-alist src-body)
      ;; 状況に応じて、ヘッダを追加する
      ;; 追加する必要があるのは、Date, Connection, Content-Length, Server
      ;; 但し、Serverはコメントアウト状態とする
      ;; - Dateは常に付ける
      ;; - Connection, Content-Lengthは、use-persistent-connectionによる
      ;; -- 但し、Content-Lengthは、既に付いているなら、付けない
      `(("date" ,(sys-strftime
                   "%a, %d %b %Y %X %Z"
                   (sys-gmtime (sys-time)))) ; 常に必要
        ,@(if (not use-persistent-connection)
            '(("connection" "close")) ; 持続的接続でないならclose
            (if use-keep-alive
              '(("connection" "Keep-Alive"))
              '()))
        ,@(or
            (and
              use-persistent-connection
              (not (assoc "content-length" cgi-response-header))
              `(("content-length" ,(string-size cgi-response-body))))
            '())
        ,@(if (use-server-header-of self)
            `(("server" ,(get-header-value
                           "SERVER_SOFTWARE"
                           (ref state 'merged-cgi-metavariables))))
            '())
        ,@src-alist))

    (define (parse-error internal-description)
      (set! (ref state 'response-code) 500)
      (set! (ref state 'internal-description) internal-description)
      #f)

    (let/cc return
      (receive (response-code
                response-status-line
                pre-response-header
                response-body)
        (cond
          (status (let1 status-number (status-string->status-number status)
                    (if status-number
                      (values
                        status-number
                        (format "HTTP/1.1 ~a" status)
                        (alist-delete "status"
                                      cgi-response-header
                                      string=?)
                        cgi-response-body)
                      (return (parse-error 'invalid_status_header_of_cgi)))))
          (location (let ((true-location
                            (make-full-uri
                              (ref state 'merged-cgi-metavariables)
                              location))
                          )
                      (values
                        302
                        "HTTP/1.1 302 Found"
                        (list*
                          (list "location" true-location)
                          (list "content-type" "text/html")
                          (remove
                            (lambda (l)
                              (let1 k (car l)
                                (or
                                  (string=? "location" k)
                                  (string=? "content-type" k))))
                            cgi-response-header))
                        (make-location-html-string true-location))))
          (content-type (values
                          200
                          "HTTP/1.1 200 OK"
                          cgi-response-header
                          cgi-response-body))
          ;; apacheでは、この場合に勝手にContent-Type: text/plainを付加する
          ;; 同じ挙動にする
          (else (values
                  200
                  "HTTP/1.1 200 OK"
                  (cons
                    '("content-type" "text/plain")
                    cgi-response-header)
                  cgi-response-body)))
        (set! (ref state 'response-code) response-code)
        (set! (ref state 'response-status-line) response-status-line)
        (set!
          (ref state 'response-header)
          (pre-response-header->http-header
            pre-response-header
            response-body))
        (set! (ref state 'response-body) response-body)
        #t))))


(define *crlf* "\r\n")


(define (tcpcgi-display-errordoc! self response-code)
  (unless (errordoc-instance-of self)
    (set!
      (errordoc-instance-of self)
      (make <tcpcgi.errordoc>
        :ht (apply hash-table 'eqv? (errordoc-alist-of self)))))
  (display-errordoc (errordoc-instance-of self) response-code))


;; response-codeのエラーをHTTP/1.1形式でdisplayする
(define (response-error! self state response-code)
  (define (execute-errordoc-thunk thunk)
    (set! (ref state 'dispatched-cgi-target) thunk)
    (and
      thunk
      ;; tcpcgi-mainと同じような順に実行する。
      ;; ココは、tcpcgi-mainから持ってきた。
      (execute-cgi-thunk! self state)
      (parse-cgi-response! self state)
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
    (execute-errordoc-thunk
      (lambda ()
        (tcpcgi-display-errordoc! self response-code)))))

(define (display-http-object status-line header body)
  ;; status-line出力
  (display status-line)
  (display *crlf*)
  ;; header出力
  ;; cgiからの戻り値のheaderは、form形式の複数値は無いので、
  ;; そのままcadrを使っていい
  (for-each
    (lambda (key&values)
      (display (string-titlecase (car key&values)))
      (display ": ")
      (display (cadr key&values))
      (display *crlf*))
    header)
  (display *crlf*)
  ;; body出力
  (display body)
  (flush))


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




(define-method tcpcgi-main ((self <tcpcgi>)
                            server-addr
                            server-port
                            server-name
                            remote-addr
                            remote-port
                            remote-host ; or #f
                            https) ; boolean
  (define (with-buffering-port thunk)
    (let ((old-log-port-mode #f)
          (old-error-port-mode #f)
          )
      (dynamic-wind
        (lambda ()
          (when (log-port-of self)
            (set!
              old-log-port-mode
              (port-buffering (log-port-of self)))
            (set! (port-buffering (log-port-of self)) :line))
          (when (cgi-error-port-of self)
            (set!
              old-error-port-mode
              (port-buffering (cgi-error-port-of self)))
            (set! (port-buffering (cgi-error-port-of self)) :line))
          )
        thunk
        (lambda ()
          (when (log-port-of self)
            (set! (port-buffering (log-port-of self)) old-log-port-mode))
          (when (cgi-error-port-of self)
            (set!
              (port-buffering (cgi-error-port-of self))
              old-error-port-mode))
          ))))

  (define (with-signal-handler-guard thunk)
    ;; SIGPIPE, SIGINT, SIGTERM, SIGHUPから保護する
    (define (signal-handler s)
      (set! (ref self 'signaled) #t))
    ;; ToDo : ↓race condition対策に初期値undefにするようにしたので、
    ;;        ↓ちゃんとundefでないかどうかを判定する事
    (let* ((undef (when #f #f))
           (old-sigpipe undef)
           (old-sigint undef)
           (old-sigterm undef)
           (old-sighup undef)
           )
      ;; 本当は、with-signal-handlersを使いたいが、使うと、何故か、
      ;; 外部コマンドの実行に支障が出る…‥
      (dynamic-wind
        (lambda ()
          (set! old-sigpipe (get-signal-handler SIGPIPE))
          (set! old-sigint  (get-signal-handler SIGINT))
          (set! old-sigterm (get-signal-handler SIGTERM))
          (set! old-sighup  (get-signal-handler SIGHUP))
          (set-signal-handler! SIGPIPE signal-handler)
          (set-signal-handler! SIGINT signal-handler)
          (set-signal-handler! SIGTERM signal-handler)
          (set-signal-handler! SIGHUP signal-handler)
          )
        thunk
        (lambda ()
          (set-signal-handler! SIGPIPE old-sigpipe)
          (set-signal-handler! SIGINT old-sigint)
          (set-signal-handler! SIGTERM old-sigterm)
          (set-signal-handler! SIGHUP old-sighup)
          ))))

  (define (execute-one-transaction! state)
    ;; sigpipeのシグナル自体は無効化してあるが、sigpipeの起こったportに
    ;; writeしようとするとエラーが起こるので、それを防ぐ必要がある
    (define (block-sigpipe-error thunk)
      (with-error-handler
        (lambda (e)
          ;; 何にしても、致命的なので、この時点で終了の準備をする
          (set! (ref state 'connection-close) #t) ; 接続断
          (set! (ref state 'cgi-error-instance) e) ; 記録する
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
          (tcpcgi-request-consume! self state)

          (begin0
            ;; ここまでに得られた情報を使ってディスパッチする
            (set-dispatched-info! self state)

            ;; persistent-connectionを使うかどうか判定
            ;; コレはdispatch後、#fが返ってきても実行して欲しい
            (check&set-persistent-connection! self state))

          ;; 認証有りなら、認証を行う
          (check-authentication! self state)

          ;; ここから先は、エラードキュメント生成時に再度呼び出される。
          ;; この辺の修正時には、エラードキュメント生成部分もチェックする事

          ;; メタ変数を構築
          (make-cgi-metavariables! self state)

          ;; cgiスクリプトを実行。タイムアウト判定有り。
          (execute-cgi-thunk! self state)

          ;; cgiスクリプトの実行結果をパーズしてheaderとbodyを得る
          (parse-cgi-response! self state)

          ;; cgiスクリプトの実行結果のパーズ内容から、
          ;; 実際にクライアントに返すhttpレスポンスの元情報を生成する。
          (make-http-response! self state)
          )))

    ;; ロギングする
    (tcpcgi-logging self state)

    ;; (結果を返す必要があるなら)結果をクライアントに返して終了
    ;; sigpipeは書き込もうとしないと判定できないようで、ヘッダ読み込み時に
    ;; 切断されても、パーズエラーにならなければ、そのままcgi実行が行われる。
    ;; そして、最後の書き出し時にsigpipeが起こる。
    (block-sigpipe-error
      (lambda ()
        (send-response! self state))))

  (define (tcpcgi-execute! state counter)
    ;; とりあえずリクエスト一回を処理
    (execute-one-transaction! state)
    ;; 永続的接続が有効な場合は、再度実行する
    (when (and
            (not (ref state 'connection-close))
            (ref state 'use-persistent-connection)
            (not (ref self 'signaled))
            (< 1 counter)
            )
      (tcpcgi-execute!
        (make <tcpcgi.state>
          :first-transaction #f ; 二回目以降は#f
          :server-addr server-addr
          :server-port (x->string server-port)
          :server-name server-name
          :remote-addr remote-addr
          :remote-port remote-port
          :remote-host remote-host
          :https https)
        (- counter 1))))


  ;; 接続してきたクライアントの処理を開始
  (with-buffering-port
    (lambda ()
      (with-signal-handler-guard
        (lambda ()
          (with-error-to-port
            (or
              (cgi-error-port-of self)
              (open-output-file "/dev/null" :buffering :none))
            (lambda ()
              (tcpcgi-execute!
                (make <tcpcgi.state>
                  :first-transaction #t ; 一回目のみ#t
                  :server-addr server-addr
                  :server-port (x->string server-port)
                  :server-name server-name
                  :remote-addr remote-addr
                  :remote-port remote-port
                  :remote-host remote-host
                  :https https)
                (max-requests-per-child-of self)))))))))


(provide "tcpcgi")

