;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme sw=2 ts=2 et:
;;; $Id$


;;; ToDo : tcpserver��߻��ˡ��ޤ���ư���Ƥ���tcpcgi�˥����ʥ뤬ή���Τǡ�
;;;        ����ʤ���к���Ԥ�
;;;        �ʸ����Ǥ�ư��Ū�ˤ������̵����������stacktrace���Ĥ��

;;; ToDo : �ǥ����ѥå������ѿ�̾�Υ�ե�������󥰤�ɬ��

;;; ToDo : �٤����⥸�塼���ʬ��

;;; ToDo : check-authentication!����

;;; ToDo : rfc2616�򸫤ơ����饤����Ȥ������³��HTTP/1.1�ʤ顢
;;;        ���饤����Ȥ����method��إå���HTTP/1.1���������Ƥ��뤫��
;;;        �����å������������Ƥ��ʤ��ʤ顢Ŭ�ڤʥ��顼���֤��褦�ˤ���

;;; ToDo : �ѥե����ޥ󥹸���ΰ٤ˡ����饤����Ȥ�������Ϥ�Хåե����
;;;        ���ʤ����ץ�����Ĥ��롩
;;;        ���λ��˥��顼��ȯ�������顢�ꥯ�����Ȥ�Content-Length������ʤ顢
;;;        0���ѹ����Ƥ��饨�顼thunk��Ƥ֤褦�ˤ��ʤ��ƤϤʤ�ʤ�
;;;        ����Ť�read���Ƥ��ޤ��Ȥޤ�����

;;; ToDo : state�Υ���åȤν���ͤ�#f����ˤʤäƤ��뤬��
;;;        ���������Ǥ�ư���褦�ˤ��������ɤ���������Τǡ���Ƥ�����

;;; ToDo : ������Ū��DoS�к�
;;;        apache��RLimit�Ϥ�����򿿻�����


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
  (use tcpcgi.errordoc) ; default-errordoc-thunk-table fallback-errordoc-thunk
  ;(use tcpcgi.filer) ; ToDo : ������
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

   ;; #f�ʤ���󥰤��ʤ�
   (log-port :accessor log-port-of
             :init-keyword :log-port
             :init-form (current-error-port))
   ;; #f�ʤ���󥰤��ʤ�
   (logger :accessor logger-of
           :init-keyword :logger
           :init-form default-logger) ; ������<tcpcgi.state>�����������

   ;; #f�ʤ�/dev/null��ή��
   ;; ���Υ��ե������:buffering :none�ǳ����Ƥ�����
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

   ;; key�Ȥ��ƥ��顼�ֹ�ο��͡�
   ;; value�Ȥ��ƥ��顼�ɥ�����Ȥ���ϤǤ���cgi-thunk��
   ;; ���������ä�hash-table�򥳥��˻��ꤹ��
   ;; ToDo : �����dispatch���饹�����٤�����
   (errordoc-table
     :accessor errordoc-table-of
     :init-keyword :errordoc-table
     :init-form (make-hash-table 'eqv?))

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
   ))


;; accessor-method
(define-method get-vhost-dispatch ((self <tcpcgi>))
  (vhost-dispatch-of self))
(define-method get-path-dispatch ((self <tcpcgi>))
  (path-dispatch-of self))
(define-method get-none-dispatch ((self <tcpcgi>))
  (none-dispatch-of self))
(define-method set-vhost-dispatch! ((self <tcpcgi>) new-vhost-alist)
  (set! (vhost-dispatch-instance-of self) #f)
  (set! (vhost-dispatch-of self) new-vhost-alist))
(define-method set-path-dispatch! ((self <tcpcgi>) new-path-alist)
  (set! (path-dispatch-instance-of self) #f)
  (set! (path-dispatch-of self) new-path-alist))
(define-method set-none-dispatch! ((self <tcpcgi>) new-value)
  (set! (none-dispatch-instance-of self) #f)
  (set! (none-dispatch-of self) new-value))



(define (default-logger state)
  (write
    (list
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
  ;; ToDo : SIGALRM��Ȥ�ʤ���ˡ��ͤ��롣
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



;; �������鲼�ˤ��롢������!�դ��ؿ��ϡ�
;; �����������������ä��顢state�˥��顼�������򥻥åȤ��Ƥ���#f���֤���
;; ����ʾ��ϡ��ؿ�̾���ͤ򥻥åȤ����������Ƥ���#t���֤���


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
         (request-method (car mup)) ; �����ޤ���줿�ʾ塢�����ɬ��¸�ߤ���
         (request-uri (if (= 1 mup-num)
                        "/" ; HTTP/0.9���Ȥ��ε�ư
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
    ;; �����ޤǼ����������Ƥ򸡾ڤ���#t��#f���֤�
    (cond
      ((not (<= mup-num 3)) ; �ѥ�᡼���ϻ��Ĥޤ�
       (error-return 'too_many_request_line_parameter))
      ((not (#/^http\/\d\.\d/i request-protocol)) ; protocol��HTTP/n.m��
       (error-return 'bad_protocol))
      ((not (#/^\// (list-ref parsed-uri-list 4))) ; path��/�Ϥ��ޤ�
       (error-return 'bad_path))
      ((let1 uri-scheme (list-ref parsed-uri-list 0)
         (and
           uri-scheme ; uri-scheme��ͭ�ꡢ
           (not (#/^https?$/ uri-scheme)))) ; http�Ǥ�https�Ǥ�ʤ�
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
        "")) ; �⤷cgi������ץȤ���ä��ɤ߽Ф����Ȥ��������ɻ���
    #t))




(define (tcpcgi-request-consume! self state)
  ;; ���饤����Ȥ���read������ʬ�������ॢ����Ƚ���Ԥ�ɬ�פ����롣
  ;; �ޤ���keep-aliveȽ��ΰ٤ˡ�(ref state 'first-transaction)�ˤ�äơ�
  ;; ��ư���ѹ�����ɬ�פ����롣
  (and
    ;; first-transaction�Ǥʤ��ʤ顢keepalive�ǥ����ॢ���Ȥ��뤫������å�
    (if (ref state 'first-transaction)
      #t
      (with-timer-handler
        (keepalive-timeout-of self)
        (lambda ()
          (set! (ref state 'connection-close) #t) ; ��³��λ
          (set! (ref state 'internal-description) 'keepalive_timeout)
          #f)
        (lambda ()
          (peek-byte)
          ;; �����Ƶ��κ�Ŭ���ǥ����ޡ���ª�˼��Ԥ��ʤ��褦�ˡ�
          ;; �����Ƶ��κ�Ŭ��������ʤ��褦�ˤ��Ƥ�����ǰ�ΰ�
          #t)))
    (with-timer-handler
      (request-timeout-of self)
      (lambda ()
        (set! (ref state 'connection-close) #t) ; ��³��λ
        (set! (ref state 'response-code) 408)
        (set! (ref state 'internal-description) 'request_timeout)
        #f)
      (lambda ()
        ;; ����Ǽ��Ԥ����顢��ν����ϥѥ�����
        (and
          ;; request-line�����
          (read-request-line! state)

          ;; request-line����method, path, protocol, 0.9flag������������å�
          (parse-request-line! state)

          ;; request-header�����
          (read-request-header! state)

          ;; ���饤����Ȥ���POST�ͤ�ͽ�����
          (prepare-request-content! state)
          )))))





(define (dispatch-none! state dispatcher)
  (and
    (not (boolean? dispatcher))
    (let* ((cgi-list (dump-dispatch.none dispatcher))
           (cgi-thunk (car cgi-list))
           (keywords (cdr cgi-list))
           (nph (get-keyword :nph keywords #f))
           )
      (set! (ref state 'dispatched-cgi-thunk) cgi-thunk)
      (set! (ref state 'nph) nph)
      #t)))

(define (dispatch-path! state dispatcher)
  (and
    (not (boolean? dispatcher)) ; #t, #f�ϵѲ���#f���֤���
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
                plain-path-info ; #f�ʤ�#f���å�
                (not (string=? "" plain-path-info)) ; ""�λ���#f�򥻥å�
                plain-path-info)) ; ""�Ǥʤ��ʤ顢plain-path-info�򥻥å�
            ;; �Ǹ�ˡ�dispatch-none����򥻥åȤ��ƽ�λ
            (dispatch-none! state none-dispatch)))))))

(define (dispatch-vhost! state dispatcher)
  (and
    (not (boolean? dispatcher)) ; #t, #f�ϵѲ���#f���֤���
    (and-let* (
               (vhost (or
                        ;; Host�إå�
                        (and-let* ((host (get-header-value
                                           "host"
                                           (ref state 'request-header)))
                                   ;; port��ʬ���դ��Ƥ���ʤ�����
                                   (m (#/^([\w\-\.]+)(?:\:\d+)?$/ host))
                                   )
                          (m 1))
                        ;; �ޤ��ϡ�request-line��uri-hostname
                        (list-ref (ref state 'parsed-uri-list) 2)
                        ;; �ޤ��ϡ�SERVER_NAME
                        (ref state 'server-name)
                        ))
               )
      (receive (path-dispatch server-name)
        (dispatch.vhost->dispatch.path dispatcher vhost)
        (and
          path-dispatch
          (begin
            (set! (ref state 'dispatched-server-name) server-name)
            ;; �Ǹ�ˡ�dispatch-path����򥻥åȤ��ƽ�λ
            (dispatch-path! state path-dispatch)))))))

;; �ص��塢���̲���������
(define-syntax set-dispatch-instance!
  (syntax-rules ()
    ((_ self *-dispatch-instance-of *-dispatch-of make-dispatch-*)
     (unless (*-dispatch-instance-of self)
       (set!
         (*-dispatch-instance-of self)
         (let1 src (*-dispatch-of self)
           (or
             (not src) ; src��#f�λ��ϡ������å��ѤξڤȤ���#t�򥻥å�
             (make-dispatch-* src) ; ���󥹥��󥹲������������֤��ͤ�#f�ʤ顢
             #t))))))) ; �����å��ѤξڤȤ���#t�򥻥å�

(define (tcpcgi-dispatch-none! self state)
  ;; �ޤ���ˡ���������åȤΥ��󥹥��󥹤򹹿�
  (set-dispatch-instance!
    self
    none-dispatch-instance-of
    none-dispatch-of
    make-dispatch-none)
  ;; state���ͤ򥻥åȤ���
  (dispatch-none! state (none-dispatch-instance-of self)))

(define (tcpcgi-dispatch-path! self state)
  ;; �ޤ���ˡ���������åȤΥ��󥹥��󥹤򹹿�
  (set-dispatch-instance!
    self
    path-dispatch-instance-of
    path-dispatch-of
    make-dispatch-path)
  ;; �ǥ����ѥå�������¹Ԥ���state���ͤ򥻥åȤ���
  (dispatch-path! state (path-dispatch-instance-of self)))

(define (tcpcgi-dispatch-vhost! self state)
  ;; �ޤ���ˡ���������åȤΥ��󥹥��󥹤򹹿�
  (set-dispatch-instance!
    self
    vhost-dispatch-instance-of
    vhost-dispatch-of
    make-dispatch-vhost)
  ;; �ǥ����ѥå�������¹Ԥ���state���ͤ򥻥åȤ���
  (dispatch-vhost! state (vhost-dispatch-instance-of self)))


;; ToDo : �����դ�̾����ʬ����ˤ����Τǡ����Ȥǥ�ե�������󥰤����
;; - tcpcgi-dispatch-vhost!�ϡ�������ԤäƤ��顢dispatch-vhost!��¹Ԥ���
;; -- dispatch-vhost!�ϡ�vhost��Ϣ�ξ����state�˥��åȤ���
;;    ����vhost����path����Ф���dispatch-path!��¹Ԥ���
;; - tcpcgi-dispatch-path!�ϡ�������ԤäƤ��顢dispatch-path!��¹Ԥ���
;; -- dispatch-path!�ϡ�path��Ϣ�ξ����state�˥��åȤ���
;;    ����path����none����Ф���dispatch-none!��¹Ԥ���
;; - tcpcgi-dispatch-none!�ϡ�������ԤäƤ��顢dispatch-none!��¹Ԥ���
;; -- dispatch-none!�ϡ�cgi-thunk��Ϣ�ξ����state�˥��åȤ��롣
(define (set-dispatched-info! self state)
  (define (return404)
    (set! (ref state 'response-code) 404)
    (set! (ref state 'internal-description) 'cannot_found_cgi_thunk)
    #f) ; ���θ塢������³���ʤ��Τ�#f���֤�

  (and
    (or ; �ʲ��Τ������ɤ줫��ļ¹Դ�λ������ɤ�
      (tcpcgi-dispatch-vhost! self state)
      (tcpcgi-dispatch-path! self state)
      (tcpcgi-dispatch-none! self state)
      (return404)) ; �Ǹ�ޤ�cgi-thunk�����դ���ʤ��ä��Τǡ�404���֤��ƽ�λ
    ;; dispatch�ϼ¹Դ�λ������������Ū��404�����ꤵ��Ƥ����礬����
    (unless (ref state 'dispatched-cgi-thunk)
      (return404))))



(define (check&set-persistent-connection! self state)
  (and
    (use-persistent-connection-of self) ; ���Ѥ��륪�ץ����ˤʤäƤ����
    (not (ref state 'connection-close)) ; ��³�ǤǤʤ���
    (not (ref state 'nph)) ; nph�Ǥʤ���
    (string=? "HTTP/1.1" (or (ref state 'request-protocol) "")) ; HTTP/1.1
    (set! (ref state 'use-persistent-connection) #t)) ; �ʤ顢ͭ��
  #t) ; ���#t���֤���



(define (check-authentication! self state)
  ;; ToDo : �ޤ�̤����
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



;; Ʊ�������ν�ʣ�Τ���rfc822��alist���饭���ν�ʣ��ʤ����ơ�
;; ��ĤΥ�����ʣ�����ͤ����alist�����������֤�
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
                        (rfc822-header-merge over-mv) ; over��
                        (rfc822-header-merge under-mv) ; under����
                        ))
                 (result '()))
        (if (null? src)
          result
          (loop
            (cdr src)
            (let1 line (car src)
              (if (assoc (car line) result)
                result ; Ʊ̾����������¸�ߤ���ʤ��ɲä��ʤ�
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
      ;; alist���֤�
      `(
        ("GATEWAY_INTERFACE" "CGI/1.1")
        ,@(list-or-empty "AUTH_TYPE" (ref state 'auth-type)) ; apache�ߴ��ε�ư
        ("SERVER_SOFTWARE" ,(server-software-of self))
        ;("DOCUMENT_ROOT" ,document-root) ; apache��ĥ�������Ǥ�̵��
        ;("SERVER_ADMIN" ,server-admin) ; apache��ĥ�������Ǥ�̵��

        ("SERVER_ADDR" ,server-addr)
        ("SERVER_PORT" ,(ref state 'server-port))
        ("SERVER_NAME" ,(or
                          (ref state 'server-name)
                          server-addr)) ; fallback

        ("REMOTE_ADDR" ,(ref state 'remote-addr))
        ("REMOTE_PORT" ,(ref state 'remote-port))
        ;("REMOTE_HOST" ,(or
        ;                  (ref state 'remote-host)
        ;                  ""))
        ;; ����CGI/1.1�λ��ͤ�����apache�Ϣ��ε�ư
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
                             (not script-name) ; �ޤ�dispatch���Ƥ��ʤ��ʤ顢
                             (list-ref parsed-uri-list 4))))) ; uri-path�����
              (uri-decode-string plain-path-info :cgi-decode #t)))
        ;("PATH_TRANSLATED" ,path-translated) ; �����Ǥ�̵��
        ("QUERY_STRING" ,(or
                           (list-ref parsed-uri-list 5)
                           ""))
        ("SCRIPT_NAME" ,(or
                          script-name
                          ""))
        ;("SCRIPT_FILENAME" ,(with-module
        ;                      user
        ;                      *program-name*)) ; apache��ĥ��̵����

        ;; HTTPS
        ,@(list-or-empty "HTTPS" (ref state 'https))

        ;; CONTENT_TYPE, CONTENT_LENGTH
        ,@(list-or-empty "CONTENT_TYPE" (ref state 'content-type))
        ,@(list-or-empty "CONTENT_LENGTH" (ref state 'content-length))

        ;; ���饤����Ȥ�HTTP�إå�������������᥿�ѿ���HTTP_*
        ;; â����Content-Type, Content-Length,
        ;; Authorization, Proxy-Authorization��HTTP_*�����ʤ�
        ,@(filter-map
            (lambda (line)
              (if (*exclude-http-header-regexp* (car line))
                #f
                (list
                  (string-append "HTTP_" (header-name->env-name
                                           (car line)))
                  (string-join ; Ʊ�������Υإå���ʣ������ʤ�Ϣ��
                    (cdr line)
                    ", "))))
            ;; rfc822�إå��ϡ�Ʊ��̾���Υإå���ͭ������Τǡ�
            ;; ������ͽ��ޡ������Ƥ���
            (rfc822-header-merge (or
                                   (ref state 'request-header)
                                   '()))))))

  (let1 mv (make-mv)
    (set! (ref state 'plain-cgi-metavariables) mv)
    (set!
      (ref state 'merged-cgi-metavariables)
      (get-merged-metavariables-alist
        mv ; under-mv��
        (cgi-metavariables))) ; over-mv����
    #t))



(define (execute-cgi-thunk! self state)
  (with-timer-handler
    (response-timeout-of self)
    (lambda ()
      (set! (ref state 'response-code) 504)
      (set! (ref state 'internal-description) 'cgi_script_timeout)
      #f)
    (lambda ()
      (with-error-handler
        (lambda (e)
          (if (and
                (eq? <system-error> (class-of e))
                ;; "write failed on #<oport (stdout) 0x8083e40>: Broken pipe"
                (#/^write\sfailed\son\s/ (ref e 'message))
                )
            (begin
              (set! (ref state 'connection-close) #t) ; ��³�ǤʤΤ�
              (set!
                (ref state 'internal-description)
                'connection_reset_by_peer)
              (set! (ref state 'cgi-error-instance) e))
            (begin
              (set! (ref state 'response-code) 500)
              (set!
                (ref state 'internal-description)
                'cgi_script_error_exception)
              (set! (ref state 'cgi-error-instance) e)
              (report-error e))) ; ���顼���Ƥ򥨥顼���˽��Ϥ���
          #f)
        (lambda ()
          (let1 cgi-thunk (lambda ()
                            (with-input-from-string
                              (ref state 'request-body)
                              (ref state 'dispatched-cgi-thunk)))
            (parameterize ((cgi-metavariables
                             (ref state 'merged-cgi-metavariables)))
              (if (ref state 'nph)
                (begin
                  (set! (ref state 'internal-description) 'work_under_nph_mode)
                  (set! (ref state 'connection-close) #t)
                  (cgi-thunk)
                  #f) ; nph�Ǥϡ����θ�ν����ϹԤ�ʤ��Τ�#f���֤�
                (begin
                  (set!
                    (ref state 'cgi-response)
                    (with-output-to-string
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
    ;; uri-scheme��̵�����Τ��䴰����
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
         (use-persistent-connection (use-persistent-connection-of self))
         )
    (define (pre-response-header->http-header src-alist src-body)
      ;; �����˱����ơ��إå����ɲä���
      ;; �ɲä���ɬ�פ�����Τϡ�Date, Connection, Content-Length, Server
      ;; â����Server�ϥ����ȥ����Ⱦ��֤Ȥ���
      ;; - Date�Ͼ���դ���
      ;; - Connection, Content-Length�ϡ�use-persistent-connection�ˤ��
      ;; -- â����Content-Length�ϡ������դ��Ƥ���ʤ顢�դ��ʤ�
      `(("date" ,(sys-strftime
                   "%a, %d %b %Y %X %Z"
                   (sys-gmtime (sys-time)))) ; ���ɬ��
        ,@(if use-persistent-connection
            '()
            '(("connection" "close"))) ; ��³Ū��³�Ǥʤ��ʤ�ɬ��
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
          ;; apache�Ǥϡ����ξ��˾����Content-Type: text/plain���ղä���
          ;; Ʊ����ư�ˤ���
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

;; response-code�Υ��顼��HTTP/1.1������display����
(define (response-error! self state response-code)
  (define (execute-errordoc-thunk thunk)
    (set! (ref state 'dispatched-cgi-thunk) thunk)
    (and
      thunk
      ;; tcpcgi-main��Ʊ���褦�ʽ�˼¹Ԥ��롣
      ;; �����ϡ�tcpcgi-main������äƤ�����
      (execute-cgi-thunk! self state)
      (parse-cgi-response! self state)
      (make-http-response! self state)
      (send-response! self state)
      ;; ���꤬ȯ��������#f���֤��Ȥ�������ˤʤäƤ��롣
      ))

  (or
    (ref state 'connection-close) ; ��³�Ǥʤ鲿�⤷�ʤ�
    (begin
      ;; �ͤ���������
      (set! (ref state 'nph) #f) ; ����Ū����nphư��Ȥ��롢���ΤȤ����
      (unless (ref state 'merged-cgi-metavariables)
        ;; ���顼�����ˤ�äƤϡ�merged-cgi-metavariables��̵������
        ;; ����Τǡ��������Ƥ���
        (make-cgi-metavariables! self state))
      ;; ���顼cgi�˥쥹�ݥ󥹥����ɤ��󶡤���
      (set!
        (ref state 'merged-cgi-metavariables)
        (cons
          (list "X_RESPONSE_CODE" (number->string response-code))
          (ref state 'merged-cgi-metavariables)))
      #f) ; ³��
    ;; �ʲ���#t���Ф�ޤ�ĩ�魯��
    (execute-errordoc-thunk
      (ref (errordoc-table-of self) response-code #f))
    (execute-errordoc-thunk
      (ref default-errordoc-thunk-table response-code #f))
    (execute-errordoc-thunk
      fallback-errordoc-thunk)))

(define (display-http-object status-line header body)
  ;; status-line����
  (display status-line)
  (display *crlf*)
  ;; header����
  ;; cgi���������ͤ�header�ϡ�form������ʣ���ͤ�̵���Τǡ�
  ;; ���Τޤ�cadr��ȤäƤ���
  (for-each
    (lambda (key&values)
      (display (string-titlecase (car key&values)))
      (display ": ")
      (display (cadr key&values))
      (display *crlf*))
    header)
  (display *crlf*)
  ;; body����
  (display body)
  (flush))


(define (send-response! self state)
  ;; connection-close��#t�ʤ鲿���֤��ʤ�
  (unless (ref state 'connection-close)
    (let ((response-status-line (ref state 'response-status-line))
          (response-header (ref state 'response-header))
          (response-body (ref state 'response-body))
          (response-code (ref state 'response-code))
          )
      ;; response-body������ʤ����̤˽��ϡ�̵���ʤ饨�顼�����
      (cond
        ((not response-body) (response-error! self state response-code))
        ((ref state 'http-0.9-flag) (display response-body))
        (else
          (display-http-object
            response-status-line
            response-header
            response-body)))))
  (flush) ; �Ǹ�ˡ�ǰ�ΰ١�flush���Ƥ���
  #t)




(define-method tcpcgi-main ((self <tcpcgi>)
                            server-addr
                            server-port
                            server-name
                            remote-addr
                            remote-port
                            remote-host ; or #f
                            https) ; boolean
  ;; sigpipe��̵�뤹��
  (define (with-ignore-sigpipe-handler thunk)
    (let1 old-sigpipe-handler #t
      (dynamic-wind
        (lambda ()
          (set! old-sigpipe-handler (get-signal-handler SIGPIPE))
          (set-signal-handler!  SIGPIPE #f))
        thunk
        (lambda ()
          (set-signal-handler! SIGPIPE old-sigpipe-handler)))))

  (define (execute-one-transaction! state)
    ;; sigpipe�Υ����ʥ뼫�Τ�̵�������Ƥ��뤬��sigpipe�ε����ä�port��
    ;; write���褦�Ȥ���ȥ��顼��������Τǡ�������ɤ�ɬ�פ�����
    (define (block-sigpipe-error thunk)
      (with-error-handler
        (lambda (e)
          (if (and
                (eq? <system-error> (class-of e))
                ;; "write failed on #<oport (stdout) 0x8083e40>: Broken pipe"
                (#/^write\sfailed\son\s/ (ref e 'message))
                )
            (begin
              (set! (ref state 'connection-close) #t) ; ��³�ǤʤΤ�
              (set! (ref state 'internal-description) 'client_disconnected)
              (set! (ref state 'cgi-error-instance) e)
              #f)
            (report-error e))) ; sigpipe�ʳ��Υ��顼�ʤ顢���顼���˻Ĥ�
        thunk))

    (block-sigpipe-error
      (lambda ()
        ;; ����Ǽ��Ԥ����顢��ν����ϥѥ�����
        (and
          ;; ���饤����Ȥ�http�ꥯ�����Ȥ�����������
          (tcpcgi-request-consume! self state)

          (begin0
            ;; �����ޤǤ�����줿�����Ȥäƥǥ����ѥå�����
            (set-dispatched-info! self state)

            ;; persistent-connection��Ȥ����ɤ���Ƚ��
            ;; �����dispatch�塢#f���֤äƤ��Ƥ�¹Ԥ����ߤ���
            (check&set-persistent-connection! self state))

          ;; ǧ��ͭ��ʤ顢ǧ�ڤ�Ԥ�
          (check-authentication! self state)

          ;; ����������ϡ����顼�ɥ�������������˺��ٸƤӽФ���롣
          ;; �����դν������ˤϡ����顼�ɥ������������ʬ������å������

          ;; �᥿�ѿ�����
          (make-cgi-metavariables! self state)

          ;; cgi������ץȤ�¹ԡ������ॢ����Ƚ��ͭ�ꡣ
          (execute-cgi-thunk! self state)

          ;; cgi������ץȤμ¹Է�̤�ѡ�������header��body������
          (parse-cgi-response! self state)

          ;; cgi������ץȤμ¹Է�̤Υѡ������Ƥ��顢
          ;; �ºݤ˥��饤����Ȥ��֤�http�쥹�ݥ󥹤θ�������������롣
          (make-http-response! self state)
          )))

    ;; ���󥰤���
    (tcpcgi-logging self state)

    ;; (��̤��֤�ɬ�פ�����ʤ�)��̤򥯥饤����Ȥ��֤��ƽ�λ
    ;; sigpipe�Ͻ񤭹��⤦�Ȥ��ʤ���Ƚ��Ǥ��ʤ��褦�ǡ��إå��ɤ߹��߻���
    ;; ���Ǥ���Ƥ⡢�ѡ������顼�ˤʤ�ʤ���С����Τޤ�cgi�¹Ԥ��Ԥ��롣
    ;; �����ơ��Ǹ�ν񤭽Ф�����sigpipe�������롣
    (block-sigpipe-error
      (lambda ()
        (send-response! self state))))

  (define (tcpcgi-execute! state)
    ;; �Ȥꤢ�����ꥯ�����Ȱ������
    (execute-one-transaction! state)
    ;; ��³Ū��³��ͭ���ʾ��ϡ����ټ¹Ԥ���
    (when (and
            (not (ref state 'connection-close))
            (ref state 'use-persistent-connection))
      (tcpcgi-execute!
        (make <tcpcgi.state>
          :first-transaction #f ; ����ܰʹߤ�#f
          :server-addr server-addr
          :server-port server-port
          :server-name server-name
          :remote-addr server-addr
          :remote-port remote-port
          :remote-host remote-host
          :https https))))


  ;; ��³���Ƥ������饤����Ȥν����򳫻�
  (with-ignore-sigpipe-handler
    (lambda ()
      (with-error-to-port
        (or
          (cgi-error-port-of self)
          (open-output-file "/dev/null" :buffering :none))
        (lambda ()
          (tcpcgi-execute!
            (make <tcpcgi.state>
              :first-transaction #t ; ����ܤΤ�#t
              :server-addr server-addr
              :server-port server-port
              :server-name server-name
              :remote-addr server-addr
              :remote-port remote-port
              :remote-host remote-host
              :https https)))))))


(provide "tcpcgi")

