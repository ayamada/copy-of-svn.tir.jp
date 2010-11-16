;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$


;;; ToDo : �ᥤ�������ʬ��state�⥸�塼��¦�˰ܤ�
;;; ToDo : state�⥸�塼���̾���ѹ�(transaction?)

;;; ToDo : HEAD�᥽�å��ѥե��륿���Ѱդ���
;;;        HEAD�᥽�åɤǤϡ�����ƥ�Ĥ������֤��ƤϤʤ�ʤ�����
;;;        content-length�ϼºݤ��̤��֤�ɬ�פ����롣
;;;        ��³Ū��³����cgi������ץȤ�HEAD���θ�����˺���Ƥ�����ˡ�
;;;        ���줬���Ǳ�³Ū��³�˰۾郎ȯ�������ǽ�����⤤��
;;;        - HEAD�᥽�åɤ��ɤ�����Ƚ�ꤹ��
;;;        - response-body����content-length��¬�ꤹ��
;;;        - �إå���ʬ�Τ���������
;;;        ���������parsed-header�ǤϤʤ���tcpcgi�⥸�塼�뤬�Ԥ�����


;;; ToDo : apache��clf�ǥ��󥰤���logger����
;;;        �Ĥ��Ǥ�full dump logger����(autoload��Ȥ�)

;;; ToDo : �ǽ�ˡ��֥��󥰤��Ƥ����̤��֤��׷�³���ꡢ
;;;        �Ƽ¹ԥե������ˡ����η�³���Ϥ���ɬ�פ˱����ƻȤäƤ�餦�褦��
;;;        ����С����ˤʤ�
;;;        (�������֤��ͤˤ�������ʬ����ˤ���)
;;;        �����̤�˺��ľ����
;;;        �����������Ȥǡ��ޤ�ư�����֤ˤ��Ƥ��顣


;;; ToDo : ab -k�Ǥκǽ��ʳ��ǤΥ�������̤��ǧ���ơ�
;;;        ����꡼��Ū�ʾ��֤ˤʤäƤ��ʤ������ǧ�����
;;;        (test case�ˤǤ�����롩����)


;;; ToDo : tcpcgi:inner:dispatch�ڤ�tcpcgi/dispatch.scm�������Τ�ľ������


;;; ToDo : nph������ץȤΰ٤ˡ�CGI/1.1->HTTP/1.1�Ѵ�vport����
;;;        ��chunked�ǽ��Ϥ����

;;; note : ư����ϡ�(current-error-port)�ϡ�cgi-error-port�����åȤ����
;;;        ��stderr�˽��Ϥ��줿���Ƥϥ��顼���Ȥ��ƻĤ��

;;; ToDo : �����ʥ��ݸ���ʬ���ľ�����Τǡ�ư���ǧ�򤹤��
;;; ToDo : tcpcgi.supplement��ư���ǧ��Ԥ�����

;;; ToDo : ��ñ�˥��顼�ƥ�ץ졼�Ȥ˰���css���إå����եå�����
;;;        �����ǽ�ʻ��Ȥߤ��󶡤���


;;; ToDo : ���顼ȯ�����˥᡼������Τ��뵡ǽ���ߤ���




;;; ToDo : ����ϡ�apache�Τ褦�ˡ��֤��Ƥ���*.cgi�ե�����˥����������褿�顢
;;;        ���Υե������Ƚ�̤������줬scheme�ե�������ä���
;;;        ���Τޤ��ɤ߹���Ǽ¹Ԥ���褦�ˤ��롩


;;; ToDo : ���饤����Ȥؤν��Ϥ򡢤ʤ�٤���Ψ�ɤ��Ǥ���褦�ˤ�����
;;;        �ʶ���Ū�ˤϡ����饤����Ȥؤ�HTTP/1.1���ϻ���
;;;        �Хåե���󥰥⡼��:none���Ǥ��Ψ���������������뤬��
;;;        :full���ȡ����ޤ��flush���ʤ��ȥ����Ψ��������������
;;;        ����Ȥ⡢Nagle���르�ꥺ�बƯ���ơ���Ψ�ɤ�����Ƥ롩
;;;        â�����ɤ���ˤ��衢nph, filerư��λ��ϡ�:none�ˤ��ʤ������ܡ�



;;; ToDo : auth����

;;; ToDo : rfc2616�򸫤ơ����饤����Ȥ������³��HTTP/1.1�ʤ顢
;;;        ���饤����Ȥ����method��إå���HTTP/1.1���������Ƥ��뤫��
;;;        �����å������������Ƥ��ʤ��ʤ顢Ŭ�ڤʥ��顼���֤��褦�ˤ���


;;; ToDo : ������Ū��DoS�к�
;;;        apache��RLimit�Ϥ�����򿿻�����


;;; ToDo : fastcgi�����ФȤ���ư��Ǥ���褦�ˤ���


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

   ;; #f�ʤ���󥰤��ʤ�
   (log-port :accessor log-port-of
             :init-keyword :log-port
             :init-form (current-error-port))
   ;; #f�ʤ���󥰤��ʤ�
   (logger :accessor logger-of
           :init-keyword :logger
           :init-form tcpcgi:default-logger) ; ������<tcpcgi.state>��������

   ;; #f�ʤ�/dev/null��ή��
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
                           :init-value 65536) ; #f�Բġ�
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

   ;; �ʲ��ϡ����������ݻ��ѥ���å�
   (dispatch-vhost-instance :init-value #f)
   (dispatch-path-instance :init-value #f)
   (dispatch-fallback-instance :init-value #f)
   (error-document-instance :init-value #f)
   ;; ���Υ���åȤΰ�̣
   ;; - #f : ̤�����å�����������
   ;; - #t : �����å�������������̵�����ä���
   ;; - <tcpcgi.dispatch> : �����å��������������������Ѳ�ǽ��


   ;; �����ʥ륭��å��ե饰�����줬#t�ˤʤä��鼡����³�ϹԤ�ʤ���
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
;; ToDo : ref��set!�����̤˥��������Ǥ���褦�ˤ���
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
;; �����ѥ��������᥽�å�
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
      (sys-ctime (sys-time)) ; ToDo : ���Ȥ�Ŭ����ľ����
      (ref state 'request-line)
      (ref state 'response-code)
      (get-header-value "referer" request-header :default '-)
      (get-header-value "user-agent" request-header :default '-)
      )))
;; ****.ne.jp - - [08/Feb/2005:16:40:42 +0900] "GET /banner.png HTTP/1.1" 200 4992 "http://********/index.htm" "Mozilla/4.0 (compatible; MSIE 6.0; Windows 98; .NET CLR 1.1.4322)"



(define (with-timer-handler timeout timeout-thunk thunk)
  ;; ToDo : SIGALRM��Ȥ�ʤ���ˡ��ͤ��롣̵����
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





;;;;;;;;;;;;;;;;;; �������鲼�ϸŤ�










(define (tcpcgi-display-error-document! self response-code)
  (unless (error-document-instance-of self)
    (set!
      (error-document-instance-of self)
      (make <tcpcgi.error-document>
        :ht (apply hash-table 'eqv? (error-document-of self)))))
  (display-error-document (error-document-instance-of self) response-code))


;; response-code�Υ��顼��HTTP/1.1������display����
(define (response-error! self state response-code)
  (define (execute-error-document-thunk thunk)
    (set! (ref state 'dispatched-cgi-target) thunk)
    (and
      thunk
      ;; tcpcgi-main��Ʊ���褦�ʽ�˼¹Ԥ��롣
      ;; �����ϡ�tcpcgi-main������äƤ�����
      (execute-cgi-thunk! self state)
      (parse-cgi/1.1-response self state)
      (make-http-response! self state)
      (send-response! self state)
      ;; ���꤬ȯ��������#f���֤��Ȥ�������ˤʤäƤ��롣
      ;; ToDo : �����ն�ϡ��⤦�������ޡ��Ȥ˽����Ȧ
      ))

  (unless (ref state 'connection-close) ; ��³�Ǥʤ鲿�⤷�ʤ�
    (set! (ref state 'nph) #f) ; ����Ū����nphư��Ȥ��롢���ΤȤ����
    (unless (ref state 'merged-cgi-metavariables)
      ;; ���顼�����ˤ�äƤϡ�merged-cgi-metavariables��̵������
      ;; ����Τǡ��������Ƥ���
      (make-cgi-metavariables! self state))
    (execute-error-document-thunk
      (lambda ()
        (tcpcgi-display-error-document! self response-code)))))



(define (send-response! self state)
  ;; connection-close��#t�ʤ鲿���֤��ʤ�
  (unless (ref state 'connection-close)
    (let ((response-status-line (ref state 'response-status-line))
          (response-header (ref state 'response-header))
          (response-body (ref state 'response-body))
          (response-code (ref state 'response-code))
          )
      ;; response-body������ʤ����̤˽��ϡ�̵���ʤ饨�顼����ϡ���ʸ����ok
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


;; �ʾ�ϡ��Ť�������
;;;;;;;;;;;;;;;;;;;;;





;; ����������current-input-port���ɤ߹��߲�ǽ�ˤʤä����ɤ���Ĵ�٤�
;; #t�ʤ��ɤ߹��߲�ǽ��#f�ʤ饿���ॢ����
;; timeout-sec��#f�ʤ顢�ɤ߹����褦�ˤʤ�ޤ��Ԥ�
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
      ;; �����餯�����饤����Ȥ������Ǥ��줿
      (set! (ref state 'error-instance) e) ; ��¸
      (set! (ref state 'connection-close) #t) ; ��³��λ
      (set!
        (ref state 'internal-description)
        'connection_reset_by_peer)
      #f) ; �۾ｪλ
    (lambda ()
      (and
        ;; check to keep-alive timeout
        (when (and
            (not (= 0 (ref state 'counter))) ; ���Ǥʤ���
            (keep-alive-timeout-of self)) ; timeout�ͤ����ꤵ��Ƥ����
          (or
            (can-read-stdin-until? (keep-alive-timeout-of self)) ; ¬��¹�
            (begin ; �����ॢ���Ȥ�����
              (set! (ref state 'connection-close) #t) ; ��³��λ
              (set! (ref state 'response-code) #f) ; �������֤��ʤ�
              (set! (ref state 'internal-description) 'keep_alive_timeout)
              #f))) ; �۾ｪλ
        ;; ����³�ԡ�
        (and-let* ((keywords
                     (with-timer-handler
                       (request-timeout-of self)
                       (lambda ()
                         (set! (ref state 'connection-close) #t) ; ��³��λ
                         (set! (ref state 'response-code) #f) ; �������֤��ʤ�
                         ;; note : ��rfc2616�ˤ���408���֤��Ƥ��ɤ��餷������
                         ;;        ���֥饦����������Ƚ�Ǥ��Ƥ���뤫��̯
                         (set!
                           (ref state 'internal-description)
                           'request_timeout)
                         #f) ; �۾ｪλ
                       (lambda ()
                         ;; tcpcgi.request��¢��request-body-caching�ϻȤ鷺��
                         ;; �����ǥ���å��夹��褦�ˤ���
                         ;; ��request-header�����ˤ�timeout���������������
                         ;;   request-body�����ˤ�timeout������������ʤ���
                         (receive (keywords error-desc)
                           (stdin->http-request
                             :convert-incomplete-string-uri
                             (convert-incomplete-string-uri-of self)
                             :request-body-caching #f) ; �����Ǽ�����
                           (if error-desc
                             (begin
                               (set! (ref state 'connection-close) #t)
                               (set! (ref state 'response-code) 400)
                               (set! (ref state 'request-keywords) keywords)
                               (set!
                                 (ref state 'internal-description)
                                 error-desc)
                               #f) ; �۾ｪλ
                             keywords)))))) ; ���ｪλ
          ;; request-body-port�μ�����state�ؤ�ȿ��
          (set!
            (ref state 'request-keywords)
            (list*
              :request-body-port (stdin->request-body-port
                                   (get-keyword :request-header keywords)
                                   (request-body-on-memory-limit-size-of self)
                                   (temporary-file-prefix-of self))
              keywords))
          #t))))) ; ���ｪλ





(define (tcpcgi:inner:dispatch self state)
  (let* ((request-keywords (ref state 'request-keywords))
         ;; ɬ�פ��ͤ�Ф��Ƥ���
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
         ;; �ǽ�ˡ�vhost����Ĵ�٤Ƥ���
         (dispatched-by-vhost (dispatch-from-vhost
                                (get-dispatch-vhost-instance self)
                                target-host
                                target-path))
         ;; vhost��match������path��match���ʤ��ä����˻Ȥ���
         (fallback-server-name (if (string? dispatched-by-vhost)
                                 dispatched-by-vhost
                                 fallback-host))
         ;; ����dispatch��Ԥ�
         (dispatched-list (if (list? dispatched-by-vhost)
                            dispatched-by-vhost
                            (or
                              (dispatch-from-path
                                (get-dispatch-path-instance self)
                                target-path)
                              (dispatch-from-fallback
                                (get-dispatch-fallback-instance self))
                              '())))
         ;; dispatched-list�ϡ��ʲ��Τɤ줫�ˤʤ�
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
      ;; 404���֤��ƽ�λ��
      (set! (ref state 'response-code) 404)
      (set! (ref state 'internal-description) 'dispatch-failed)
      #f))) ; �ǥ����ѥå������ʤ顢���ͤȤ���undef���֤�






(define (tcpcgi:inner:metavariables self state)
  (let* ((plain-path-info (ref state 'dispatched-plain-path-info))
         (path-info (and
                      plain-path-info
                      (uri-decode-string plain-path-info :cgi-decode #t)))
         ;; note : path-info��¸�ߤ���ʤ顢path-info�Ͼ��/�ǻϤޤ롣
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

               ;; ToDo : ���������
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
             ;; sigpipe���ä�
             (set! (ref state 'connection-close) #t) ; ��³��
             (set!
               (ref state 'internal-description)
               'connection_reset_by_peer)
             (set! (ref state 'error-instance) e))
            ((#/^unhandled\ssignal\s/ message)
             ;; ����¾�Υ����ʥ���ä���¿ʬ����λ�ι�ޡ�
             ;; ToDo : �����ʥ�μ�����ǧ����
             (set! (ref state 'connection-close) #t) ; ��³��
             (set! (ref state 'internal-description) 'signal_received)
             (set! (ref state 'error-instance) e))
            (else
              ;; ����ʳ��Υ��顼�ʤ顢���顼���˻Ĥ�
              (set! (ref state 'response-code) 500)
              (set! (ref state 'connection-close) #t) ; ��³��
              (set!
                (ref state 'internal-description)
                'cgi_script_error_exception)
              (set! (ref state 'error-instance) e)
              (report-error e))) ; ���顼���Ƥ򥨥顼���˽��Ϥ���
          #f))
      thunk))

  (let* (
         (second-flag (get-optional opts #f)) ; �ٱ�¹Լ»ܥե饰
         (executee (ref state 'dispatched-executee))
         (lazy-execute-flag (lazy-execute? executee)) ; �ٱ�¹ԥ����ץե饰
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
                   ;; �ٱ�¹Է��ϥ����ॢ���Ȥ⥨�顼�㳰�ⵯ����ʤ��Τǡ�
                   ;; guard̵���˼¹Ԥ����ɤ�(�᥿�ѿ�����ɬ��)
                   ;; ToDo : guarded-execute��ľ��Ū�Ǥʤ�(������)��ľ����
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
                       (guarded-execute lazy-execute) ; �ٱ�¹Ԥ���
                       #t) ; ���⤷�ʤ�
                     (if lazy-execute-flag
                       (execute executee) ; �ٱ�¹Ԥ����˥����Ѿ������
                       (guarded-execute execute)))) ; �¹Ԥ��Ʒ�̤�����
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
    #t) ; ���#t���֤�

  (cond
    ;; ���餫�θ����Ǵ�����³�Ǥ����ꤷ�Ƥ������Connection: close
    ((ref state 'connection-close) (closing))
    ;; ����ˤ�äƱ�³Ū��³��Ȥ�ʤ�����Connection: close
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
            ;; ���饤����Ȥ�Connection: close�����äƤ������ϡ�
            ;; Connection: close�򤳤ä����֤�
            ((#/close/ connection-string) (closing))
            ;; ���饤����Ȥ�Connection: Keep-Alive�����äƤ������ϡ�
            ;; Connection: Keep-Alive�򤳤ä����֤�
            ((string-ci=? "Keep-Alive" connection-string)
             (set! (ref state 'response-connection) "Keep-Alive")
             #t) ; ���#t���֤�
            ;; ���饤����Ȥ�HTTP/1.1�λ��ϡ����⤷�ʤ��Ǥ��ΤޤޤȤ���
            ;; �ʲ��⤷�ʤ��Ჿ����鷺�˱�³Ū��³��Ȥ���
            ((string=? "HTTP/1.1"
                       (get-keyword :request-protocol request-keywords))
             #t) ; ���#t���֤�
            ;; �����Ǥʤ��ʤ顢Connection: close
            (else (closing))))
        ;; �ѥ�᡼�������˼��Ԥ�������Connection: close
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
                ;; ToDo : HEAD�᥽�åɤ��ɤ����ǵ�ư���ѹ�����
                ;;        ����method�ˤ�äƤ�connection����Ū��
                ;;        close�ˤ���ɬ�פΤ����Τ����뤫���Τ�ʤ�
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





;;;; �����ޤǳ�ǧ��λ

(define (tcpcgi:inner:response-code->http/1.1 self state)
  (let ((response-code (ref state 'response-code))
        (response-keywords (or (ref state 'response-keywords) '()))
        )
    ;; �쥹�ݥ󥹥����ɤ˱������쥹�ݥ󥹤����������֤�
    ;;;;
    #f))




(define (tcpcgi:inner:lazy-execute self state)
  (tcpcgi:inner:execute self state #t))



(define (tcpcgi:inner:send-http/1.1 self state)
  ;; ����ͤ����˽��Ϥ���Τ�
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
  ;; log-port��cgi-error-port��mode����Ū��:line�ˤ���
  (define (log-port-with-buffering thunk)
    (with-port-buffering
      (log-port-of self)
      :line
      (lambda ()
        (with-port-buffering
          (cgi-error-port-of self)
          :line
          thunk))))

  ;; ���Ū�˥����ʥ뤫���ݸ�
  (define (protection-from-signal thunk)
    ;; SIGPIPE, SIGINT, SIGTERM, SIGHUP�����ݸ��
    (define (signal-handler s)
      (set! (ref self 'signaled) #t))
    ;; ToDo : race condition�к�
    (let loop ((left (list SIGPIPE SIGINT SIGTERM SIGHUP)))
      (with-signal-handler
        (car left)
        signal-handler
        (if (null? (cdr left))
          thunk
          (lambda ()
            (loop (cdr left)))))))

  ;; HTTP�ȥ�󥶥��������ʬ�����
  (define (execute-one-transaction! state)
    ;; sigpipe�Υ����ʥ뼫�Τ�̵�������Ƥ��뤬��sigpipe�ε����ä�port��
    ;; write���褦�Ȥ���ȥ��顼��������Τǡ�������ɤ�ɬ�פ�����
    (define (block-sigpipe-error thunk)
      (with-error-handler
        (lambda (e)
          ;; ���ˤ��Ƥ⡢��̿Ū�ʤΤǡ����λ����ǽ�λ�ν����򤹤�
          (set! (ref state 'error-instance) e) ; ��Ͽ����
          (set! (ref state 'connection-close) #t) ; ��³��
          (let1 message (ref e 'message)
            (cond
              ((and
                 (eq? <system-error> (class-of e))
                 ;; "write failed on #<oport (stdout) 0x8083e40>: Broken pipe"
                 (#/^write\sfailed\son\s/ message)
                 )
               ;; sigpipe���ä�
               (set! (ref state 'internal-description) 'client_disconnected))
              ((#/^unhandled\ssignal\s/ message)
               ;; ����¾�Υ����ʥ���ä���¿ʬ����λ�ι�ޡ�
               (set! (ref state 'internal-description) 'signal_received))
              (else
                ;; ����ʳ��Υ��顼�ʤ顢���顼���˻Ĥ�
                (report-error e)))
            #f))
        thunk))

    (block-sigpipe-error
      (lambda ()
        ;; ����Ǽ��Ԥ����顢��ν����ϥѥ�����
        (and
          ;; ���饤����Ȥ�http�ꥯ�����Ȥ�����������
          (tcpcgi:inner:receive-request self state)
          ;; �ꥯ�����Ȥ�host��path��Ȥ����ǥ����ѥå���Ԥ�
          (tcpcgi:inner:dispatch self state)
          ;; �ꥯ�����Ȥ���᥿�ѿ�����
          (tcpcgi:inner:metavariables self state)
          ;; �¹Ԥ���CGI/1.1�����η�̤������롢�ޤ��ϡ�
          ;; response-code�ȡ�������տ魯������������
          (tcpcgi:inner:execute self state)
          ;; ��³Ū��³��ͭ�����ɤ�����Ƚ��
          (tcpcgi:inner:check-persistent-connection self state)
          ;; CGI/1.1�����η�̤���HTTP/1.1�����ν��Ϥ���������
          (tcpcgi:inner:cgi/1.1->http/1.1 self state)
          )))
    ;; ���󥰤���
    (tcpcgi:inner:logging self state)
    ;; HTTP/1.1�����η��/���顼���֤����ޤ����ٱ�¹Ԥ���
    (block-sigpipe-error
      (lambda ()
        (cond
          ;; response-code������ʤ顢����˱��������������������֤�
          ;; response-keywords��Ȥ�
          ;; ToDo : 200�����äƤ�����ˡ�������Ȥޤ�����
          ;; ¾�Υ���åȤ򸫤�Ƚ�ꤹ��褦�ˤ����
          ((ref state 'response-code)
           (tcpcgi:inner:response-code->http/1.1 self state))
          ;; �ٱ�¹ԥ����פʤ顢�ٱ�¹Ԥ�Ԥ�
          ((ref state 'lazy-execute) (tcpcgi:inner:lazy-execute self state))
          ;; cgi/1.1-response������ʤ顢�쥹�ݥ󥹤��֤�
          ;; ToDo : ¾�Υ���åȤ�Ƚ�Ǥ��٤���������
          ((ref state 'cgi/1.1-response)
           (tcpcgi:inner:send-http/1.1 self state))
          (else #f)))))

  (define (tcpcgi-execute! state counter)
    ;; �Ȥꤢ�����ꥯ�����Ȱ������
    (execute-one-transaction! state)
    ;; ��³Ū��³��ͭ���ʾ��ϡ����ټ¹Ԥ���
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
          #t)))) ; �������¹Ԥ�����ɬ�פʤ顢���󥰤��Ƥ��ɤ�


  ;; ��³���Ƥ������饤����Ȥν����򳫻�
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

