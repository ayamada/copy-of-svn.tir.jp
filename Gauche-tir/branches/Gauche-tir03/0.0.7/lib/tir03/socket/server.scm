;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; socket server module

;;; WARN: ���Υ⥸�塼���obsolete�Ǥ���
;;;       ���Υ⥸�塼��ο��������Τ�tir03.socket.cth-server�ˤʤ�ͽ��Ǥ���

;;; ToDo: selected-ccid�򥭥塼�ˤ���

;;; ToDo: pause�ΰ����ɲäȡ������ơ��֥��ɲ�
;;; ToDo: ������ʬ��socket/partcont-server.scm���ˤǤ�ʬ�䤷��
;;;       read���˷�³�ݻ�������ʬ�����򤳤��˻Ĥ�

;;; ToDo: DoS�к�

;;; ToDo: reader��buffer��ʸ����ǤϤʤ���ʸ�����list�Ȥ����ݻ�����
;;;       (read-line��read-block�ˤĤ��Ƥϡ�����������Ψ���ɤ���)

;;; ToDo: ���祯�饤�������³������

;;; ToDo: graceful shutdown(���饤����Ȥ����ƽ����򽪤���ޤ��Ԥ�)���������

;;; ToDo: ���ä���ư���褦�ʤ顢�⥸�塼��̾���Ѥ��ơ�
;;;       ñ�Υ⥸�塼��Ȥ��Ƹ������褦��

;;; ToDo: ����꡼���Υ����å�

;;; ���Υ⥸�塼��ϡ�gauche.net��gauche.selector�ˤ�ä���³���Ԥ�������
;;; call/cc�ˤ�äƥ��롼���󲽤��줿socket server��¸����롣
;;; client-handler�ϡ�read���˥֥�å��������ϡ����饤����Ȥ���
;;; ���Ϥ��Ϥ��ޤǤ��Ԥ����졢���δ֤ˡ�¾�Υ��饤����Ȥν��������¹Ԥ���롣
;;; client-handler�����ｪλ/���顼��λ�����ʤ顢��ưŪ���̿������Ǥ���롣
;;; ����client-handler���֤��ͤϼΤƤ��롣
;;; read���ˤ��ä����³���ݻ�����¾�Υ��롼����˽������Ϥ����ꤷ�Ƥ��롣
;;; ���ΰ١�socket����read����ݤˤϡ���³��extent��
;;; �ڤ�ʤ��褦�ʰ��֤ǤΤ�read����褦�˵����դ��ʤ��ƤϤʤ�ʤ���
;;; (�����̾�ϡ��ռ����ʤ��Ƥ�����פ�Ȧ��)
;;; ToDo: ���������򡢤�äȤ����Ƚ񤯡�

;;; note: socket-server:start�����Ǥϡ��ʲ������Ƥ�stderr��ή����
;;;       - :verbose-mode��#t���Ϥ������ΥǥХå���
;;;       - client-handler�����顼���ꤲ�ƽ�λ��������stack trace

;;; note: �Ƽ亮���ʥ��������ư��
;;; - SIGTERM : ���߼¹���λҥץ��������潪λ/�����ʥݥ���Ȥ�server��λ��
;;; - SIGHUP : �����ʥݥ���Ȥޤ���Ƥ���server��λ��
;;; - SIGINT : �����ʥݥ���Ȥޤ���Ƥ���server��λ��
;;; - SIGPIPE : ���̵��(�ɤ�socket��������SIGPIPE���褿�Τ�Ƚ�̤Ǥ��ʤ���)��
;;;             client-handler�ϡ�port����#<eof>���ɤ���ꡢ
;;;             write���˥��顼�㳰���Ф뤫�ɤ����ǡ���³��̵ͭ��Ƚ�Ǥ������

;;; ���Ѿ�����:
;;; - ��Ĵ����å�ư��ΰ١���ս�ǥǥåɥ�å���̵�¥롼������ȯ������ȡ�
;;;   ���������Τ���ߤ���Τ���ա�
;;;   ɬ����λ�����������褦�ˤ������
;;; - �����å��̿��λ��;塢�̿�����������Ǥ�����ǽ�������롣
;;;   ����Ū�ˤϡ�read������̤�#<eof>���ä��ꡢ�����ʥ�(SIGPIPE)��ή�줿�ꡢ
;;;   write����error�㳰��ȯ�������ǽ�������롣
;;;   ��ǽ�ʸ¤ꡢ�����Υȥ�󥶥������ΰ�������ݤ��褦�Ȥ���ɬ�פ����롣



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
     :init-value #f) ; bind/listen����sockaddr�����
   (select-interval
     :accessor select-interval-of
     :init-keyword :select-interval
     :init-value 1000000) ; default 1 sec

   ;; ����ϡ����饤����Ȥ�����³�����ä����ˡ��ʲ��Τ褦��proc����ꤹ�롣
   ;; (proc ccid sockaddr socket-reader socket-writer)
   ;; ccid�ϡ���³���Ϣ�֤ǿ�������͡�
   ;; socket-reader�ϡ����(ToDo: ������񤭤ޤ��礦)��
   ;; socket-writer�ϡ����(ToDo: ������񤭤ޤ��礦)��
   ;; ����proc����λ/���顼���ꤲ��ȡ�socket�ϼ�ưŪ���Ĥ����롣
   ;; ������proc�ϡ�ɬ����λ���������̵�¥롼�פ����ƤϤ����ʤ���
   ;; ��socket�������塢�ɤ��ʳ�����³�����Ǥ���뤫��ʬ����ʤ��١�
   ;;   �嵭��port���Ф���read/write����ݤˤ�ɬ����#<eof>���֤äƤ����ꡢ
   ;;   ����ޤǤ���read�Ǥ��ʤ��ä��ꡢwrite���褦�Ȥ����饨�顼��ȯ�������ꡢ
   ;;   SIGPIPE��ή�줿�ꡢ�Ȥ��ä����������ʤ��ƤϤʤ�ʤ���
   (client-handler
     :accessor client-handler-of
     :init-keyword :client-handler
     :init-value #f)
   ;; �����Ф�shutdownľ���˸ƤФ��եå���
   (shutdown-hook
     :accessor shutdown-hook-of
     :init-keyword :shutdown-hook
     :init-value #f)

   ;; selector��ȿ��������timeout�������˸ƤФ��thunk����ꤹ�롣
   ;; ����ϡ���ˡ��ƥ��饤����ȤΥ����ॢ����Ƚ������Ԥ��ݤ˻��ѤǤ��롣
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
   (client-counter ; client���unique��id���ͤ�ȯ�Ԥ���٤Υ����󥿡�
     :accessor client-counter-of
     :init-value 0)
   (session-table
     :accessor session-table-of
     :init-form (make-hash-table 'eqv?)) ; ��󿷵�����������Τǡ�:init-form��
   ;; ��key��ccid(client-counter�ο���)��
   (shutdown-request ; ���줬#t�ʤ顢server�ν�λ�򳫻Ϥ��롣
     :accessor shutdown-request-of
     :init-value #f)
   (selected-ccid ; select��˻��ꤵ��롢���˼¹Ԥ��٤�ccid��
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

(define socket-server (make-parameter #f)) ; method��self���ά��������
(define in-client-handler? (make-parameter #f)) ; �����ʥ��������ư��Ƚ����
(define return/client-handler-cont (make-parameter #f))
(define client-session (make-parameter #f))
(define client-ccid (make-parameter #f))

;; �Ǿ����꡼���ÿ�������˵��Ƥ���
(define pause-sleeptime-default
  (if (zero? (values-ref (sys-gettimeofday) 1))
    0 ; os��gettimeofday�򥵥ݡ��Ȥ��Ƥ��ʤ��������0�Ȥ���
    (let* ((time1 (get-floatepoch))
           (time2 (get-floatepoch))
           )
      (* (- time2 time1) 4)))) ; Ŭ���ˡ��ƶ��ξ��ʤ��ϰϤǷ��ꤹ��

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
      select-handler ; (cut list-ref <> 4) ; �����ʹߤ�set!�Բġ�
      ;; ToDo: ���Ǥ������Ƥ�����keywords�Τޤ޴�������Ȥ����⤦�����ͤ�ľ��
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
      ;; ToDo: ����letrec�ϥ���꡼���θ����ˤʤ뤫���Τ�ʤ�/���Ȥǳ�ǧ����
      (letrec ((select-handler
                 (lambda (in flag)
                   ;; wakeup-epoch��Ĵ�٤�
                   (let1 waittime (- (session->wakeup-epoch session)
                                     (get-floatepoch))
                     (if (positive? waittime)
                       (begin
                         ;; ������waittime�δ֤��������Υ����åȤ�select����
                         ;; ̵�뤵���ɬ�פ�����ġ�
                         ;; �ɤ�����Ф�����
                         ;; ���̤˹ͤ���ʤ顢selector������Ū����Ͽ��
                         ;; �������waittime�������������������ġġ�
                         ;; ToDo: ������ͤ��롣
                         )
                       (begin
                         ;; ���˼¹Ԥ���ccid����Ͽ
                         (verbosef
                           self "received data ccid ~s" ccid)
                         (set! (selected-ccid-of self) ccid))))))
               (session (make-session
                          :client-socket client-socket
                          :select-handler select-handler
                          ))
               )
        (verbosef self "accepted to ~s (ccid=~s)" client-socket ccid)
        ;; select-handler��selector����Ͽ
        (selector-add! (selector-of self)
                       (socket-input-port client-socket :buffering :modest)
                       select-handler
                       '(r))
        ;; session-table����Ͽ
        (hash-table-put! (session-table-of self) ccid session)
        ;; selected-ccid��Ͽ
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
    ;; �Ƽ凉��åȤ�clean up
    (socket-server-clean-up self)
    ;; accept-handler��selector������
    (selector-add!
      (selector-of self)
      (socket-fd server-socket)
      (make-server-accept-handler self server-socket)
      '(r))
    ;; server-socket��¸
    (set! (server-socket-of self) server-socket)
    (verbosef self "socket-server opened.")))


;;; --------

;;; ���٤�read-block���륵����
(define *bufsize* 4096)
;;; eof
(define *eof* (read-from-string ""))


;;; (value buffer eof?)���֤���
;;; - buffer�Ͼ��ʸ����eof���ˤ�""�����롣
;;; - eof?�ϡ�buffer�θ夬eof���ɤ����򼨤���
(define (reader:load-buffer! sock-in)
  ;; ToDo: �����Ⱥ�Ŭ������ǽ��Ȧ�ʤΤǡ���Ŭ�������
  ;;       - (client-sock-in)��(�ޤ�)�ɤ�ʤ��ʤ顢old-buffer�򤽤Τޤ��֤�
  ;;       - old-buffer��""�ʤ顢old-buffer��string-append�ˤϴޤ�ʤ�
  ;;       - ���Ǥ���Ĥ����ʤ顢string-append�ϼ¹Ԥ��ʤ�
  ;;       - loop��������client-socket��ͭ���������å���������������
  ;;       - buffer�ϡ�ʸ����ǤϤʤ���ʸ�����list�Ȥ��ư����褦�ˤ���
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
      ;; ���顼�������ä����������礬������ʤ��褦�ˡ���¸���Ƥ���
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
                   ;; SIGTERM���ϡ�stack dump��ɽ�����ʤ�
                   (verbosef self "caught SIGTERM")
                   (client-terminate))
                  (else
                    (verbosef self "client-handler error (ccid=~s)" ccid)
                    (report-error e)
                    (client-terminate)))
          (let1 pc (let/cc return
                     ;; client-handler�¹���ϥե饰��Ω�Ƥ�
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
                     ;; �Ǹ�ޤǼ¹Ԥ��줿�Τǡ�disconnect/finalize����
                     (verbosef self "client-handler finished (ccid=~s)" ccid)
                     (client-terminate)
                     ;; �Ǹ�ޤǼ¹ԤǤ����ڤȤ��ơ�#f���֤�
                     #f)
            (when pc
              (set! (session->cont session) pc)))))

      (reset/pc
        (doit)))))




;;; ���ƤΥ��饤����Ȥ�disconnect���Ƥ����³��¹Ԥ��롣
;;; ���줬����³��̵���ʤ�ޤ�³�����롣
;;; ToDo: �����graceful�ȤϸƤ٤ʤ��Τǡ�̾�����Ѥ����
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
                    (execute-client-handler self ccid session))) ; ���¹�
                (set! (selected-ccid-of self) #f))) ; selected-ccid�򥯥ꥢ
            (if (shutdown-request-of self)
              (let1 shutdown-hook (shutdown-hook-of self)
                (when shutdown-hook
                  (shutdown-hook))
                (all-client-graceful-disconnect self)) ; ���θ塢close�����
              (begin
                (and
                  callee
                  (eqv? old-c c) ; select��timeout������#t�ˤʤ�(Ȧ)
                  ;; note: �����Ǥϡ������ơ����顼�ݸ�Ϥ��ʤ���
                  ;;       (���顼ȯ��=�����ФϤ��Τޤ޽�λ�Ȥ���)
                  (callee))
                (loop c)))))))))


;;; --------


(define (remove-accept-handler self)
  (let1 server-socket (server-socket-of self)
    (when (eq? 'listening (socket-status server-socket))
      ;; �ޤ��ǽ�ˡ�<sockaddr-un>�ʤ顢socket-file����
      ;; (race condition�ɻߤΰ٤ˡ���˺������)
      (let1 sockaddr (sockaddr-of self)
        (when (eq? 'unix (sockaddr-family sockaddr))
          (let1 sockfile (sockaddr-name sockaddr)
            (verbosef self "unlink to ~s" sockfile)
            (ignore-error (sys-unlink sockfile)))))
      ;; accept-handler�򥯥ꥢ(fd�δط��塢disconnect�������˥��ꥢ����)
      (selector-delete! (selector-of self) (socket-fd server-socket) #f #f)
      ;; server-socket���Ĥ�����������³������ʤ��褦�ˤ���
      (socket-disconnect/ignore-error server-socket))))




;;; WARN: socket��disconnect�ϡ�(ɬ�פʤ�)��˼����ǹԤäƤ�������
;;; WARN: ��³��Ǹ�ޤǼ¹Ԥ�����⡢(ɬ�פʤ�)��˼����ǹԤäƤ�������
(define (client-remove self ccid)
  (verbosef self "remove to ccid ~s" ccid)
  (let1 session (hash-table-get (session-table-of self) ccid #f)
    (if (not session)
      (verbosef self "cannot found ccid ~s in session-table" ccid)
      (begin
        ;(verbosef self "target: ~s" session)
        ;; selector�������
        (selector-delete!
          (selector-of self)
          #f
          (session->select-handler session)
          #f)
        ;; session-table����쥳���ɤ�ä�
        (hash-table-delete! (session-table-of self) ccid)
        (verbosef self "ccid ~s removed." ccid)))))




;;; socket-server-close�Ǥϡ�client�η�³���˴�����롣
;;; client�η�³��¹Ԥ��������ϡ����all-client-graceful-disconnect��Ƥ֡�
(define (socket-server-close self)
  (verbosef self "socket-server closing...")
  (unless (server-socket-of self)
    (error "not opened"))
  ;; accept-handler����ߤ���
  (remove-accept-handler self)
  ;; �ĤäƤ���client-socket������disconnect
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


;;; socket�ؤ�bind/listen/accept�򳫻Ϥ��롣
(define-method socket-server:start ((self <socket-server>))
  (parameterize ((socket-server self))
    (socket-server-open self)
    (socket-server-main self)
    (socket-server-close self)))


;;; ư��Ƥ���socket-server���Ф��ơ�accept-handler����������׵�����롣
;;; �ºݤ���ߤϡ������ʥݥ���Ȥ˰�ư���Ƥ���¹Ԥ���롣
;;; (�����ˤ���ߤ��ʤ�)
(define-method socket-server:shutdown ((self <socket-server>))
  (if (socket-server-opened? self)
    (begin
      (verbosef self "socket-server shutdown request accepted.")
      (remove-accept-handler self)
      (set! (shutdown-request-of self) #t))
    (error "not started")))


;;; ���ꤵ�줿ccid�Υ����åȤ����Ǥ����������饤����Ȥη�³���˴����롣
;;; opt-keep-cont��#t����ꤹ����ǡ������åȤΤ����Ǥ�����³��Ĥ������ǽ��
;;; note: client-handler�������鼫ʬ���Ȥ�ccid���Ф��ƸƤ�����ϡ�
;;;       ��³�Ͼ���˴�����ʤ���
;;;       (���λ���ˡ�Ϥ��ޤ�侩����ʤ���
;;;        client-handler������˽�λ����С������åȤϼ�ưŪ���Ĥ�����)
(define-method socket-server:client-disconnect ((self <socket-server>)
                                                (ccid <integer>)
                                                . opt-keep-cont)
  (let1 session (hash-table-get (session-table-of self) ccid #f)
    (if (not session)
      (verbosef self "cannot found ccid ~s in session-table" ccid)
      (begin
        (socket-disconnect/ignore-error (session->client-socket session))
        ;; keep-cont��#t�λ��ϡ�disconnect������ǡ�����select����select���졢
        ;; ���κݤ˷�³���ƤФ�롣
        ;; keep-cont��#f�λ��ϡ�������(��³��ޤ�)���饤����Ⱦ�����˴������
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


;;; client-handler�⤫�顢�̤Υ��롼�����������Ϥ���
;;; �����Ȥ����ÿ�(float��ok)���Ϥ��ȡ������ÿ����в᤹��ޤǤ�
;;; pause��ƤӽФ������롼����ˤ����椬��äƤ��ʤ���
;;; ��Ĵ����å���Ǥϰ°פ�sleep�Ǥ��ʤ��١������sleep�����ѤȤ��ƻȤ���
(define (socket-server:pause . opt-floatepoch)
  ;; ToDo: �ͤ�����̡����ΤȤ���ϡ�floatepoch���ץ�����
  ;;       ���Ū��̵���ˤ�����ˤ���
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


