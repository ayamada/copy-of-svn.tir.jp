;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; "socket server that work with coroutine" module

;;; ToDo: selected-csid-queue��slept-csid-alist�ϡ�alist�ǤϤʤ�
;;;       hash-table��������Ψ���ɤ������Τ�ʤ�

;;; ToDo: ��¤��ͤ�ľ��ɬ�פ�����褦�ʵ������롣
;;; - ���ߡ����Ƥ�csid����ĥ��饤����Ȥϡ�
;;;   selected-csid-queue��selector-timeout-alist(��selector)��slept-csid-alist
;;;   �Τ����줫��Ĥ���Ͽ�������֤ˤ��롣
;;;   �����Ϸ褷�ƽ�ʣ���ʤ��Τǡ���̯�˸�Ψ��������
;;;   ��������selected-csid-queue�Ͻ�������פǤ��ꡢ
;;;   selector-timeout-alist��slept-csid-alist�ϸ����������פʤΤǡ�
;;;   ���ˤ������̵���ʤ褦�ˤ�פ��롣
;;;   ������̵�����ɤ��������Ȥǹͤ��롣

;;; note: selector¦���鸫��ȡ��ʲ��Τ褦�ʻ��ͤˤʤ�(Ȧ)
;;; - �ǽ�ϡ�server-accept-handler������selector����Ͽ����Ƥ��ꡢ
;;;   select��timeout�ͤˤ�#f�����ꤵ�줿���֤�select����롣
;;; - client����³����ȡ�client-handler���ƤӽФ���뤬��
;;;   ���λ����ǤϤޤ�selector��client-socket�δƻ���ɲä���ʤ���
;;;   client-handler�⤫��ssc:wait-data-from-socket���ƤӽФ��줿
;;;   �ʳ��ǤΤߡ�selector�˴ƻ뤬�ɲä���롣
;;; - ssc:wait-data-from-socket�Ǵƻ뤬�ɲä��줿���select���줿�顢
;;;   �����ʳ���selector������®���ƻ�����롣
;;; ToDo: �����Υ������ϡ����λ��ͤ��̤�ˤʤäƤ��ʤ��Ȼפ��롣
;;;       (���饤����Ȥ���³�����餹��selector����Ͽ���Ƥ��ޤ�)
;;;       ľ��ɬ�פ����롣

;;; ToDo: ������³���csid�ΰ������������method���󶡤���
;;;       (session-table����keys�Ǽ��Ф���ok)

;;; ToDo: socket���������(�֥�å����ʤ��褦��)�ǡ������ɤ߽Ф��٤�
;;;       �桼�ƥ���ƥ��ؿ���ɬ��(���Ȥǽ�)

;;; ToDo: DoS�к�
;;; ToDo: ���祯�饤�������³������
;;; ToDo: ���ä���ư���褦�ʤ顢�⥸�塼��̾���Ѥ��ơ�
;;;       ñ�Υ⥸�塼��Ȥ��Ƹ������褦��
;;; ToDo: ����꡼���Υ����å�
;;;       (httpd���������ab�򤫤�����������̤��Ѳ��򸫤�)

;;; ���Υ⥸�塼��ϡ�gauche.net��gauche.selector�ˤ�ä���³���Ԥ�������
;;; partcont.scm�ˤ�äƶ�Ĵ����åɤȤ���ư���socket server��¸����롣
;;;
;;; ���������client-handler�ˤϡ�����socket port���Ϥ����١�
;;; ���饤����ȥϥ�ɥ�ϡ�read���˥֥�å����Ƥ��ޤ�ʤ��褦��
;;; ��դ���ɬ�פ����롣
;;; �ޤ���SIGPIPE�ϥǥե���Ȥ�̵�뤵���褦�ˤʤäƤ��뤬��
;;; socket port�ؤ��ɤ߽񤭤�EOF�䥨�顼���ꤲ��������Ф��ơ�
;;; �ä��н�Ϥ��Ƥ��ʤ��Τǡ�I/O�ΰ�����ɬ�����Ի��λ���ͤ���ɬ�פ����롣
;;;
;;; ��ĤΥ��饤����ȥϥ�ɥ餫��¾�Υ��饤����ȥϥ�ɥ��������Ϥ����ꡢ
;;; sleep��Ԥ��������ˤϡ�ssc:sleep��Ȥ�����
;;; ��Ĵ����åɤ������塢���Τޤ�sys-sleep����Ȥ���
;;; ���������Τ���ߤ��Ƥ��ޤ��١��������ˤ���դ������


;;; note: pcss:start�����Ǥϡ��ʲ������Ƥ�stderr��ή����
;;; - :verbose-mode��#t���Ϥ������ΥǥХå���
;;; - client-handler�����顼���ꤲ�ƽ�λ��������stack trace

;;; note: �Ƽ亮���ʥ��������ư��
;;; - SIGTERM : ���߼¹����client-handler�����潪λ��
;;;             �����ʥݥ���Ȥ���Ƥ���server�⽪λ��
;;;             �ĤäƤ����³������֤�¾�Υ��饤����ȥϥ�ɥ��
;;;             ³�Ԥ���ʤ���
;;; - SIGHUP : ���߼¹����client-handler��ssc:pause��ssc:sleep����뤫��
;;;            �����˼¹Դ�λ����ޤǤϼ¹Ԥ���³���롣
;;;            ����ʳ���SIGTERM��Ʊ����
;;; - SIGINT : SIGHUP��Ʊ����
;;; - SIGPIPE : ���̵��(�ɤ�socket��������SIGPIPE���褿�Τ�Ƚ�̤Ǥ��ʤ���)��
;;;             client-handler�ϡ�port����#<eof>���ɤ���ꡢ
;;;             write���˥��顼�㳰���Ф뤫�ɤ����ǡ���³��̵ͭ��Ƚ�Ǥ������

;;; ToDo: graceful-shutdown�򥵥ݡ��Ȥ����Τǡ�
;;;       �����ʥ�γ�����Ƥ��ѹ����Ƥ⤤������
;;;       (��������apache��graceful��restart���������ä���graceful��
;;;        shutdown�ʤΤǡ�SIGHUP��graceful�������Ƥ�ΤϤ�������
;;;        �Ƶ�ư����Ԥ���Ƥ⺤��褦�ʵ�������ġĤȤϸ�����
;;;        ���Υ⥸�塼���Ȥä�graceful restart���������Τ˳ڤʤ褦�ˡ�
;;;        SIGHUP��graceful-shutdown�������Ƥ��ɤ��ΤϳΤ��˻פ���)

;;; ���Ѿ�����:
;;; - socket��read���ˤϡ��褷�ƥ֥�å����ʤ��褦����դ������
;;; - ��Ĵ����å�ư��ΰ١���ս�ǥǥåɥ�å���̵�¥롼������ȯ������ȡ�
;;;   ���������Τ���ߤ���Τ���ա�
;;;   ɬ����λ�����������褦�ˤ������
;;;   �ޤ����֥�å����Ƥ��ޤ��褦�ʸƤӽФ���������ˤ���դ������
;;; - Ʊ�ͤˡ�sleepư��κݤˤ�ɬ��ssc:sleep��Ȥ���
;;; - �����å��̿��λ��;塢�̿�����������Ǥ�����ǽ�������롣
;;;   ����Ū�ˤϡ�read������̤�#<eof>���ä��ꡢ
;;;   write����error�㳰��ȯ�������ǽ�������롣
;;;   ��ǽ�ʸ¤ꡢ�����Υȥ�󥶥������ΰ�������ݤ��褦�Ȥ���ɬ�פ����롣
;;; - client-handler��³�����Ǹ�ޤǼ¹Ԥ�����ä��ꡢ���顼�㳰��
;;;   �ꤲ�����ˤϡ����饤����ȤȤΥ��ͥ������ϼ�ưŪ�����Ǥ���롣



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
    ;; - make�����ϲ����������ʬ�򸫤����

    ;; * ssc:server-�Ȥ���prefix���Ĥ��Ƥ����Τϡ������Ф�����method��
    ssc:server-start
    ;; (ssc:start object-of-socket-server-coroutine)
    ;; - �����åȤ��������serverư��򳫻Ϥ��롣
    ;; - �֤��ͤȤ��ơ��������֤����(���Ȥǻ��ͤ�ͤ���)��
    ;;   ¿ʬ����������csid�ο��Ȥ����̾�shutdown��graceful-shutdown���Ȥ���
    ;;   ����ʤΤ��֤�ͽ�ꡣ
    ssc:server-shutdown
    ;; (ssc:shutdown object-of-socket-server-coroutine)
    ;; (ssc:shutdown)
    ;; - client-handler���椫��Ƥ֡�
    ;;   �����Ф�shutdownͽ���Ԥ���
    ;;   �ºݤ�shutdown�ϡ������Ф�ư��ְ����ʥݥ���ȡפ�
    ;;   �褿�ʳ��ǹԤ��롣
    ;;   ����Ū�ˤϡ�ssc:shutdown��ƤӽФ���client-handler��
    ;;   ssc:sleep��ssc:pause��¹Ԥ����ꡢ�Ǹ�ޤǼ¹Ԥ��줿�ʳ���
    ;;   �ºݤ�shutdown���Ԥ��롣
    ;; - shutdown��ϡ�ssc:startľ������椬�Ϥ���롣
    ;; - �����Ͼ�ά��ǽ(client-handler��Τ�)��
    ssc:server-shutdown-graceful
    ;; (ssc:shutdown-graceful object-of-socket-server-coroutine)
    ;; (ssc:shutdown-graceful)
    ;; - �����Ф�graceful shutdownͽ���Ԥ���
    ;;   �����Ф��Ф�����³���Ƥ��륯�饤����Ȥο���0�ˤʤä��ʳ���
    ;;   �����Ф�shutdown��Ԥ���
    ;;   ����graceful shutdownͽ�󤬹Ԥ��Ƥ⥵���ФΥ��饤������Ԥ�������
    ;;   ��ߤ�����Ϥ���ʤ���
    ;; - �ޤ�̤������

    ;; * ssc:server-�Ȥ���prefix���Ĥ��Ƥ��ʤ���Τϡ�client-handler��ǻȤ���
    ssc:wait-data-from-socket
    ;; (ssc:wait-data-from-socket timeout-second)
    ;; (ssc:wait-data-from-socket)
    ;; - ���ߤ�client-handler���������äƤ���socket����
    ;;   �ɤ߽Ф���ǽ�ˤʤ뤫��timeout-second�ä��в᤹��ޤ��Ԥġ�
    ;;   ���μ�³���Ͽ����ͤ��֤���
    ;;   �֤��ͤ�#t�ʤ顢socket���ɤ߽Ф���ǽ�ˤʤä���
    ;;   �֤��ͤ�#f�ʤ顢timeout-second�ä��вᤷ������
    ;;   socket���ɤ߽Ф���ǽ�ˤʤ�ʤ��ä���
    ;;   timeout-second�äλ���Ͼ�ά��ǽ�ǡ����ξ���#f�����Ѥ���롣
    ;;   timeout-second��#f�ξ��ϡ��ɤ߹��߲�ǽ�ˤʤ�ޤǱʱ���Ԥ�³���롣
    ;;   (����socket�����Ǥ��줿���ϡ�socket����EOF���ɤ߽Ф���ǽ�ˤʤ�Τǡ�
    ;;    �֤��ͤȤ���#t���֤���롣)
    ;; - ����timeout-second�Ͼ����Ǥ�ok��
    ssc:pause
    ;; (ssc:pause)
    ;; - ���롼�������楳�ޥ�ɡ�
    ;;   ¾�Υ��饤����Ȥ�¸�ߤ���ʤ顢���ä���������Ϥ���
    ;;   ¾�Υ��饤����Ȥ�¸�ߤ��ʤ��ʤ顢���Τޤ����椬��롣
    ssc:sleep
    ;; (ssc:sleep second)
    ;; - ���롼�������楳�ޥ�ɡ�
    ;;   second�ä��в᤹��ޤǤϡ��ƤӽФ������롼����ˤ����椬�Ϥ���ʤ���
    ;;   second�ä��вᤷ����ϡ�ssc:pause��Ʊ�ͤΥ����ߥ󥰤����椬��롣
    ;;   sleep�����ѡ�
    ;; - ����second�Ͼ����Ǥ�ok��
    ssc:disconnect
    ;; (ssc:disconnect object-of-socket-server-coroutine csid . opt-drop-cont)
    ;; (ssc:disconnect csid . opt-drop-cont)
    ;; - client-handler�˴�Ϣ�դ���줿csid����ꤹ��ȡ�����餬��������
    ;;   socket��shutdown����close���롣
    ;; - ���μ�³���ϡ��̾�Ϥޤ��Ȥ������̵���Ȼפ��롣
    ;;   (client-handler����λ�����socket�ϼ�ưŪ���Ĥ������)
    ;; - opt-drop-cont�ˤϿ����ͤ���ꤹ�롣��ά��������#f�Ȥ��ư����롣
    ;; -- opt-drop-cont��#f�ʤ�socket��shutdown�Τߤ��Ԥ��롣
    ;;    ��������socket�˴�Ϣ�դ����Ƥ���client-handler�Ϥޤ��Ĥꡢ
    ;;    ����client-handler�˼������椬�Ϥä����ˤϡ�
    ;;    socket���ɤ߽񤭤򤷤褦�Ȥ����EOF�䥨�顼�ˤʤ�Τǡ�
    ;;    socket�����Ǥ���Ƥ������ǧ���Ǥ���ɬ�פʤ�finalize������
    ;;    �¹Ԥ�����褦�ʻ�������롣
    ;; -- opt-drop-cont��#t�ʤ�socket��shutdown�ȡ�client-handler��
    ;;    ����ޤǼ¹Ԥ��졢�ݻ����줿�ޤޤΥ��롼������˴�����롣
    ;;    ����ϡ�client-handler�ˡ�socket�����Ǥ��줿����finalize������
    ;;    �񤤤Ƥ����Ȥ��Ƥ�¹Ԥ���ʤ����Ȥ��������̣���롣
    ;;    �ɤ��������ʤΤ�ʬ���äƤ�����ˤ������ꤹ�����
    ;; - ����ssc:disconnect��Ƥֺݤˤϡ���ʬ���Ȥ�csid����ꤹ����ϤǤ��ʤ���
    ;;   ���μ�³���Ϥ����ޤǡ�¾��Ǥ�դΥ��饤����Ȥ����Ǥ���٤Τ�Ρ�
    ;;   (���⤽�⡢client-handler�Υ��롼���󤬽�λ����С���Ϣ�դ����Ƥ���
    ;;    socket�ϼ�ưŪ�����Ǥ����Τǡ����������ƤӽФ�������)

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
     :init-value #f) ; bind/listen����sockaddr�����
   (select-interval
     :accessor select-interval-of
     :init-keyword :select-interval
     :init-value 1000000) ; default 1 sec

   ;; ����ϡ����饤����Ȥ�����³�����ä����ˡ��ʲ��Τ褦��proc����ꤹ�롣
   ;; (proc csid client-socket in-port out-port)
   ;; csid�ϡ���³���Ϣ�֤ǿ�������͡�
   ;; in-port�ϡ����饤����Ȥ���Υǡ������ɤ߼���port��
   ;; �ɤ߼����ˤϡ��֥�å����Ƥ��ޤ�ʤ��褦����դ��ʤ��ƤϤʤ�ʤ���
   ;; out-port�ϡ����饤����Ȥ˥ǡ����������Ǥ���port��
   ;; ����proc����λ/���顼���ꤲ��ȡ�socket�ϼ�ưŪ���Ĥ����롣
   ;; ��socket�������塢�ɤ��ʳ�����³�����Ǥ���뤫��ʬ����ʤ��١�
   ;;   �嵭��socket���Ф���read/write����ݤˤ�ɬ����#<eof>���֤äƤ����ꡢ
   ;;   ����ޤǤ���read�Ǥ��ʤ��ä��ꡢwrite���褦�Ȥ����饨�顼��ȯ�������ꡢ
   ;;   SIGPIPE��ή�줿�ꡢ�Ȥ��ä����������ʤ��ƤϤʤ�ʤ���
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
   (client-counter ; client���unique��id���ͤ�ȯ�Ԥ���٤Υ����󥿡�
     :accessor client-counter-of
     :init-value 0)
   (session-table
     :accessor session-table-of
     :init-form (make-hash-table 'eqv?)) ; key��csid(client-counter�ο���)��
   (shutdown-request ; ���줬#t�ʤ顢server�ν�λ�򳫻Ϥ��롣
     :accessor shutdown-request-of
     :init-value #f)
   (graceful-shutdown-request ; ���줬#t�ʤ顢server��graceful��λ�򳫻Ϥ��롣
     :accessor graceful-shutdown-request-of
     :init-value #f)
   (selected-csid-queue
     :accessor selected-csid-queue-of
     :init-form (make-queue))
   (selector-timeout-alist ; (csid timeout-epoch)�����ǤȤ���list��
     :accessor selector-timeout-alist-of
     :init-value '())
   (slept-csid-alist ; (csid wakeup-epoch)�����ǤȤ���list��
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

;; �����Υѥ�᡼���ϴ���Ū�ˡ�client-handler�ǻȤ���
(define ssc (make-parameter #f)) ; self
(define in-client-handler? (make-parameter #f)) ; �����ʥ��������ư��Ƚ����
(define return/client-handler-cont (make-parameter #f)) ; ��³����ݻ���
(define client-session (make-parameter #f)) ; session�����ݻ���
(define client-csid (make-parameter #f)) ; csid�ݻ���


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
  (let-keywords* keywords ((cont #f) ; �¹�����η�³���ݻ�����
                           (client-socket #f) ; socket���Τ�Τ��ݻ�����
                           (select-handler #f)
                           ;; remove-client�λ���ɬ�פˤʤ롣
                           ;; client-handler�Ȥϰ㤦�������
                           )
    ;; ToDo: ���Ǥ������Ƥ�����keywords�Τޤ޴�������Ȥ����⤦�����ͤ�ľ��
    (list
      cont ; car
      client-socket ; cadr
      select-handler ; caddr
      )))
(define session->cont car)
(define session->client-socket cadr)
(define session->select-handler caddr)





;;;; ToDo: �ʲ��ϸŤ�������





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
                   ;; SIGTERM���ϡ�stack dump��ɽ�����ʤ�
                   (verbosef self "caught SIGTERM")
                   (client-terminate))
                  (else
                    (verbosef self "client-handler error (csid=~s)" csid)
                    (report-error e)
                    (client-terminate)))
          (let1 pc (let/cc return
                     ;; client-handler�¹���ϥե饰��Ω�Ƥ�
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
                     ;; �Ǹ�ޤǼ¹Ԥ��줿�Τǡ�disconnect/finalize����
                     (verbosef self "client-handler finished (csid=~s)" csid)
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







;;;; ToDo: �ʾ�ϡ��Ť�������



;; �Ƽ¹Ԥ���ݤ˱ƶ���̵���褦�ˡ����������ˤ����³��
(define (ssc:clean-up self)
  ;; note: client-counter�����Ͻ���������˺����Ѥ��롣
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


;;; selector����Ͽ����٤Ρ�client-socket��handler���֤�
(define (make-select-handler self csid)
  (lambda (in flag)
    ;; note: �������ΤǤϼºݤν����ϹԤ鷺������ñ��
    ;;       selected-csid-queue��csid���ɲä����������Ԥ���
    (verbosef self "received data csid ~s" csid)
    (set! (selected-csid-queue-of self) csid)))

;;; selector����Ͽ����٤Ρ�server-socket��handler���֤�
(define (make-server-accept-handler self server-socket)
  ;; client��server����³����ȡ�selector���餳��proc(��)���ƤФ�롣
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
        ;; select-handler��selector����Ͽ
        (selector-add! (selector-of self)
                       (socket-input-port client-socket)
                       select-handler
                       '(r))
        ;; session-table����Ͽ
        (hash-table-put! (session-table-of self) csid session)
        ;; selected-csid-queue���ɲ�
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
    ;; �Ƽ凉��åȤ�ǰ�ΰ٤�clean up
    (ssc:clean-up self)
    ;; server���Ȥ�accept-handler��selector����Ͽ
    (selector-add!
      (selector-of self)
      (socket-fd server-socket)
      (make-server-accept-handler self server-socket)
      '(r))
    ;; server-socket�򥹥�åȤ���¸
    (set! (server-socket-of self) server-socket)
    (verbosef self "ssc opened.")))

;;; (slept-csid-alist-of self)��(selector-timeout-alist-of self)�򸫤ơ�
;;; Ŭ�ڤ�select��microseconds�ޤ���#f���֤���
;;; (�Ĥޤꡢ���Τޤ�selector-select��timeout�ͤȤ������ѤǤ���)
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
      ;;;; ToDo: �ʲ��Ϥޤ���ǧ���Խ�ʬ��
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
                    (execute-client-handler self csid session))) ; ���¹�
                (set! (selected-csid-of self) #f))) ; selected-csid�򥯥ꥢ
            (if (shutdown-request-of self)
              (let1 server-finalizer (server-finalizer-of self)
                (when server-finalizer
                  (server-finalizer))
                (all-client-graceful-disconnect self)) ; ���θ塢close�����
              (begin
                (and
                  callee
                  (eqv? old-c c) ; select��timeout������#t�ˤʤ�(Ȧ)
                  ;; note: �����Ǥϡ������ơ����顼�ݸ�Ϥ��ʤ���
                  ;;       (���顼ȯ��=�����ФϤ��Τޤ޽�λ�Ȥ���)
                  (callee))
                (loop c)))))))))

(define (ssc:close self)
  (verbosef self "ssc closing...")
  (unless (ssc:server-opened? self)
    (error "not opened"))
  ;; �ޤ���accept-handler����ߤ���
  (remove-accept-handler self)
  ;; �ĤäƤ������Ƥ�client�ˤĤ��ơ�
  ;; client-socket��disconnect�����¹�����η�³���˴�����
  (map
    (cut remove-client self <>)
    (hash-table-keys (session-table-of self)))
  ;; clean up
  (ssc:clean-up self)
  (verbosef self "ssc closed."))


;;; ���μ�³���ϡ�server-socket��disconnect����
;;; selector����accept-handler�����롣
;;; ��������(server-socket-of self)�Ϥޤ�#f�ˤϤ��ʤ���
;;; (����ϥ����Фε�ư/��ߤ�Ƚ��ˤ�Ȥ��Ƥ����)
(define (remove-accept-handler self)
  (unless (ssc:server-opened? self)
    (error "server-socket not opened"))
  (let1 server-socket (server-socket-of self)
    ;; �ޤ��ǽ�ˡ�<sockaddr-un>�ʤ顢socket-file��������
    ;; (race condition�ɻߤΰ٤ˡ���˺������)
    (when (eq? 'listening (socket-status server-socket))
      (let1 sockaddr (sockaddr-of self)
        (when (eq? 'unix (sockaddr-family sockaddr))
          (let1 sockfile (sockaddr-name sockaddr)
            (verbosef self "unlink to ~s" sockfile)
            (ignore-error (sys-unlink sockfile))))))
    ;; accept-handler�򥯥ꥢ(fd�δط��塢disconnect�������˥��ꥢ����)
    (selector-delete! (selector-of self) (socket-fd server-socket) #f #f)
    ;; server-socket�����Ǥ�����������³������ʤ��褦�ˤ���
    (socket-disconnect/ignore-error server-socket)))

;;; ���μ�³���ϡ�csid�˷���դ����Ƥ���socket��disconnect����
;;; csid�˷���դ����Ƥ����³���˴����롣
(define (remove-client self csid)
  (verbosef self "remove to csid ~s" csid)
  (let1 session (hash-table-get (session-table-of self) csid #f)
    (if (not session)
      (verbosef self "cannot found csid ~s in session-table" csid)
      (begin
        ;; socket���Ĥ���
        (socket-disconnect/ignore-error (session->client-socket session))
        ;; selector�������
        (selector-delete!
          (selector-of self)
          #f
          (session->select-handler session)
          #f)
        ;; session-table����쥳���ɤ�ä�
        (hash-table-delete! (session-table-of self) csid)
        ;; selected-csid-queue��slept-csid-alist��
        ;; selector-timeout-alist�����ä�
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
      ;; ssc:server-shutdown�Ȥϰ㤤��accept-handler�Ͻ���ʤ�
      ;; (graceful��λ��ľ����remove�����)
      (set! (graceful-shutdown-request-of self) #t))
    (error "server is not started")))
(define-method ssc:server-shutdown-graceful ()
  (if (ssc)
    (ssc:shutdown-graceful (ssc))
    (error "not in client-handler")))



(define-method ssc:wait-data-from-socket ((timeout-second <number>))
  ;;;; ToDo: ���Ȥ�
  #f)
(define-method ssc:wait-data-from-socket ()
  (ssc:wait-data-from-socket 0))

(define-method ssc:pause ()
  ;;;; ToDo: ���Ȥ�
  #f)

(define-method ssc:sleep ((second <number>))
  ;;;; ToDo: ���Ȥ�
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


