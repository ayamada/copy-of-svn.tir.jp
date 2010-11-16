;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; plain irc bot (pib) module

;;; basis on http://homepage3.nifty.com/oatu/gauche/try.html#ircbot

;;; RFC: http://www.haun.org/kent/lib/rfc1459-irc-ja.html
;;; �ɤ��⡢�Ƕ��irc�����Фϳ�ĥ����Ƥ��ơ�rfc1459���⹭���ϰϤ�
;;; ���꤬��ǽ�ʤ褦���������Υ�����ץȤǤ�rfc1459���Ȥ���
;;; (nick�˥�������С����Ȥ��ʤ����������ޤ�)

;;; usage:
;;; (with-irc
;;;   irc-server-ip ; irc�����Ф�ip�ޤ��ϥɥᥤ��̾��ʸ�����
;;;   irc-server-port ; irc�����Ф�port����ͤ�
;;;   :irc-server-encoding "iso-2022-jp" ; �ǥե���Ȥ�"utf-8"
;;;   :irc-server-pass "..." ; �ǥե���Ȥ�#f
;;;   :base-nick "..." ; nick����ʣ���Ƥ����ݤˤϡ���ưŪ����̾�����
;;;   :logging-handler (make-basic-irc-logger "log") ; �ǥե���Ȥ�#f
;;;   ;; ��proc����ꤹ��ȡ�event�����������ˡ����󥰤ΰ٤ˡ�
;;;   ;; �����Υϥ�ɥ餬�ƤФ�롣�֤��ͤϼΤƤ��롣
;;;   ;; �����Υϥ�ɥ������/��������åɤǼ¹Ԥ�����������ա�
;;;   ;; ��(make-basic-irc-logger log-dir)�ǡ��ʰץ�����³���������Ǥ���
;;;   (lambda ()
;;;     (irc-send! '(JOIN "#channel")) ; join���ޥ�ɤ�¹�
;;;     (let loop ()
;;;       (let1 event (irc-recv-event! 1)
;;;         ;; ��timeout 1�ä�message����
;;;         ;;   (������timeout��#f����ꤷ�Ƥ��ʤ��Τϡ�#f����ꤷ�Ƥ��ޤ���
;;;         ;;    �����ʥ������Ǥ��ʤ��ʤäƤ��ޤ���)
;;;         (when event
;;;           (write event)
;;;           (newline))
;;;         (loop)))))
;;; - irc-send-event! �ڤ� irc-recv-event! �ǰ���irc�ץ�ȥ���Υ�å������ϡ�
;;;   ��event�׽񼰤�Ϳ����졢���������ˤʤ롣
;;;   ��event�׽񼰤ϰʲ��Τ褦��list�Ǥ��롣
;;;   '(timestamp mode prefix command . params)
;;; -- timestamp�ϡ�����/�������������ꤹ��ʸ����
;;;    ��������(�Ĥޤꡢirc-send-event!���Ϥ�)event�Ǥϡ������#f�ˤ��Ƥ�����
;;; -- mode�ϡ������������������ꤹ�륷��ܥ롣'send�ޤ���'recv��
;;;    �������������event�Ǥϡ�#f�ˤ��Ƥ�����
;;; -- prefix�ϡ�irc�����Фˤ�ä���Ϳ����롢����message�δ�Ϣ�Ԥ򼨤�ʸ����
;;;    �������������event�Ǥϡ�#f�ˤ��Ƥ�����
;;; -- command�ϡ�irc���ޥ�ɤ򼨤�����ܥ롢�ޤ��ϡ�irc�쥹�ݥ󥹤򼨤�
;;;    ���绰��ο��͡�
;;; -- params�ϡ�command��Ϳ�����������
;;; - (irc-send-event! event)��¹Ԥ�����ǡ�event��irc�����Ф������Ǥ��롣
;;;   ������async�˼¹Ԥ����١������������������ɤ����˴ؤ�餺��
;;;   ���μ�³���Ͼ��#t���֤���
;;; - (irc-send! arg)�ϡ�(irc-send-event! (list* #f #f #f arg))��������
;;;   ���Ҥ�û������٤Τ�Ρ�
;;; - (irc-recv-event! timeout)��¹Ԥ�����ǡ�irc�����Ф�������졢
;;;   ���˥��塼�ˤ��ޤäƤ���event���ļ����Ǥ��롣
;;;   ���塼��event��̵����timeout���ԤäƤⲿ�������Ƥ��ʤ��ä����ϡ�
;;;   ���μ�³����#f���֤���
;;;   timeout�ϥ��ץ���ʥ�����ǡ��ǥե���Ȥ�0��
;;;   timeout��#f��Ϳ������ǡ���å����������ޤ��ԤĻ����Ǥ��뤬��
;;;   gauche.pthreads�λ��ͤ�ꡢ�ԤäƤ���֤ϥ����ʥ�������Ԥ��ʤ�����
;;;   ��դ��ʤ��ƤϤʤ�ʤ���
;;; - (get-recv-cv)��¹Ԥ�����ǡ��������塼�ƻ��Ѥ�condition-variable��
;;;   �����Ǥ��롣
;;;   ����cv�ϡ��������塼��event���ɲä�������condition-variable-broadcast!
;;;   ���¹Ԥ����Τǡ�Ŭ����mutex���Ѱդ���mutex-unlock�ΰ�����cv���Ϥ�����
;;;   irc-recv-event!��Ȥ鷺�ˡ��������塼�Ԥ���¸��Ǥ��롣
;;;   ����cv��¾�����Ӥ�ή�Ѥ���ʣ������Ե���Ԥ������ǽ��
;;;
;;; �����:
;;; - with-irc����������η�³/���顼æ�Фϲ�ǽ�Ǥ�����
;;;   �����ؤη�³��é����ˤ��б����Ƥ��ޤ���
;;; - irc-recv-event!��irc-send-event!�ǥ֥�å����Ƥ������ϡ�
;;;   �����ʥ뤬��������ޤ���
;;;   (�Ĥޤꡢctrl+c��SIGTERM�ǽ�λ�������ʤ��ʤ�Ȥ������Ǥ�)
;;;   ��ǽ�ʤ顢��֥�å��⡼�ɤˤ��뤫��û���ܤ�timeout�����ꤷ��
;;;   �����֤������������Ǥ���

;;; TODO: rfc�Ρ����ޥ�ɿ��ͤ�ʸ������Ѵ�����ơ��֥���󶡤������Ȥ���

;;; TODO: JOIN/NICK����prefix�򸫤���ǡ���ʬ���Ȥ�ؤ�ʸ�������롣
;;;       ����åȤ��ɲä��Ƥ������¸���Ƥ������桼�������ȤǤ���褦�ˤ��롩

;;; TODO: ��λ�������ˡ��ƥ���åɤ�socket-input-port��close������socket��
;;;       ��λ�����Ƥ⡢��������åɤ�select��ȿ�����Ƥ��ʤ��äݤ���
;;;       �Ȥꤢ������select�Τޤޤʤ顢���Τޤ�thread-terminate!���Ƥ�
;;;       ����ʤ�Ȧ�ʤΤǡ����Τޤޤˤ��Ƥ�����
;;;       �����ɤ��ˤ����Ʋ���ǽ�ʤ��褷�Ƥ������������Ȥǡ�

;;; TODO: ��ǽ�ʤ顢�ͤ�Ŭ�����ɤ����Υ����å��ϡ�slot��set!����ݤ˹Ԥ�����

;;; TODO: :logging-handler���ҥ���åɤǼ¹Ԥ����ΤϤ���ä���̯��
;;;       �����ɤ���ˡ��̵������

;;; TODO: cthreads�б�
;;; �б����ϰʲ��Τ褦�ˤʤ�
;;; - cthreads��use����ޥ����guard�դ��Ǽ¹�(cthreads��¸�ߤ��ʤ����ΰ٤�)
;;; - pib��ѥ�᡼���ѿ�����¸������褦�ˤ���
;;; - thread, mutex, cv��Ϣ�μ�³����Ƥ�Ǥ�����ʬ�򡢰ʲ��Τ褦�˽������롣
;;; -- �㤨�С�thread-start!�ʤ顢
;;;    (thread-wrapper 'thread-start! ...)
;;;    �Τ褦�ˤʤ�
;;; - thread-wrapper��³�����������
;;; -- (thread-type-of (pib))�򸫤ơ�gauche.threads��������cthreads��������
;;;    �ɤ��餫��¹Ԥ���褦�ˤ���
;;; -- thread-select!�Τߡ�cthreads��ͭ�μ�³���ʤΤǡ�
;;;    gauche.threads�������̰�����ɬ�פˤʤ�������

;;; ���ߤλ��ͤ�������:
;;; - with-irc�⤫���³����¸����ȴ������³����Ƴ�������硢
;;;   NICK��USER�ϺƼ¹Ԥ���뤬��JOIN��MODE���Ƽ¹Ԥ���ʤ���
;;;   �����Ƽ¹Ԥ����褦�ˤ���٤ˤϡ��ʲ��μ�����ˡ�����롣
;;;   (���������ɤ���⡢�����ʲ����ˡ�Ȥϸ�����)
;;; -- join-channel!���μ�³�����Ѱդ��ơ������ȤäƤ�餦�褦�ˤ���
;;;    (���ǡ����֥����������JOIN���������ͥ����򵭲����Ƥ�����
;;;     dynamic-wind�γ���thunk�ǡ�����JOIN������褦�ˤ���)
;;; --- ������ˡ���������ϡ��桼�������JOIN���ޥ�ɤ�¹Ԥ����ꤹ��ȡ�
;;;     �����������ʤ��ʤ����ˤ��롣
;;; -- irc-send-event!��ˤơ����ޥ�ɤ�ƻ뤷��JOIN/PART/MODE����
;;;    ���Υѥ�᡼���򵭲����Ƥ����褦�ˤ���
;;;    (���ǡ����֥����������JOIN���������ͥ����򵭲����Ƥ�����
;;;     dynamic-wind�γ���thunk�ǡ�����JOIN������褦�ˤ���)
;;; --- ������ˡ�������ϡ��桼�����ռ�����ɬ�פ�̵�����ˤ��롣
;;;     �ޤ���change-nick!�μ����⤳��ȶ��̲���ǽ�ˤʤ롣
;;; --- ������ˡ���������ϡ��¹Ԥ������ޥ�ɤ�irc������¦�ǥ��顼��Ƚ�ꤵ�졢
;;;     �ºݤˤ�ȿ�Ǥ���ʤ��ä����Ǥ⡢������н褷�Ť餤�Ȥ������ˤ��롣
;;;     �ޤ���KICK���Ρ�irc������¦��������ˤ��б�����ɬ�פ�����(¿ʬ)��
;;; - �Ȥꤢ���������Ԥβ����ˡ�ϼ��ʤ����ˤ��롣
;;;   �б�����Ȥ������Ԥ���ˡ�ǡ�
;;; - �ַ�³�ˤ���������ԤäƤϤʤ�ʤ��פ����֥���ץ�ʲ�����ġ�
;;; -- �Ȥꤢ���������ΤȤ���ϡ�����Ǥ���(�������פʰ�)

;;; ���:
;;; - ��������åɤϡ��������塼�˥ǡ��������äƤ���Τ��Ԥİ٤ˡ�
;;;   ������cv��mutex�򥢥��å�����(�����ॢ����̵��)
;;; - �ƥ���å�(�ڤӼ�������å�)�ϡ��������塼�˥ǡ�����������Ʊ����
;;;   ������cv��condition-variable-broadcast!��Ԥ�
;;; - �ƥ���åɤϡ��������塼�˥ǡ��������äƤ���Τ��Ԥİ٤ˡ�
;;;   ������cv��mutex�򥢥��å�����(�����ॢ���Ȥ�Ǥ����)
;;; - ��������åɤϡ��ǡ�����������ƥ��塼��������Ʊ����
;;;   ������cv��condition-variable-broadcast!��Ԥ�
;;;
;;; - ��ʬ���Ȥ�nick�δ�����ˡ
;;; -- ��������åɤ�NICK���ޥ�ɤ����ä��ݤˡ�base-nick�򹹿�����
;;; -- ��������åɤ�NICK���ޥ�ɤ���������ݤˡ�����prefix��nick��ʬ��
;;;    current-nick�Ȱ��פ�����硢current-nick�򹹿�����
;;; -- â����USER���ޥ�ɼ¹����ϡ�NICK���ޥ�ɤ��������Ƥ⡢
;;;    ������б�����NICK���ޥ�ɤϼ�������ʤ��١�
;;;    �ü�ʰ����ˤ���ɬ�פ����롣
;;;    ������������Τ�irc-user-command-state����åȡ�
;;; -- ��������åɤ�USER���ޥ�ɼ¹�����NICK���ޥ�ɤ����ä��ݤϡ�
;;;    base-nick�����ǤϤʤ���current-nick��Ʊ���˹�������褦�ˤ���

;;; NB: rfc�Ǥϡ�message��Ĺ����512byte�Τ�Τϵ��Ƥ����褦������
;;;     ���ǰ�ΰ١����Υ⥸�塼��Ǥϡ�512byte�ϥ����ȤȤ������ˤ���

;;; �����Ȥ��ƥ����ͥ�̾�ޤ���nick���륳�ޥ�ɰ���:
;;; (������(s)���դ��Ƥ����Τϡ�����޶��ڤ��ʣ�������ǽ�ʤ��)
;;; - JOIN channel(s) ...
;;; - PART channel(s)
;;; - MODE channel-or-nick ...
;;; - TOPIC channel ...
;;; - INVITE nick channel
;;; - KICK channel(s) user(s) ...
;;; - PRIVMSG channel-or-nick(s) ...
;;; - NOTICE channel-or-nick ...
;;; - LIST, NAMES (����̵����ͭ������)
;;; - WHO, WHOIS, WHOWAS (�ü�)

(define-module pib
  (use srfi-1)
  (use srfi-13)
  (use srfi-19)
  (use gauche.net)
  (use gauche.parameter)
  (use gauche.charconv)
  (use gauche.threads)
  (use gauche.selector)
  (use file.util)
  (use util.match)
  (use util.list)
  (use util.queue)
  (use text.tree)

  (export
    with-irc
    ;; �ʲ��ϡ�with-irc�����ǤΤ߻��Ѳ�ǽ�ʼ�³��
    irc-send-event! ; event����(512byte��ۤ���ʬ���ڤ�ΤƤ���)
    irc-recv-event! ; event����
    irc-send! ; �����桼�ƥ���ƥ���³��
    get-recv-cv ; �������塼��cv����
    get-current-nick ; ���ߤμ�ʬ���Ȥ�nick����
    event->reply-to ; ����event���顢�ֿ����channel�ޤ���nick����Ф�
    ;; �ֿ��褬¸�ߤ��ʤ�����#f���֤�
    send-event-split-last-param
    ;; event��message���Ѵ���encoding�Ѵ��������512byte��ۤ���ʤ顢
    ;; (values 512byte�˼��ޤ�褦�˽������줿event �ڤ�ΤƤ�줿��ʬ)
    ;; ���֤���롣â�����ڤ�Τ��оݤˤʤ�Τϡ�event��params�κǸ�����ǤΤ�
    ;; �Ȥ���(params�Ǹ�����Ǥ����ƼΤƤƤ�512byte��ۤ�����ϥ��顼�㳰)
    ;; 512byte��ۤ��ʤ��ʤ顢(values ����event #f)���֤����
    ;; ����event�������ʾ��⥨�顼�㳰���ꤲ��
    ;; ���μ�³���ϥޥ���Х���ʸ�������Ȥ���((gauche-character-encoding)����)
    ;; ���μ�³���Ϥ�������Τǡ����ꥮ��³������侮�����ڤ����������
    ;; (512byte�ʾ�ˤʤ�ݥ���Ȥ��ڤ������̵��)

    ;; event�񼰤��Ф��륢������
    event->timestamp
    event->mode
    event->prefix
    event->command
    event->params

    ;; ����¾�Υ桼�ƥ���ƥ���³��
    message->event ; �Ѵ���³��(encoding�Ѵ��ϹԤ�ʤ��������)
    event->message ; Ʊ��(���ץ���ʥ����ͭ��)
    valid-nick? ; nick��valid���ɤ�����ǧ����Ҹ�
    make-basic-irc-logger ; �ʰץ��������ⳬ��³��
    make-thread/report-error ; ����åɤ����顼�㳰�ǽ�λ�����ݤ�
                             ; (standard-error-port)�˥����å��ȥ졼��ɽ��
                             ; ����ʳ���make-thread��Ʊ��

    ;; TODO: ¾�ˤ⡢rfc�˶ᤤ��ʬ���󶡤��������ɤ�
    ))
(select-module pib)



;; ��������
(define event->timestamp car)
(define event->mode cadr)
(define event->prefix caddr)
(define event->command cadddr)
(define event->params cddddr)


(define param:pib (make-parameter #f))

(define (irc-send! command+params . opt-sync)
  (apply irc-send-event! (list* #f #f #f command+params) opt-sync))
(define (irc-send-event! event . opt-sync)
  (unless (param:pib)
    (error "must be into with-irc"))
  ;; �����ߴ����ΰ٤ˡ�event��command��ʸ������ä���硢�����ʳ���
  ;; ���ä���symbol���Ѵ����Ƥ���
  (let1 event-fixed (if (not (string? (event->command event)))
                      event
                      (list*
                        (car event)
                        (cadr event)
                        (caddr event)
                        (string->symbol (cadddr event))
                        (cddddr event)))
    (apply %irc-send-event! (param:pib) event-fixed opt-sync)))
(define (irc-recv-event! . opts)
  (unless (param:pib)
    (error "must be into with-irc"))
  (apply %irc-recv-event! (param:pib) opts))
(define (get-recv-cv)
  (unless (param:pib)
    (error "must be into with-irc"))
  (irc-recv-cv-of (param:pib)))
(define (get-current-nick)
  (unless (param:pib)
    (error "must be into with-irc"))
  (current-nick-of (param:pib)))


(define (event->reply-to event)
  (unless (param:pib)
    (error "must be into with-irc"))
  (let/cc return
    ;; �������٥�Ȥξ��ˤ�̵���Ȥ���
    (when (eq? 'send (event->mode event))
      (return #f))
    (let1 command (event->command event)
      ;; command�����ͤΤ�ΤϽ�������
      (when (number? command)
        (return #f))
      ;; ����Υ��ޥ�ɤΤ߽�����Ԥ�
      ;; ����Ū�ˡ�������Ԥ�ɬ�פ����륳�ޥ�ɤϰʲ����̤�
      ;; KICK PRIVMSG NOTICE MODE INVITE
      ;; �����⡢INVITE�Τ��ü�ǡ�(car params)�ǤϤʤ���
      ;; (cadr params)���ֿ���ˤʤ�(car�����ϼ�ʬ���Ȥˤʤ��)
      ;; �ޤ��������Ǥϥ���޶��ڤ��ʣ������λ��Ϲ�θ���ʤ��Ƥ��ɤ�
      ;; (�ۤȤ�ɤ�ʣ�������ǽ���ޥ�ɤ��������٥�Ȼ��Τߤǡ�
      ;;  �������٥�Ȥ�������̵��Ȧ)
      (let1 params (event->params event)
        (case command
          ((KICK PRIVMSG NOTICE MODE) #t) ; ³��
          ((INVITE) (return (cadr params)))
          (else (return #f)))
        (let1 target (car params)
          ;; target�������ͥ�̾��(#�ޤ���&�Ϥ��ޤ�)�ʤ顢
          ;; ����target���ֿ���ˤʤ�
          (when (#/^(?:\#|\&)/ target)
            (return target))
          ;; ����event�ϼ�ʬ���ƤʤΤǡ�prefix�����ֿ����Ĵ������
          ;; prefix���顢����message������������Ф���������֤�
          ;; prefix�ν񼰤ϡ�"nick!~username@hostname"
          ;; ��äơ�!���������ʬ����Ф��������ɤ�
          (and-let* ((prefix (event->prefix event))
                     (matched (#/\!/ prefix)))
            (matched 'before)))))))



;; NB: irc�����ФǻȤ���encoding��utf-8�ޤ���jis������Ū�ǡ�
;;     �������θ���ư��������ꥵ�������ڤ���Τ���̯�˺���
;;     �����Ǥϡ����Τ�����®�٤�ͥ�褹�롣
(define (send-event-split-last-param event-orig)
  (define (conv-enc str)
    (ces-convert str
                 (gauche-character-encoding)
                 (irc-server-encoding-of (param:pib))))
  (define (event->converted-size event)
    (string-size (conv-enc (event->message event #t))))
  (define (get-size-of-converted-last-param last-param)
    (string-size (conv-enc last-param)))
  (define (find-suitable-index require-size str)
    (define (get-size idx)
      (get-size-of-converted-last-param (string-take str idx)))

    ;; �Ѵ����ˤɤ줰�餤���������Ѳ����뤫���ñ�˵����ʤ��١�
    ;; ��ʬˡ��Ȥä���ƻ�˵��Ƥ���
    ;; TODO: counter�ν���ͤϡ�require-size��str��length����ʬ��ǵ���٤�
    (let retry ((counter 8) ; �������
                (start-idx 0)
                (end-idx (- (string-length str) 1)))
      ;; �������ʾ���������顢start-idx���֤�
      (if (not (positive? counter))
        start-idx
        (let1 middle-idx (quotient (+ start-idx end-idx) 2)
          (cond
            ((= start-idx middle-idx) start-idx)
            ((= end-idx middle-idx) middle-idx)
            (else
              ;; middle-idx���ڤä��ݤ��Ѵ���̤����׵ᤵ�줿size����
              ;; �礭�������������Ǽ���Ƚ����ѹ����롣
              ;; (Ʊ�����ϡ�idx���������ʤ��������ֻ��ˤ���)
              (let* ((middle-size (get-size middle-idx))
                     (next-start/end-idx (if (< middle-size require-size)
                                           (list middle-idx end-idx)
                                           (list start-idx middle-idx))))
                (apply retry (- counter 1) next-start/end-idx))))))))

  (unless (param:pib)
    (error "must be into with-irc"))
  (let/cc return
    ;; �ޤ��ǽ�ˡ�512byte��ۤ��Ƥ��뤫�δ�ñ�ʥ����å���Ԥ�
    (let1 converted-size-orig (event->converted-size event-orig)
      ;; �ۤ��Ƥ��ʤ��ʤ顢���Τޤ޽�λ
      (when (< converted-size-orig 512)
        (return event-orig #f))
      ;; �ۤ��Ƥ���ʤ顢�ۤ��Ƥ����̤˱����ƽ�����Ԥ�ɬ�פ�����
      ;; �ޤ���ɬ�פ��ͤ���˵��Ƥ���
      (let* ((event-without-last-param (drop-right event-orig 1))
             (params-orig (event->params event-orig))
             (last-param (if (null? params-orig)
                           (error "params not found, but over 512 byte")
                           (last params-orig)))
             (least-converted-size (event->converted-size
                                     `(,@event-without-last-param "")))
             (level-value (- 512 least-converted-size))
             )
        (when (<= 512 least-converted-size)
          ;; TODO: ���顼���Ƥ�ʸ�Ϥ��ѤʤΤ�ľ����
          (error "too big without last-param"))
        ;; ���Ȥϡ�last-param��size��level-value̤���ˤʤ륮�ꥮ���size��
        ;; ����Ф褤
        (let* ((result-index (find-suitable-index level-value
                                                  last-param))
               (result-event
                 `(,@event-without-last-param ,(string-take last-param
                                                            result-index)))
               (result-remainder (string-drop last-param result-index))
               )
          ;; ǰ�ΰ١��ǽ������å���Ԥ�
          (when (<= 512 (event->converted-size result-event))
            (error "assertion occurred" event-orig))
          (values result-event result-remainder))))))


(define-syntax ignore-error
  (syntax-rules ()
    ((ignore-error fallback . bodies)
     (guard (e (else fallback)) . bodies))))


(define-class <pib> ()
  (
   ;; ����˴ؤ��륹��å�
   (irc-server-ip
     :accessor irc-server-ip-of
     :init-keyword :irc-server-ip
     :init-form (error "must be need irc-server-ip"))
   (irc-server-port
     :accessor irc-server-port-of
     :init-keyword :irc-server-port
     :init-form (error "must be need irc-server-port"))
   (irc-server-encoding
     :accessor irc-server-encoding-of
     :init-keyword :irc-server-encoding
     :init-value "utf-8")
   (irc-server-pass
     :accessor irc-server-pass-of
     :init-keyword :irc-server-pass
     :init-value #f)
   (thread-type
     :accessor thread-type-of
     :init-keyword :thread-type
     :init-value 'gauche.threads)
   (base-nick
     :accessor base-nick-of
     :init-keyword :base-nick
     :init-value "pib")
   (username
     :accessor username-of
     :init-keyword :username
     :init-value #f)
   (realname
     :accessor realname-of
     :init-keyword :realname
     :init-value #f)
   (main-thunk
     :accessor main-thunk-of
     :init-keyword :main-thunk
     :init-form (error "must be need main-thunk"))
   (flood-protection-interval
     :accessor flood-protection-interval-of
     :init-keyword :flood-protection-interval
     :init-value 900000)
   (logging-handler
     :accessor logging-handler-of
     :init-keyword :logging-handler
     :init-value #f)
   ;; �������֥���å�(���ơ����Ȥ����ꤵ���)
   (current-nick
     :accessor current-nick-of)
   (irc-socket
     :accessor irc-socket-of)
   (irc-input-port
     :accessor irc-input-port-of)
   (irc-output-port
     :accessor irc-output-port-of)
   (irc-recv-thread
     :accessor irc-recv-thread-of)
   (irc-send-thread
     :accessor irc-send-thread-of)
   (irc-recv-queue
     :accessor irc-recv-queue-of)
   (irc-send-queue
     :accessor irc-send-queue-of)
   (irc-recv-queue-mutex
     :accessor irc-recv-queue-mutex-of)
   (irc-send-queue-mutex
     :accessor irc-send-queue-mutex-of)
   (irc-recv-cv
     :accessor irc-recv-cv-of)
   (irc-send-cv
     :accessor irc-send-cv-of)
   (irc-recv-cv-mutex
     :accessor irc-recv-cv-mutex-of)
   (irc-send-cv-mutex
     :accessor irc-send-cv-mutex-of)
   (irc-send-laststatus
     :accessor irc-send-laststatus-of)
   (irc-send-last-microsec
     :accessor irc-send-last-microsec-of)
   (irc-logger-mutex
     :accessor irc-logger-mutex-of)
   (irc-registered?
     :accessor irc-registered?-of)
   (irc-finished?
     :accessor irc-finished?-of)
   ))

(define-method initialize ((self <pib>) initargs)
  (next-method)
  #t)


;; event�ϡ��ʲ��Τ褦��list�Ȥ��롣
;; '(timestamp mode prefix command . params)
;; - timestamp������������Ǥ���ʸ�����������ˤ�̵�뤵���
;; - mode��'send�ޤ���'recv�Υ���ܥ롣�������ˤ�̵�뤵���
;; - prefix�ϡ�����message��sender������Ǥ���ʸ�����������ˤ�̵�뤵���
;; - command�ϡ�'PRIVMSG���Υ���ܥ롢�ޤ��ϻ���ο���
;; - params�ϡ�����ν񼰤�������ʸ�����list(�񼰤ˤĤ��Ƥϡ�rfc����)
(define-method %irc-send-event! ((self <pib>) event . opt-sync)
  ;; - opt-sync��#f�ʤ顢������async�˼¹Ԥ��졢������������������ʬ����ʤ�
  ;;   (�ǥե���Ȥ�ư��)
  ;; - opt-sync��#t�ʤ顢������sync�˼¹Ԥ��졢�֤��ͤȤ���������������������
  ;;   �����ͤȤ����֤äƤ��뤬�������������˴�λ����ޤ��Ԥ������
  ;;   (��������ϡ������פ������������ɤ����Ǥ��ꡢ�֥��ޥ�ɼ¹ԡפ�
  ;;    �����������ɤ����ǤϤʤ�������դ�����������ơ������פ����Ԥ��븶����
  ;;    �̾�̿��ǰʳ��ˤ�¸�ߤ��ʤ�)
  ;;   ���Υ����å��μ�����ˡ��̤�ꡣ
  ;;   (�����ȼ������褦�Ȥ���ȡ����ʤ���ä�������
  ;;    race condition����̵�¥롼�פ����ǽ�����Ĥ�)
  (irc-send-enqueue! self event)
  (if (get-optional opt-sync #f)
    (error "not implement yet") ; TODO: ���ȤǼ�������
    #t))
(define-method %irc-recv-event! ((self <pib>) . opts)
  (let-optionals* opts ((timeout 0))
    (let loop ()
      (mutex-lock! (irc-recv-cv-mutex-of self))
      (let1 event (irc-recv-dequeue! self)
        (if (or event (equal? timeout 0))
          ;; ���塼���ͤ����äƤ��� or timeout��0���ä���
          ;; �����������̤˥����å����ƽ�λ
          (begin
            (mutex-unlock! (irc-recv-cv-mutex-of self))
            event)
          ;; event��#f���ä����Ĥޤꡢ���塼�϶����ä�
          (let1 start-usec (gettimeofday-usec)
            ;; ��������åɤ�����eof�������äƽ�λ���Ƥ����ʤ顢
            ;; �����ǥ��顼�㳰���ꤲ��ɬ�פ�����
            (when (irc-finished?-of self)
              (mutex-unlock! (irc-recv-cv-mutex-of self))
              (error "irc socket already closed"))
            ;; ���塼�����ʤΤǡ������ॢ����ͭ���cv�����ʥ���Ԥ�
            (if (not (mutex-unlock! (irc-recv-cv-mutex-of self)
                                    (irc-recv-cv-of self)
                                    timeout))
              ;; �����ॢ���Ȥ���
              #f
              ;; cv�����ʥ���������塼�����å�������ľ��
              ;; â���������ʥ뤬�������塼�˥����ƥब���ä��ʳ������Τ�
              ;; ��ǽ���⤢��١�����������ơ�
              ;; timeout���Ԥä����֤˱����Ƹ��餹
              ;; (�����timeout��1��ñ�̤ΰ������Τ��������ι�äƤ��ok�Ȥ���)
              (begin
                (when timeout
                  (let* ((go-on-usec (- (gettimeofday-usec) start-usec))
                         (go-on-sec (x->integer go-on-usec))
                         (delta-timeout (- timeout go-on-usec))
                         (new-timeout (if (positive? delta-timeout)
                                        delta-timeout
                                        0)))
                    (set! timeout new-timeout)))
                (loop)))))))))



;; NICK�˼��Ԥ����ݤˡ����˻�Ԥ���NICK�����������³��
(define (generate-next-nick nick)
  (define (nick-chop nick)
    ;; �ޥ���Х���ʸ����¸�ߤ�����١���ʸ�������ڤ�ͤ�Ƥ���ɬ�פ�����
    (if (< *max-nick-length* (string-size nick))
      (nick-chop (string-drop-right nick 1))
      nick))
  (define (increment-nick nick)
    (define (increment-rnc rnc)
      (if (null? rnc)
        '() ; ����ʤ顢��ʸ���ɲä���ɬ�פ����뤬��increment�������塢
            ; ���줬�¹Ԥ�������Τϡ�}}}}}}}}}�פ����----------�פˤʤ��
            ; �ΤߤʤΤǡ������ok�Ȥ���
        (let* ((target-char (car rnc))
               (remains (cdr rnc))
               (target-char-idx (hash-table-get
                                  *nickchar->nicknum* target-char -1))
               (next-idx (+ target-char-idx 1))
               (carry-flag (not (zero? (quotient next-idx *nicknum-max*))))
               (next-idx-true (remainder next-idx *nicknum-max*))
               (next-char (vector-ref *chars-can-use-nick* next-idx-true))
               )
          (cons
            next-char
            (if carry-flag
              (increment-rnc remains)
              remains)))))

    (let* ((reverse-nick-chars (reverse (string->list nick)))
           (incremented (increment-rnc reverse-nick-chars)))
      (list->string (reverse incremented))))

  (let/cc return
    ;; nick��9byte��ã���Ƥ��ʤ��ʤ顢�����˰�ʸ���ɲä���Τߤ�ok
    (when (< (string-size nick) *max-nick-length*)
      (return
        (string-append nick (string (vector-ref *chars-can-use-nick* 0)))))
    ;; nick��9byte���Ĺ���ʤ顢�ޤ����9byte���ڤ�ͤ��
    (let1 chopped-nick (nick-chop nick)
      ;; nick�򥤥󥯥���Ȥ���
      (let1 result-nick (nick-chop (increment-nick chopped-nick))
        ;; ������nick��valid-nick?���̤�ɬ�פ�̵��
        ;; (nick��rfc����ʸ���������äƤ����硢�����
        ;;  �桼������ʬ���äƤ��ơפ�äƤ���Ȧ�ʤΤ�)
        ;; ������nick�ΰ�ʸ���ܤ�<letter>�Ǥ���פȤ�����������
        ;; ���餵������ˤ���
        ;; (increment-nick�λ��;塢���줬����Ƥ�����������
        ;;  irc������¦�����餷�Ƥ�����硢�䡹��NICK���ѹ���
        ;;  �Ԥ��Ƥ��ޤ����ˤʤ��)
        (if (char-alphabetic? (string-ref result-nick 0))
          result-nick
          (nick-chop
            (string-append "A" result-nick)))))))

(define *max-nick-length* 9)
(define *chars-can-use-nick*
  (list->vector
    (string->list
      (string-append
        "-"
        "0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "[\\]^`"
        "abcdefghijklmnopqrstuvwxyz"
        "{}"
        ))))
#|
'a' ... 'z' | 'A' ... 'Z'
'0' ... '9'
'-' | '[' | ']' | '\' | '`' | '^' | '{' | '}'
|#
(define *nicknum-max*
  (vector-length *chars-can-use-nick*))
(define *nickchar->nicknum*
  (let1 table (make-hash-table 'eq?)
    (guard (e (else table))
      (let loop ((idx 0))
        (hash-table-put! table (vector-ref *chars-can-use-nick* idx) idx)
        (loop (+ 1 idx))))))


#|
<message>  ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
<prefix>   ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
<command>  ::= <letter> { <letter> } | <number> <number> <number>
<SPACE>    ::= ' ' { ' ' }
<params>   ::= <SPACE> [ ':' <trailing> | <middle> <params> ]

<middle>   ::= <��Ƭ��':'�ǤϤʤ�,SPACE,NUL,CR,CF��ޤޤʤ������Ǥʤ������ƥåȤ���>
<trailing> ::= <SPACE,NUL,CR,CF��ޤޤʤ������ƥåȤ���(���Υ����åƥȤ�����)>
<crlf>     ::= CR LF
|#
;; irc��message(��Ԥ�ʸ����)�������ꡢ�������Ϥ���
;; S���ˤ����֤���³��
;; ����ɽ���ˤĤ��Ƥϡ��ʲ��򻲾�
;; http://www.haun.org/kent/lib/rfc1459-irc-ja.html#c2.3.1
;; message�������ʾ��ϥ��顼�㳰���ꤲ�롣
(define (message->event message . opt-send?)
  (define (parse-params params)
    (let* ((m:params (or
                       (#/^ / params)
                       (error "invalid params format" message params)))
           (params-right (m:params 'after)))
      (cond
        ((string=? params-right "") '())
        ((#/^\:/ params-right) => (lambda (m)
                                    (list (m 'after))))
        ((not (#/ / params-right)) (list params-right))
        (else
          (let* ((m:params2 (or
                              (#/^(.+?)( .*)$/ params-right)
                              (error "invalid params format"
                                     message params-right)))
                 (middle (m:params2 1))
                 (next-params (m:params2 2))
                 )
            (cons
              middle
              (parse-params next-params)))))))

  (let* ((message-chomp (string-trim-right message #[\r\n]))
         (m:message (or
                      (#/^(?:\:(.*?) )?(\w+)( .*)$/ message-chomp)
                      (error "invalid message format" message-chomp)))
         (prefix (m:message 1)) ; or #f
         (command (m:message 2))
         (params (m:message 3))
         (prefix-true prefix)
         (command-true (if (#/^\d\d\d$/ command)
                         (x->number command)
                         (string->symbol command)))
         (params-true (parse-params params))
         (mode (if (get-optional opt-send? #f) 'send 'recv))
         )
    (list* (date->string (current-date))
           mode
           prefix-true
           command-true
           params-true)))

;; event����(S��)�Υǡ�����irc��message�񼰤�ʸ������Ѵ����롣
;; event�������ʷ������ä����ϥ��顼�㳰���ꤲ�롣
(define (event->message event . opt-sendmode)
  (match-let1 (timestamp mode prefix command . params) event
    ;; - timestamp��#f���ޤ��������򼨤�ʸ����
    ;; - mode��'recv�ޤ���'send�Υ���ܥ롢�ޤ���#f
    ;; - prefix��#f���ޤ��ϲ��Ԥ������ꤹ��٤�ʸ����
    ;; - command�ϥ���ܥ롢�ޤ���(���绰���)����
    ;; - params�ϡ�'()���ޤ��ϵ����������ä�ʸ�����list
    (let* ((sendmode (get-optional opt-sendmode #f))
           (prefix-for-insert (if sendmode '() (list prefix)))
           (command-true (if (integer? command)
                           (format "~3,'0d" command)
                           (symbol->string command)))
           (params-middles (if (null? params)
                             '()
                             (drop-right params 1)))
           (params-trailing (if (null? params)
                              '()
                              (list
                                (string-append ":" (last params)))))
           )
      (tree->string
        (list
          (intersperse " " `(,@prefix-for-insert
                              ,command-true
                              ,@params-middles
                              ,@params-trailing))
          "\r\n")))))


#|
<nick>       ::= <letter> { <letter> | <number> | <special> }
<letter>     ::= 'a' ... 'z' | 'A' ... 'Z'
<number>     ::= '0' ... '9'
<special>    ::= '-' | '[' | ']' | '\' | '`' | '^' | '{' | '}'
|#
(define (valid-nick? nick)
  (let1 nick-list (string->list nick)
    (and
      ;; Ĺ�������å�
      (<= 1 (length nick-list) *max-nick-length*)
      ;; �ǽ�ΰ�ʸ���ܤ���letter����
      (char-alphabetic? (car nick-list))
      ;; ¾��ʸ��������å�
      (every
        (lambda (char)
          (guard (e (else #f))
            (hash-table-get *nickchar->nicknum* char)))
        (cdr nick-list))
      ;; �������̤ä��Τ�#t���֤�
      #t)))


(define-method %irc-main ((self <pib>))
  ;; �������褿�ʳ��ǡ��Ȥꤢ����irc�����Ф���³�������֤ˤʤäƤ���
  ;; ���Ȥϡ�main-thunk��¹Ԥ��ơ��֤��ͤ����(�ޤ��ϥ��顼�ˤʤ�)�Τ��ԤĤ���
  ((main-thunk-of self)))


;; ���塼�����λ���#f���֤�(���塼��#f�����äƤ������̵����ΤȤ���)
(define-method irc-send-dequeue! ((self <pib>))
  (with-locking-mutex
    (irc-send-queue-mutex-of self)
    (lambda ()
      (if (queue-empty? (irc-send-queue-of self))
        #f
        (dequeue! (irc-send-queue-of self))))))
(define-method irc-recv-dequeue! ((self <pib>))
  (with-locking-mutex
    (irc-recv-queue-mutex-of self)
    (lambda ()
      (if (queue-empty? (irc-recv-queue-of self))
        #f
        (dequeue! (irc-recv-queue-of self))))))

(define-method irc-send-enqueue! ((self <pib>) event)
  ;; �������塼�ϡ����λ����ǰ�ö�Ѵ���Ԥ������顼���Фʤ������ǧ����
  (event->message event #t)
  ;; ���塼��event��Ĥä���
  (with-locking-mutex
    (irc-send-queue-mutex-of self)
    (lambda ()
      (enqueue! (irc-send-queue-of self) event)))
  (condition-variable-broadcast! (irc-send-cv-of self)))
(define-method irc-recv-enqueue! ((self <pib>) event)
  ;; �������塼�ϡ��ä˥����å���Ԥ�ɬ�פ�̵��
  (with-locking-mutex
    (irc-recv-queue-mutex-of self)
    (lambda ()
      (enqueue! (irc-recv-queue-of self) event)))
  (condition-variable-broadcast! (irc-recv-cv-of self)))


;; ���μ�³���λ�����:
;; - socket���Ȥ����ǤʤɤϿƥ���åɤޤ�����³�襵���Ф��Ԥ��١�
;;   port���Ф������������˥��顼�㳰��ȯ�������ʤ顢������ǧ����
;;   �⤷�����ʤ顢���顼�ǤϤʤ���³�ǤȤ��ƥ��塼�˥��٥�Ȥ���������
;;   �����ơ����θ������ƥ���åɤǤ��äƤ���³�襵���ФǤ��äƤ⡢
;;   �ʹߤޤȤ��������������Ͻ���ʤ�Ȧ�ʤΤǡ�����åɤ�
;;   ��ʬ���Ȥ�λ���������
(define-method %irc-recv-thread ((self <pib>))
  (define (fetch-event)
    ;; event��eof�����顼�������ǡ���(S�����Ѵ���)�Τɤ줫
    (guard (e
             ;; ���顼��port��close����Ƥ�����˵�������ʤ顢
             ;; �����eof���֤�
             ((condition-has-type? e <io-closed-error>) (read-from-string ""))
             ;; ����¾��io���顼�⡢��Χ��eof���֤����ˤ���(��)
             ((condition-has-type? e <io-error>) (read-from-string ""))
             ;; ����ʳ��ξ��ϡ����Τޤޥ��顼���֥������Ȥ��֤�
             ;; (message��������̿��Ǥ�ȯ���������ϡ�
             ;;  message->event�����顼���֥������Ȥ��֤�(�ޤ��ϡ�
             ;;  ���ޤ��ޥ��顼�ˤʤ餺��������(�Դ�����)event���֤�)�Τ�
             ;;  ������ꡢ���줬���塼�����ä��塢
             ;;  ����³����eof�����塼��������ˤʤ�)
             (else e))
      (let1 message (read-line (irc-input-port-of self))
        (if (eof-object? message)
          message ; eof
          (message->event ; ������event�Ѵ�����
            (string-incomplete->complete
              (ces-convert message
                           (irc-server-encoding-of self)
                           (gauche-character-encoding))
              :omit))))))

  ;; ����selector��port���ɤ߼���ޤ��Ԥİ٤����Τ�ΤʤΤǡ�
  ;; �ϥ�ɥ鼫�Τϲ��⤷�ʤ��Ƥ�褤
  (let1 selector (make <selector>)
    (selector-add! selector (irc-input-port-of self) (lambda (p f) #t) '(r x))
    (let loop ((exit? #f))
      ;; ��������ǽ�ʥǡ��������Ƽ���
      (with-port-locking
        (irc-input-port-of self)
        (lambda ()
          (let next ()
            (if (not (byte-ready? (irc-input-port-of self)))
              #f
              (let1 event (fetch-event)
                ;; event��PING��433�ξ�硢���塼�ˤ���¸�����ˡ�
                ;; �����Ǽ�ưŪ�˱�����Ԥ�ɬ�פ�����
                ;; (���������ˤ�뼫ư�������ϡ�laststatus���ѹ����ʤ���)
                (logging self event) ; �������Ƥ�event���Ф��Ƽ��
                (cond
                  ((eof-object? event) ; eof
                   (set! (irc-finished?-of self) #t)
                   (irc-recv-enqueue! self event)
                   (set! exit? #t)) ; ��λ
                  ((condition? event) ; ���顼���֥�������
                   (irc-recv-enqueue! self event)
                   (next)) ; ³��
                  (else ; �̾�message
                    (when (recv-update-pib! self event)
                      (irc-recv-enqueue! self event))
                    (next)))))))) ; ³��
      ;; ���λ�����exit�ե饰��Ω�äƤ���ʤ齪λ
      (unless exit?
        ;; ���μ����ǡ�����select���Ԥ�
        ;; TODO: �����Ͼ��衢cthreads���б������ݤˡ�thread-select!�ڤӡ�
        ;;       �����Ʊ���������֤���������
        (selector-select selector)
        ;; selector��ȿ��������Ƽ¹�
        (loop #f)))))

(define-method %irc-send-thread ((self <pib>))
  (let loop ()
    (mutex-lock! (irc-send-cv-mutex-of self))
    (let* ((event (and-let* ((e (irc-send-dequeue! self)))
                    (send-event-split-last-param e)))
           (message (and
                      event ; event��#f�ʤ�message��#f�ˤ���
                      (ces-convert (event->message event #t)
                                   (gauche-character-encoding)
                                   (irc-server-encoding-of self))))
           (sent-usec #f) ; ���Ȥ����ꤵ���
           )
      (cond
        ;((eq? message 'shutdown) #f) ; ��λ(�쥳����)
        ((not message) ; ���塼�������ä�(cv�����ʥ���Ԥ�)
         (mutex-unlock! (irc-send-cv-mutex-of self) (irc-send-cv-of self))
         (loop)) ; cv�����ʥ���������塼�����å����ʳ�����Ƽ¹Ԥ���
        (else ; �̾�message���ä�
          ;; �ޤ���(irc-send-last-microsec-of self)������å�����
          ;; ������ְ���ʤ��Ԥ�
          (when (flood-protection-interval-of self)
            (let1 remain (-
                           (+
                             (irc-send-last-microsec-of self)
                             (flood-protection-interval-of self))
                           (gettimeofday-usec))
              (when (positive? remain)
                (dynamic-wind
                  (lambda ()
                    ;; ��ö�����å�����
                    (mutex-unlock! (irc-send-cv-mutex-of self)))
                  (lambda ()
                    ;; �Ԥ�
                    ;; TODO: �����Ͼ��衢cthreads���б������ݤˡ�
                    ;;       thread-select!�ڤӡ�
                    ;;       �����Ʊ���������֤��������롩
                    (selector-select (make <selector>) remain))
                  (lambda ()
                    ;; ���٥�å�����
                    (mutex-lock! (irc-send-cv-mutex-of self)))))))
          ;; ��������
          (guard (e (else
                      (set! (irc-send-laststatus-of self) 'error)
                      (set! exit? #t)
                      (set! sent-usec (gettimeofday-usec))))
            (with-port-locking
              (irc-output-port-of self)
              (lambda ()
                ;; ����ľ���ˡ�pib���֥��������������ѹ���ڤܤ�command�ʤ顢
                ;; ����餷���ѹ���Ԥ�ɬ�פ�����
                (send-update-pib! self event)
                ;; �����Ѵ��ѤʤΤǡ����Τޤ������
                (display message (irc-output-port-of self))
                (flush (irc-output-port-of self))))
            (let1 usec (gettimeofday-usec)
              ;; ����¾�ι��ܤ����ꤹ��
              (set! (irc-send-last-microsec-of self) usec)
              (set! (irc-send-laststatus-of self) 'ok)
              (set! sent-usec usec)))
          ;; ������
          (let1 sent-event (list* (usec->timestamp sent-usec)
                                  'send
                                  #f
                                  (cdddr event))
            (logging self sent-event))
          ;; �����å�����
          (mutex-unlock! (irc-send-cv-mutex-of self))
          (loop))))))

;; TODO: ̾����Ŭ�ڤǤʤ������Ȥǹͤ�ľ����
;; ���μ�³���ϡ�pib�Υ���åȤ򹹿����Ĥġ�enqueue���٤����ɤ����򿿵��ͤ��֤�
(define (recv-update-pib! self event)
  (let1 command (event->command event)
    ;; 433, 451�ʳ���������Ϥ᤿�顢irc�����Ф���registered���줿��ΤȤ���
    ;; (�ºݤϡ�register����NOTICE�����Τ��Ƥ���irc�����������뤬�������դ��
    ;;  �����å������ϡ�with-irc���dynamic-wind��before thunk���
    ;;  �������ʬ�ǰ�ö����������äƤ��顢
    ;;  ���θ�˥ե饰���ö���ꥢ���Ƥ�����ǡ�
    ;;  ����Ū�˲��򤷤Ƥ��롣����ä�ʬ����ˤ���)
    (when (and
            (not (irc-registered?-of self))
            (not (eqv? command 433))
            (not (eqv? command 451)))
      (set! (irc-registered?-of self) #t))
    (cond
      ((eq? command 'NICK)
       ;; prefix����ʬ���Ƥʤ顢current-nick���ѹ�����
       ;; channel��(get-current-nick)��Ʊ�����Ĥޤ꼫ʬ���Ȱ��Ƥʤ顢
       ;; prefix�����ֿ�������
       ;; prefix�ν񼰤ϡ�"nick!~username@hostname"
       ;; ��äơ�!���������ʬ����Ф��������ɤ�
       (when (equal?
               (current-nick-of self)
               (and-let* ((prefix (event->prefix event))
                          (matched (#/\!/ prefix)))
                 (matched 'before)))
         (guard (e (else e))
           (set! (current-nick-of self) (car (event->params event)))))
       #t) ; enqueue����
      ((eq? command 'PING) ; PING/��ư����
       (irc-send-enqueue!
         self `(#f #f #f PONG ,@(event->params event)))
       #f) ; enqueue���ʤ�
      ((eqv? command 433) ; NICK�ѹ�����/��ư����
       (let1 next-nick (generate-next-nick (base-nick-of self))
         (irc-send-enqueue!
           self `(#f #f #f NICK ,next-nick)))
       #f) ; enqueue���ʤ�
      (else
        #t)))) ; enqueue����

(define (send-update-pib! self event)
  ;; ���ΤȤ����ѹ���ɬ�פʤΤ�NICK�Τ�
  (let1 command (event->command event)
    (cond
      ((eq? command 'NICK)
       (guard (e (else e))
         (let1 nick (car (event->params event))
           (unless (irc-registered?-of self)
             (set! (current-nick-of self) nick))
           (set! (base-nick-of self) nick))))
      (else
        #t))))


(define (usec->timestamp usec)
  (date->string
    (time-utc->date
      (seconds->time
        (quotient usec 1000000)))))

(define (gettimeofday-usec)
  (receive (sec usec) (sys-gettimeofday)
    (+ (* sec 1000000) usec)))


(define-method logging ((self <pib>) event)
  (when (logging-handler-of self)
    (with-locking-mutex
      (irc-logger-mutex-of self)
      (lambda ()
        ((logging-handler-of self) event)))))


(define (make-thread/report-error thunk)
  (make-thread
    (lambda ()
      (guard (e (else
                  (display
                    (call-with-output-string
                      (lambda (p)
                        (with-error-to-port p (cut report-error e))))
                    (standard-error-port))
                  (raise e)))
        (thunk)))))


(define (with-irc irc-server-ip ; "111.222.33.44" ����ʸ����
                  irc-server-port ; 6667 ���ο���
                  . keywords+main-thunk)
  (let ((keywords (drop-right keywords+main-thunk 1))
        (main-thunk (last keywords+main-thunk)))
    (let-keywords keywords ((irc-server-encoding "utf-8")
                            (irc-server-pass #f)
                            (thread-type 'gauche.threads)
                            (base-nick "pib") ; ��Ĺ9ʸ���餷��
                            (username #f)
                            (realname #f)
                            (flood-protection-interval 900000)
                            (logging-handler #f)
                            )
      ;; ������̤����κݤδ�ñ�ʼ�ư����
      (unless username
        (set! username base-nick))
      (unless realname
        (set! realname username))
      ;; �����δ�ñ�ʥ����å�
      (unless (valid-nick? base-nick)
        (error "invalid nick" base-nick))
      ;; TODO: ¾���ͤ�����å������

      (let1 pib (make <pib>
                      :irc-server-ip irc-server-ip
                      :irc-server-port irc-server-port
                      :irc-server-encoding irc-server-encoding
                      :irc-server-pass irc-server-pass
                      :thread-type thread-type
                      :base-nick base-nick
                      :username username
                      :realname realname
                      :main-thunk main-thunk
                      :flood-protection-interval flood-protection-interval
                      :logging-handler logging-handler
                      )
        (parameterize ((param:pib pib))
          (dynamic-wind
            (lambda ()
              ;; pib�������ѥ���åȤ��ͤ����ꤹ��
              (set! (current-nick-of pib) base-nick)
              (set! (irc-socket-of pib)
                (make-client-socket 'inet irc-server-ip irc-server-port))
              (set! (irc-input-port-of pib)
                (socket-input-port (irc-socket-of pib) :buffering :modest))
              (set! (irc-output-port-of pib)
                (socket-output-port (irc-socket-of pib) :buffering :line))
              (set! (irc-recv-thread-of pib)
                (make-thread/report-error (lambda ()
                                            (%irc-recv-thread pib))))
              (set! (irc-send-thread-of pib)
                (make-thread/report-error (lambda ()
                                            (%irc-send-thread pib))))
              (set! (irc-recv-queue-of pib) (make-queue))
              (set! (irc-send-queue-of pib) (make-queue))
              (set! (irc-recv-queue-mutex-of pib)
                (make-mutex "recv-queue"))
              (set! (irc-send-queue-mutex-of pib)
                (make-mutex "send-queue"))
              (set! (irc-recv-cv-of pib)
                (make-condition-variable "recv"))
              (set! (irc-send-cv-of pib)
                (make-condition-variable "send"))
              (set! (irc-recv-cv-mutex-of pib)
                (make-mutex "recv-cv"))
              (set! (irc-send-cv-mutex-of pib)
                (make-mutex "send-cv"))
              (set! (irc-send-laststatus-of pib) 'ok)
              (set! (irc-send-last-microsec-of pib) 0)
              (set! (irc-logger-mutex-of pib)
                (make-mutex "logger"))
              (set! (irc-registered?-of pib) #f)
              (set! (irc-finished?-of pib) #f)
              ;; ����¾�ν����������Ԥ�
              (thread-start! (irc-recv-thread-of pib))
              (thread-start! (irc-send-thread-of pib))
              ;; ��³ľ��������Ƥ�����å���������̤��������Τ��Ԥ�
              (thread-sleep! 2)
              ;; ���̤���������顢��öirc-registered?��#f���᤹
              (set! (irc-registered?-of pib) #f)
              ;; PASS, NICK, USER���ޥ�ɤ��̤�
              (when (irc-server-pass-of pib)
                (%irc-send-event! pib
                                  `(#f #f #f PASS ,(irc-server-pass-of pib))))
              (%irc-send-event! pib `(#f #f #f NICK ,(base-nick-of pib)))
              (%irc-send-event! pib `(#f
                                      #f
                                      #f
                                      USER
                                      ,(username-of pib)
                                      "0.0.0.0"
                                      "0.0.0.0"
                                      ,(realname-of pib)))
              ;; TODO: join���Ƥ�������ͥ�⡢����������Ū�˰����������ɤ���
              #t)
            (lambda ()
              (let loop ()
                (unless (irc-registered?-of pib)
                  (thread-sleep! 1)
                  (loop)))
              (%irc-main pib))
            (lambda ()
              ;; �����åȤ���߽�����Ԥ�
              ;; (��������åɤ���ߤ����Τ���ư����ͤƤ���)
              (with-port-locking
                (irc-input-port-of pib)
                (lambda ()
                  (with-port-locking
                    (irc-output-port-of pib)
                    (lambda ()
                      (ignore-error #f (close-input-port
                                         (irc-input-port-of pib)))
                      (ignore-error #f (close-output-port
                                         (irc-output-port-of pib)))
                      (ignore-error #f (socket-shutdown (irc-socket-of pib) 2))
                      (ignore-error #f (socket-close (irc-socket-of pib)))))))
              ;; �ҥ���åɤ���߽�����Ԥ�
              (ignore-error #f (thread-join! (irc-recv-thread-of pib) 1))
              (ignore-error #f (thread-terminate! (irc-recv-thread-of pib)))
              (ignore-error #f (thread-join! (irc-send-thread-of pib) 1))
              (ignore-error #f (thread-terminate! (irc-send-thread-of pib)))
              ;; TODO: ¾�ˤ�Ԥ��٤�����������ΤǤϡ�
              ;; ǰ�ΰ١�����åȤ�������Ƥ���
              (set! (current-nick-of pib) #f)
              (set! (irc-socket-of pib) #f)
              (set! (irc-input-port-of pib) #f)
              (set! (irc-output-port-of pib) #f)
              (set! (irc-recv-thread-of pib) #f)
              (set! (irc-send-thread-of pib) #f)
              (set! (irc-recv-queue-of pib) #f)
              (set! (irc-send-queue-of pib) #f)
              (set! (irc-recv-queue-mutex-of pib) #f)
              (set! (irc-send-queue-mutex-of pib) #f)
              (set! (irc-recv-cv-of pib) #f)
              (set! (irc-send-cv-of pib) #f)
              (set! (irc-recv-cv-mutex-of pib) #f)
              (set! (irc-send-cv-mutex-of pib) #f)
              (set! (irc-send-laststatus-of pib) #f)
              (set! (irc-send-last-microsec-of pib) #f)
              (set! (irc-logger-mutex-of pib) #f)
              (set! (irc-registered?-of pib) #f)
              (set! (irc-finished?-of pib) #f)
              #t)))))))



;;; ----


;; NB: ����event�ȼ���event�ǡ������褬�㤦��Τ�¸�ߤ��롩
;; NB: �֤��ͤ�list�Ǥ��������դ����(#f�λ���Τ�����)
(define (event->logging-names event)
  (let ((prefix (event->prefix event))
        (mode (event->mode event))
        (command (event->command event))
        (params (event->params event))
        )
    (let/cc return
      (define (channel? entity)
        (#/^(?:\#|\&)/ entity))
      (define (split-channels entity)
        (string-split entity #\,))
      (define (find-channel-from-params)
        (or
          (find
            (cut channel? <>)
            params)
          (return #f)))

      (let1 entity (case command
                     ((PRIVMSG NOTICE) ; channel�ޤ���nick����
                      ;; ����event�����оݤ�channel�ǤϤʤ�nick���ä���硢
                      ;; ����ϼ�ʬ���ȤȤʤäƤ��ޤ��Τǡ����λ���
                      ;; prefix�����оݤ���ɬ�פ�����
                      (let1 target (car params)
                        (if (or
                              (eq? mode 'send)
                              (channel? target))
                          target
                          (or
                            (and-let* ((prefix (event->prefix event))
                                       (matched (#/\!/ prefix)))
                              (matched 'before))
                            (return #f)))))
                     ((JOIN PART TOPIC KICK) ; channel����
                      (car params))
                     ((INVITE) ; INVITE�����̰�������
                      ;; NB: INVITE��car����ʤ���ͳ:
                      ;; - car������ϡ���̾=���������¹Լ�̾�Ȥ������
                      ;; - cadr������ϡ���̾=where�����Ȥ������
                      ;; PRIVMSG�ˤ��nickľ�̤β��ä���send��recv��
                      ;; ���nick�ΤȤ���˥��󥰤������ˤˤʤäƤ���١�
                      ;; INVITE�⤳��˹�碌�ơ���Ԥβ������ΤȤ��롣
                      (cadr params))
                     ((MODE) ; channel�ޤ���nick����
                      (let1 target (car params)
                        ;; target��channel�ʤ顢���Τޤ޺��Ѥ��Ƥ褤
                        ;; target��nick�ʤ顢#f���֤�(��ʬ���ȤΤߤ�Ȧ�ʤΤ�)
                        (unless (channel? target)
                          (return #f))
                        target))
                     ((NAMES LIST)
                      ;; �ͤ�����̡�NAMES, LIST��#f���֤����ˤ���
                      (return #f))
                     ((QUIT)
                      ;; TODO: QUIT�������Ƥ�����硢����nick��join���Ƥ���
                      ;;       channel���Ƥ˥���Ĥ��Τ�better��
                      ;;       �����������ΤȤ����üԸ��̤�channel���þ����
                      ;;       ���äƤ��ʤ��Τǡ�����ϤǤ��ʤ���
                      ;;       �⤷����˰����褦�ˤʤä��顢�������Ƥ�褤
                      (return #f))
                     ;; �ʲ�������reply�б�
                     ;; TODO: ¾�ˤ��ɲä��٤�����reply¿��
                     ;; nick�֤���
                     ((301) (car params)) ; RPL_AWAY
                     ;; channel�֤���
                     ((321 322 323) ; RPL_LIST*
                      (find-channel-from-params))
                     ((324) ; RPL_CHANNELMODEIS
                      (find-channel-from-params))
                     ((331 332) ; RPL_NOTOPIC, RPL_TOPIC
                      (find-channel-from-params))
                     ((341 342) ; RPL_INVITING, RPL_SUMMONING
                      (find-channel-from-params))
                     ((353 366) ; RPL_NAMEREPLY, RPL_ENDOFNAME
                      (find-channel-from-params))
                     (else
                       (return #f)))
        ;; entity�ˤϡ�channel(s)�ޤ���nick������
        ;; channel(s)�ξ���ʬ�䤹��ɬ�פ�����
        (if (channel? entity)
          (split-channels entity)
          (list entity)))))) ; �֤��ͤ�list�ˤ���ɬ�פ�����

;; with-irc�ǰ���event�����Υ��٥�Ȥ���󥰤���桼�ƥ���ƥ���³��
;; (��Ψ�ϰ���)
;; �����ͥ��̤Υǥ��쥯�ȥ꤬���졢����������̤Υե����뤬���졢������
;; (write event)����Ԥ����ɵ������
;; �����ξܺ٤ˤĤ��Ƥϰʲ����̤�
;; - log-dir�ϥ��ե��������������ǥ��쥯�ȥ�(�ץե�������������)
;; - event������event�ޤ��ϼ���event
;; - global-dirname�ϡ�MOTD���Ρ������ͥ��������̵��event����¸����
;;   �ǥ��쥯�ȥ����ꤹ��(�ǥե���Ȥ�"AUTH")
(define (make-basic-irc-logger log-dir . opt-global-dirname)
  (let1 global-dirname (get-optional opt-global-dirname "AUTH")
    ;; ���٥�Ȥ������ꡢ���󥰤�Ԥ���³�����֤�
    (lambda (event)
      (let1 target-dirs (guard (e (else (list global-dirname)))
                          ;; event��event�񼰤ǤϤʤ���eof��error�ʤ顢
                          ;; global�˵�Ͽ������
                          (unless (list? event)
                            (error "!"))
                          ;; �����command�ʤ顢���󥰼��Τ�Ԥ�ʤ�
                          ;(when (eq? (event->command event) 'PASS) '())
                          ;; USER���ޥ���̲����Ͼ��global-dirname�Ȥ���
                          (unless (irc-registered?-of (param:pib))
                            (error "!"))
                          ;; target���������
                          ;; (�����Ǥ��ʤ��ä�����global-dirname�ˤ���)
                          (let1 targets (or
                                          (event->logging-names event)
                                          (error "!"))
                            (map
                              (lambda (t)
                                ;; �������ʸ����ʤ���̾����
                                (if (equal? t "")
                                  "_"
                                  ;; "."��"/"��ޤޤʤ��褦���ִ�
                                  (regexp-replace #/\.|\// t "_")))
                              targets)))
        ;; target-dirs�ο�����������Ԥ�
        (for-each
          (lambda (target-dir)
            (let* ((dir (build-path log-dir target-dir))
                   (date (guard (e (else (current-date)))
                           (string->date (event->timestamp event))))
                   (file (date->string date "~Y-~m-~d.log"))
                   (path (build-path dir file))
                   )
              (make-directory* dir)
              (sys-unlink (build-path dir "latest"))
              (sys-symlink file (build-path dir "latest"))
              (with-output-to-file
                path
                (lambda ()
                  (write event)
                  (newline))
                :if-exists :append
                :buffering :full)))
          target-dirs)))))

(provide "pib")

