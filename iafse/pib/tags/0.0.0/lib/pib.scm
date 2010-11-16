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
;;;   (lambda (pib)
;;;     (send-message! pib '(#f "JOIN" "#channel")) ; join���ޥ�ɤ�¹�
;;;     (let loop ()
;;;       (let1 event (receive-message! pib 1) ; timeout��1�ä�message����
;;;         (when event
;;;           (write event)
;;;           (newline))
;;;         (loop)))))
;;; - ���Ȥǡ�send-message!��receive-message!�ΰ����ܺ٤��
;;; �����:
;;; - with-irc����������η�³/���顼æ�Фϲ�ǽ�Ǥ�����
;;;   �����ؤη�³��é����ˤ��б����Ƥ��ޤ���
;;; - receive-message!��send-message!�ǥ֥�å����Ƥ������ϡ�
;;;   �����ʥ뤬��������ޤ���
;;;   (�Ĥޤꡢctrl+c��SIGTERM�ǽ�λ�������ʤ��ʤ�Ȥ������Ǥ�)
;;;   ��ǽ�ʤ顢��֥�å��⡼�ɤˤ����ꡢû���ܤ�timeout�����ꤷ��
;;;   �����֤������������Ǥ���


;;; TODO: encoding�Ѵ�port��close���줿���ˡ�
;;;       selector�˥��٥�����Τ��ԤäƤʤ����Ǥ�����

;;; TODO: IRC�ꥯ�����ȤϺ���510ʸ��+CRLF�餷�������Ȥ������Ĥ����

;;; TODO: ��ǽ�ʤ顢�ͤ�Ŭ�����ɤ����Υ����å��ϡ�slot��set!����ݤ˹Ԥ�����

;;; TODO: flood protection ("Excess Flood")

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
;;; -- send-message!��ˤơ����ޥ�ɤ�ƻ뤷��JOIN/PART/MODE����
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
;;;   ������cv��condition-variable-signal!��Ԥ�
;;; - �ƥ���åɤϡ��������塼�˥ǡ��������äƤ���Τ��Ԥİ٤ˡ�
;;;   ������cv��mutex�򥢥��å�����(�����ॢ���Ȥ�Ǥ����)
;;; - ��������åɤϡ��ǡ�����������ƥ��塼��������Ʊ����
;;;   ������cv��condition-variable-signal!��Ԥ�


(define-module pib
  (use srfi-1)
  (use srfi-13)
  (use gauche.net)
  (use gauche.charconv)
  (use gauche.threads)
  (use gauche.selector)
  (use util.list)
  (use util.queue)
  (use text.tree)

  (export
    with-irc
    <pib>
    send-message!
    receive-message!

    ;; �ʲ��ϡ��桼�ƥ���ƥ���³���Ȥ�����(��)
    message->event
    event->message-for-send
    ))
(select-module pib)


(define-syntax ignore-error
  (syntax-rules ()
    ((ignore-error fallback . bodies)
     (guard (e (else fallback)) . bodies))))


(define (%with-port-locking port thunk)
  ;; �ɤ��⡢with-port-locking���ǥåɥ�å����Ƥ��ޤ��褦�ʤΤǡ�
  ;; ��å����ʤ��褦�ˤ��Ƥߤ�
  ;; TODO: ���Ȥ��к���ͤ����
  (thunk))


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
   (main-proc
     :accessor main-proc-of
     :init-keyword :main-proc
     :init-form (error "must be need main-proc"))
   ;; �������֥���å�(���ơ����Ȥ����ꤵ���)
   (current-nick
     :accessor current-nick-of)
   (irc-socket
     :accessor irc-socket-of)
   (irc-input-port
     :accessor irc-input-port-of)
   (irc-output-port
     :accessor irc-output-port-of)
   (irc-receive-thread
     :accessor irc-receive-thread-of)
   (irc-send-thread
     :accessor irc-send-thread-of)
   (irc-receive-queue
     :accessor irc-receive-queue-of)
   (irc-send-queue
     :accessor irc-send-queue-of)
   (irc-receive-queue-mutex
     :accessor irc-receive-queue-mutex-of)
   (irc-send-queue-mutex
     :accessor irc-send-queue-mutex-of)
   (irc-receive-cv
     :accessor irc-receive-cv-of)
   (irc-send-cv
     :accessor irc-send-cv-of)
   (irc-receive-cv-mutex
     :accessor irc-receive-cv-mutex-of)
   (irc-send-cv-mutex
     :accessor irc-send-cv-mutex-of)
   (irc-send-laststatus
     :accessor irc-send-laststatus-of)
   ;; TODO: flood�к��ѥ���åȤ򤢤Ȥ��ɲä����
   ))

(define-method initialize ((self <pib>) initargs)
  (next-method)
  #t)


;; event�ϡ��ʲ��Τ褦��list�Ȥ��롣
;; '(prefix command . params)
;; - prefix�ϡ�����message��sender������Ǥ���ʸ�����������ˤ�̵�뤵���
;; - command�ϡ�"PRIVMSG"����ʸ���󡢤ޤ��ϻ���ο���
;; - params�ϡ�����ν񼰤�������ʸ�����list(�񼰤ˤĤ��Ƥϡ�rfc����)
(define-method send-message! ((self <pib>) event . opt-sync)
  ;; - opt-sync��#f�ʤ顢������async�˼¹Ԥ��졢������������������ʬ����ʤ�
  ;;   (�ǥե���Ȥ�ư��)
  ;; - opt-sync��#t�ʤ顢������sync�˼¹Ԥ��졢�֤��ͤȤ���������������������
  ;;   �����ͤȤ����֤äƤ��뤬�������������˴�λ����ޤ��Ԥ������
  ;;   (��������ϡ������פ������������ɤ����Ǥ��ꡢ�֥��ޥ�ɼ¹ԡפ�
  ;;    �����������ɤ����ǤϤʤ�������դ�����������ơ������פ����Ԥ��븶����
  ;;    �̾�̿��ǰʳ��ˤ�¸�ߤ��ʤ�)
  ;;   ���Υ����å��μ�����ˡ��̤�ꡣ
  ;;   (�����ȼ������褦�Ȥ���ȡ����ʤ���ä�������busy loop����
  ;;    race condition����̵�¥롼�פ����ǽ�����Ĥ�)
  (irc-send-enqueue! self event)
  (if (get-optional opt-sync #f)
    (error "not implement yet") ; TODO: ���ȤǼ�������
    #t))
(define-method receive-message! ((self <pib>) . opts)
  ;; NB: cv������ʤ顢���˰��פ��ʤ����ˤ�
  ;;     �����å�������ǽ��������١�timeoutȽ���ʣ���ˤʤ뤬��
  ;;     ����ϥ����å�������װ��ϰ�Ĥ����ʤΤǡ�
  ;;     ñ���timeoutȽ���ԤäƤ�褤(¿ʬ)
  (let-optionals* opts ((timeout #f))
    (let loop ()
      (mutex-lock! (irc-receive-cv-mutex-of self))
      (let1 event (irc-receive-dequeue! self)
        (if (or event (equal? timeout 0))
          (begin
            ;; ���塼���ͤ����äƤ��� or timeout��0���ä���
            ;; �����������̤˥����å����ƽ�λ
            (mutex-unlock! (irc-receive-cv-mutex-of self))
            event)
          (begin
            ;; ���塼�����ʤΤǡ������ॢ����ͭ���cv�����ʥ���Ԥ�
            (if (mutex-unlock! (irc-receive-cv-mutex-of self)
                               (irc-receive-cv-of self)
                               timeout)
              (loop) ; cv�����ʥ�������Ƽ��������ͤ��֤�(������NB����)
              #f))))))) ; �����ॢ���Ȥ�����



;; NICK�˼��Ԥ����ݤˡ����˻�Ԥ���NICK�����������³��
(define (generate-next-nick nick)
  (define (nick->num nick)
    (let1 reverse-nick-chars (reverse (string->list nick))
      (let loop ((idx 0)
                 (acc 0))
        (if (<= (length reverse-nick-chars) idx)
          acc
          (let* ((current-char (list-ref reverse-nick-chars idx))
                 (current-char-num (hash-table-get *nickchar->nicknum*
                                                   current-char))
                 (current-figure (expt (vector-length *chars-can-use-nick*)
                                       idx))
                 (delta (* current-char-num current-figure))
                 )
            (loop (+ 1 idx)
                  (+ acc delta)))))))
  (define (num->nick num)
    (let loop ((idx 8)
               (restnum num)
               (reverse-nick-chars '()))
      (if (<= 0 idx)
        (let* ((figure-threshold (expt (vector-length *chars-can-use-nick*)
                                       idx))
               (figure (quotient restnum figure-threshold))
               (char (vector-ref *chars-can-use-nick* figure))
               (next-restnum (remainder restnum figure-threshold))
               )
          (loop (- idx 1)
                next-restnum
                (cons char reverse-nick-chars)))
        (list->string (reverse reverse-nick-chars)))))

  (if (< (string-size nick) 9)
    ;; nick��9ʸ����ã���Ƥ��ʤ��ʤ顢�����˰�ʸ���ɲä���Τ�
    (string-append nick (string (vector-ref *chars-can-use-nick* 0)))
    ;; nick��9ʸ����ã���Ƥ���ʤ顢��öʬ�򤷤ƥ��󥯥���Ȥ��ƺƹ��ۤ���
    (num->nick (remainder (+ 1 (nick->num nick)) *nicknum-max*))))

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
  (expt (vector-length *chars-can-use-nick*) 9))
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
;;
(define (message->event message)
  (guard (e (else e))
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

    ;; prefix�ϡ����˺٤���ʬ�򤷤Ƥ⤤�������Τ�ʤ�
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
                           command))
           (params-true (parse-params params))
           )
      (list* prefix-true
             command-true
             params-true))))

;; event����(S��)�Υǡ�����irc��message�񼰤�ʸ������Ѵ����롣
;; ��������ϥ��饤����Ȥ��������˻Ȥ���ΤȤ�������ΰ١�
;; ���μ�³�����Ѵ����븵event��valid��prefix���ޤޤ�Ƥ��Ƥ⡢
;; message�ϡ�prefix��ޤ�Ǥ��ʤ���Τ��������������ΤȤ��롣
;; (rfc��ꡢprefix��ޤ��message�򥯥饤����Ȥ��饵���Ф����äƤ⡢
;;  ̵�뤵��Ƥ��ޤ���)
;; �������μ�³���ϡ�event�����Ƥ��񼰤�Ŭ�����ɤ����ϥ����å����Ƥ��ʤ��Τǡ�
;; �⤷ɬ�פʤ顢���μ�³�����Ϥ��������Ƥθ��ڤ�Ԥ�ɬ�פ����롣
;; (���Υ����å��Ͽƥ���åɤǹԤä������ɤ��Τǡ�
;;  ���塼��������դ�ǹԤ��Τ��ɤ�)
(define (event->message-for-send event)
  (let* ((prefix (car event))
         (command (cadr event))
         (params (cddr event))
         (params-middles (drop-right params 1))
         (params-trailing (string-append ":" (last params)))
         )
    (tree->string
      (list
        (intersperse " " `(,command ,@params-middles ,params-trailing))
        "\r\n"))))


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
      (<= 1 (length nick-list) 9)
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
  ;; ���Ȥϡ�main-proc��¹Ԥ��ơ��֤��ͤ����(�ޤ��ϥ��顼�ˤʤ�)�Τ��ԤĤ���
  ((main-proc-of self) self))



;; ���塼�����λ���#f���֤�(���塼��#f�����äƤ������̵����ΤȤ���)
(define-method irc-send-dequeue! ((self <pib>))
  (with-locking-mutex
    (irc-send-queue-mutex-of self)
    (lambda ()
      (if (queue-empty? (irc-send-queue-of self))
        #f
        (dequeue! (irc-send-queue-of self))))))
(define-method irc-receive-dequeue! ((self <pib>))
  (with-locking-mutex
    (irc-receive-queue-mutex-of self)
    (lambda ()
      (if (queue-empty? (irc-receive-queue-of self))
        #f
        (dequeue! (irc-receive-queue-of self))))))

(define-method irc-send-enqueue! ((self <pib>) event)
  (with-locking-mutex
    (irc-send-queue-mutex-of self)
    (lambda ()
      (enqueue! (irc-send-queue-of self) event)))
  (condition-variable-signal! (irc-send-cv-of self)))
(define-method irc-receive-enqueue! ((self <pib>) event)
  (with-locking-mutex
    (irc-receive-queue-mutex-of self)
    (lambda ()
      (enqueue! (irc-receive-queue-of self) event)))
  (condition-variable-signal! (irc-receive-cv-of self)))


;; ���μ�³���λ�����:
;; - socket���Ȥ����ǤʤɤϿƥ���åɤޤ�����³�襵���Ф��Ԥ��١�
;;   port���Ф������������˥��顼�㳰��ȯ�������ʤ顢������ǧ����
;;   �⤷�����ʤ顢���顼�ǤϤʤ���³�ǤȤ��ƥ��塼�˥��٥�Ȥ���������
;;   �����ơ����θ������ƥ���åɤǤ��äƤ���³�襵���ФǤ��äƤ⡢
;;   �ʹߤޤȤ��������������Ͻ���ʤ�Ȧ�ʤΤǡ�����åɤ�
;;   ��ʬ���Ȥ�λ���������
;;   (�֤��ͤ��֤����ޤ���thread-terminate!��¹Ԥ���)
(define-method %irc-receive-thread ((self <pib>))
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
        (cond
          ((eof-object? message) message) ; eof
          ((string-incomplete? message)
           (message->event
             (string-incomplete->complete message :omit)))
          (else
            (message->event message))))))

  ;; ����selector��port���ɤ߼���ޤ��Ԥİ٤����Τ�ΤʤΤǡ�
  ;; �ϥ�ɥ鼫�Τϲ��⤷�ʤ��Ƥ�褤
  (let1 selector (make <selector>)
    (selector-add! selector (irc-input-port-of self) (lambda (p f) #t) '(r x))
    (let loop ((exit? #f))
      ;; ��������ǽ�ʥǡ��������Ƽ���
      (%with-port-locking
        (irc-input-port-of self)
        (lambda ()
          (let next ()
            (if (not (char-ready? (irc-input-port-of self)))
              #f
              (let1 event (fetch-event)
                ;; event��PING��433�ξ�硢���塼�ˤ���¸�����ˡ�
                ;; �����Ǽ�ưŪ�˱�����Ԥ�ɬ�פ�����
                ;; (���������ˤ�뼫ư�������ϡ�laststatus���ѹ����ʤ���)
                (cond
                  ((eof-object? event) ; eof
                   (irc-receive-enqueue! self event)
                   (set! exit? #t)) ; ��λ
                  ((condition? event) ; ���顼���֥�������
                   (irc-receive-enqueue! self event)
                   (next)) ; ³��
                  ((equal? (cadr event) "PING") ; PING/��ư����
                   (irc-send-enqueue! self `(#f "PONG" ,(caddr event)))
                   (next)) ; ³��
                  ((eqv? (cadr event) 433) ; NICK�ѹ�����/��ư����
                   (set! (current-nick-of self)
                     (generate-next-nick (current-nick-of self)))
                   (irc-send-enqueue! self
                                      `(#f "NICK" ,(current-nick-of self)))
                   (next)) ; ³��
                  (else ; �̾�message
                    (irc-receive-enqueue! self event)
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
  ;; TODO: flood�к�
  (let loop ()
    (mutex-lock! (irc-send-cv-mutex-of self))
    (let1 event (irc-send-dequeue! self)
      (cond
        ((eq? event 'shutdown) #f) ; ��λ
        ((not event) ; ���塼�������ä�(cv�����ʥ���Ԥ�)
         (mutex-unlock! (irc-send-cv-mutex-of self) (irc-send-cv-of self))
         (loop)) ; cv�����ʥ�������Ƽ¹Ԥ���(receive-message!�Υ����Ȼ���)
        (else ; �̾�event���ä�
          ;; ��������
          (guard (e (else
                      (set! (irc-send-laststatus-of self) 'error)
                      (set! exit? #t)))
            (%with-port-locking
              (irc-output-port-of self)
              (lambda ()
                (display
                  (event->message-for-send event)
                  (irc-output-port-of self))
                (flush (irc-output-port-of self))))
            (set! (irc-send-laststatus-of self) 'ok))
          ;; �����å�����
          (mutex-unlock! (irc-send-cv-mutex-of self))
          (loop))))))



(define (with-irc irc-server-ip ; "111.222.33.44" ����ʸ����
                  irc-server-port ; 6667 ���ο���
                  . keywords+main-proc)
  (let ((keywords (drop-right keywords+main-proc 1))
        (main-proc (last keywords+main-proc)))
    (let-keywords keywords ((irc-server-encoding "utf-8")
                            (irc-server-pass #f)
                            (thread-type 'gauche.threads)
                            (base-nick "pib") ; ��Ĺ9ʸ���餷��
                            (username #f)
                            (realname #f)
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
                      :main-proc main-proc
                      )
        (dynamic-wind
          (lambda ()
            ;; pib�������ѥ���åȤ��ͤ����ꤹ��
            (set! (current-nick-of pib) base-nick)
            (set! (irc-socket-of pib)
              (make-client-socket 'inet irc-server-ip irc-server-port))
            (set! (irc-input-port-of pib)
              (open-input-conversion-port
                (socket-input-port (irc-socket-of pib) :buffering :modest)
                irc-server-encoding
                :to-code (gauche-character-encoding)
                :owner? #t))
            (set! (irc-output-port-of pib)
              (open-output-conversion-port
                (socket-output-port (irc-socket-of pib) :buffering :line)
                irc-server-encoding
                :from-code (gauche-character-encoding)
                :owner? #t))
            (set! (irc-receive-thread-of pib)
              (make-thread (lambda ()
                             (%irc-receive-thread pib))))
            (set! (irc-send-thread-of pib)
              (make-thread (lambda ()
                             (%irc-send-thread pib))))
            (set! (irc-receive-queue-of pib) (make-queue))
            (set! (irc-send-queue-of pib) (make-queue))
            (set! (irc-receive-queue-mutex-of pib)
              (make-mutex "receive-queue"))
            (set! (irc-send-queue-mutex-of pib)
              (make-mutex "send-queue"))
            (set! (irc-receive-cv-of pib)
              (make-condition-variable "receive-cv"))
            (set! (irc-send-cv-of pib)
              (make-condition-variable "send-cv"))
            (set! (irc-receive-cv-mutex-of pib)
              (make-mutex "receive-cv"))
            (set! (irc-send-cv-mutex-of pib)
              (make-mutex "send-cv"))
            (set! (irc-send-laststatus-of pib) 'ok)
            ;; ����¾�ν����������Ԥ�
            (thread-start! (irc-receive-thread-of pib))
            (thread-start! (irc-send-thread-of pib))
            ;; �ޤ��ǽ�ˡ�NICK���ޥ�ɤ�USER���ޥ�ɤ��̤��Ƥ���ɬ�פ�����
            (send-message! pib `(#f "NICK" ,(current-nick-of pib)))
            (thread-sleep! 2)
            (send-message! pib `(#f "USER"
                                 ,(username-of pib)
                                 "0.0.0.0"
                                 "0.0.0.0"
                                 ,(realname-of pib)))
            ;; TODO: join���Ƥ�������ͥ�⡢����������Ū�˰����������ɤ���
            #t)
          (lambda ()
            (%irc-main pib))
          (lambda ()
            ;; �����åȤ���߽�����Ԥ�
            ;; (��������åɤ���ߤ����Τ���ư����ͤƤ���)
            (%with-port-locking
              (irc-input-port-of pib)
              (lambda ()
                (%with-port-locking
                  (irc-output-port-of pib)
                  (lambda ()
                    (ignore-error #f (close-input-port
                                       (irc-input-port-of pib)))
                    (ignore-error #f (close-output-port
                                       (irc-output-port-of pib)))
                    (ignore-error #f (socket-shutdown (irc-socket-of pib) 2))
                    (ignore-error #f (socket-close (irc-socket-of pib)))))))
            ;; ��������åɤΥ��塼�ˡ���λ�����Τ���
            (send-message! pib 'shutdown)
            ;; �ҥ���åɤ���߽�����Ԥ�
            ;; (�̾�ϡ����ν����ǻҥ���åɤ���ߤ��롣
            ;;  ���Υ����ɤϤ����ޤǤ�failsafeŪ�ʰ�̣�Τ��)
            (ignore-error #f (thread-join! (irc-receive-thread-of pib) 1))
            (ignore-error #f (thread-terminate! (irc-receive-thread-of pib)))
            (ignore-error #f (thread-join! (irc-send-thread-of pib) 1))
            (ignore-error #f (thread-terminate! (irc-send-thread-of pib)))
            ;; TODO: ¾�ˤ�Ԥ��٤�����������ΤǤϡ�
            ;; ǰ�ΰ١�����åȤ�������Ƥ���
            (set! (current-nick-of pib) #f)
            (set! (irc-socket-of pib) #f)
            (set! (irc-input-port-of pib) #f)
            (set! (irc-output-port-of pib) #f)
            (set! (irc-receive-thread-of pib) #f)
            (set! (irc-send-thread-of pib) #f)
            (set! (irc-receive-queue-of pib) #f)
            (set! (irc-send-queue-of pib) #f)
            (set! (irc-receive-queue-mutex-of pib) #f)
            (set! (irc-send-queue-mutex-of pib) #f)
            (set! (irc-receive-cv-of pib) #f)
            (set! (irc-send-cv-of pib) #f)
            (set! (irc-receive-cv-mutex-of pib) #f)
            (set! (irc-send-cv-mutex-of pib) #f)
            (set! (irc-send-laststatus-of pib) #f)
            #t))))))



(provide "pib")

