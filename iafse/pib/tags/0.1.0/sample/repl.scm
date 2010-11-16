#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; basis on http://homepage3.nifty.com/oatu/gauche/try.html#ircbot

;;; �����:
;;; ���Υ�����ץȤ����˴��Ǥ���
;;; �������ƥ��ۡ���ˤʤ�ޤ���
;;; �褷�ơ��°פ�����Τޤ޻��Ѥ��ʤ��Ǥ���������

;;; �Ȥ���:
;;; - �ʲ��Υѥ�᡼����Ȥ������褦���ѹ�����
;;; - �¹Ԥ���(�����餯���ʲ��Τ褦�ʥ��ޥ�ɤˤʤ�)
;;;   ./repl.scm > stdout.log 2>&1 &
;;; - bot��join���Ƥ�������ͥ롢�ޤ���ľ��bot��PRIVMSG�ǡ�
;;;   ��(eval! '(+ 1 2))�פΤ褦�ʥƥ����Ȥ�����ȡ�ɾ����Ԥ��������֤��ͤ�
;;;   �ֿ����Ƥ����
;;;   (����ɾ���κݤ�prefix/suffix���ѹ���ǽ���㤨�С�botɾ����(+ 1 2)����)


(add-load-path "../lib")
(add-load-path "lib")
(add-load-path ".")

(use pib)
(use gauche.interactive)

(define *irc-server-ip* "localhost")
(define *irc-server-port* 6667)
(define *irc-server-encoding* "iso-2022-jp")
(define *irc-server-pass* #f)
(define *nick* "repl")

(define *log-dir* "ircbot.logs")
(define *channels* '("#pib1" "#pib2"))
(define *allowed-hosts* '("localhost"))

;; (eval! '(+ 1 2)) �Τ褦�ʥƥ����Ȥ�����ȡ�ɾ�����Ƥ����
(define *expr-prefix* "(eval! '")
(define *expr-suffix* ")")
(define *get-expr-string-re*
  (string->regexp
    (string-append
      "^"
      (regexp-quote *expr-prefix*)
      "(.*)"
      (regexp-quote *expr-suffix*)
      "$")))


;; get-expr-string�ϡ�irc��¾�ͤ�ȯ����ʸ���������˼�ꡢ
;; ��������ɾ�����٤�ʸ�����������Ф�����#f���֤��������³��
(define (get-expr-string msg)
  (and-let* ((match (*get-expr-string-re* msg)))
    (match 1)))


;; allowed-prefix-pred�ϡ�irc��prefixʸ���������˼�ꡢ
;; ���줬eval�����Ĥ��줿��ʪ�Ǥ���ʤ�#t�򡢤����Ǥʤ��ʤ�#f���֤���³��
(define (allowed-prefix-pred prefix)
  (let/cc return
    (unless prefix
      (return #f))
    ;; prefix�ϡ�"repl!~repl@fqdn" �Τ褦��ʸ����ʤΤǡ��ޤ��Ϥ����ʬ�򤹤�
    (and-let* ((match (#/^(.*?)\!(.*?)\@(.*?)$/ prefix))
               (nick (match 1))
               (username/tilda (match 2)) ; ident�ˤ��뤬����Ƭ��~���Ĥ�
               (hostname (match 3))
               )
      ;; ��äȺ٤������¥����å���ԤäƤ�褤
      (for-each
        (lambda (allowed-host)
          (when (equal? hostname allowed-host)
            (return #t)))
        *allowed-hosts*)
      #f)))


;;; ----

;; ���μ�³���ϡ�text���Ѵ����512byte��ۤ��Ƥ����顢
;; ���������ڤ�ͤ������륵�������Ѵ�����
(define (irc-privmsg/trim channel text)
  (guard (e
           ((equal? (ref e 'message) "message too long")
            (irc-privmsg/trim channel
                              (string-copy text 0 (- (string-length text) 2))))
           (else (error e)))
    (irc-send!
      `("NOTICE" ,channel ,text))))


;; ľ��nick��PRIVMSG������줿��硢�����֤���򤽤Τޤ�
;; (car (event->params event))�Ƿ��ꤹ��ȼ�ʬ���ȤˤʤäƤ��ޤ��١�
;; ���ξ�������nick������ɬ�פ����롣
;; �������μ�³����Ƥ֤Τ�doit�����ʤΤǡ�command��PRIVMSG�Ȥ����Τ�
;; ���˳��ꤷ�Ƥ���١������å���ʤ��Ƥ��롣
;; (��¤�����ꡢ�����ɤ�ή�Ѥ���ݤ����)
(define (get-channel event)
  (let1 channel (car (event->params event))
    (if (not (equal? channel (get-current-nick)))
      channel
      ((#/\!/ (event->prefix event)) 'before))))

(define (doit event)
  ;; prefix��eval����Ĥ��줿�ͤΤߡ�������ν�����Ԥ�
  (when (allowed-prefix-pred (event->prefix event))
    ;; ��å��������鼰��ʬ����Ф�
    (let* ((channel (get-channel event))
           (msg (cadr (event->params event)))
           (expr-string (get-expr-string msg))
           )
      ;; expr-string�μ��������������顢³�Ԥ���
      (when expr-string
        ;; ����������ϼ��Ԥ����ǽ��������Τǡ�guard���Ƥ���
        (guard (e (else
                    (irc-privmsg/trim channel (write-to-string e))))
          ;; read����
          (let1 sexpr (read-from-string expr-string)
            ;; eval����
            ;; TODO: eval��¹Ԥ���Ķ��ϡ���ǽ�ʤ�eval/sv���Ρ�
            ;;       �����ʴĶ��ˤ��������ɤ�
            (receive results (eval sexpr (current-module))
              ;; ¿�ͤϡ�ʣ���Υ�å���������������б�������ˤ���
              (for-each
                (lambda (x)
                  ;; print����
                  (irc-privmsg/trim channel (write-to-string x)))
                results))))))))

(define (main-thunk)
  ;; �ǽ�ˡ�JOIN����
  (for-each
    (lambda (ch)
      (irc-send! `("JOIN" ,ch)))
    *channels*)
  (let/cc finish
    (let loop ()
      (let1 event (irc-recv-event! 1)
        (cond
          ((not event) #f) ; ���٥��̵��
          ((eof-object? event) (finish)) ; ��³��
          ((equal? (event->command event) "PRIVMSG")
           (doit event))
          (else #t)))
      (loop))))


(define (main args)
  (with-irc
    *irc-server-ip*
    *irc-server-port*
    :irc-server-encoding *irc-server-encoding*
    :irc-server-pass *irc-server-pass*
    :base-nick *nick*
    :logging-handler (make-basic-irc-logger *log-dir*)
    main-thunk))



