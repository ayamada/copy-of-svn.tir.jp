#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; basis on http://homepage3.nifty.com/oatu/gauche/try.html#ircbot

;;; �Ȥ���:
;;; - �ʲ��Υѥ�᡼����Ȥ������褦���ѹ�����
;;; - �¹Ԥ���(�����餯���ʲ��Τ褦�ʥ��ޥ�ɤˤʤ�)
;;;   ./logger.scm > stdout.log 2>&1 &
;;; - �¹Ԥ���ȡ�S���񼰤Υ���*log-dir*�ˡ������ͥ��̡����̤���¸�����
;;; - INVITE�����ȡ����Υ����ͥ������


(add-load-path "../lib")
(add-load-path "lib")
(add-load-path ".")

(use pib)

(define *irc-server-ip* "localhost")
(define *irc-server-port* 6667)
(define *irc-server-encoding* "iso-2022-jp")
(define *irc-server-pass* #f)
(define *nick* "logger")

(define *log-dir* "ircbot.logs")
(define *channels* '("#pib1" "#pib2"))

;;; ----



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
          ;; invite�Τ߽������롢���Ȥϥ��롼
          ((equal? (event->command event) "INVITE")
           (irc-send! `("JOIN" ,(cadr (event->params event)))))
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



