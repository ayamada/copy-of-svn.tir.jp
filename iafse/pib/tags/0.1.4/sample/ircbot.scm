#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; basis on http://homepage3.nifty.com/oatu/gauche/try.html#ircbot

;;; �Ȥ���:
;;; - �ʲ��Υѥ�᡼����Ȥ������褦���ѹ�����
;;; - �¹Ԥ���(�����餯���ʲ��Τ褦�ʥ��ޥ�ɤˤʤ�)
;;;   ./ircbot.scm > stdout.log 2>&1 &
;;; - �¹Ԥ���ȡ�S���񼰤Υ���*log-dir*�ˡ������ͥ��̡����̤���¸�����
;;; - �¹Ԥ���ȡ�*pipe-file*�����������Τǡ�������
;;;   echo 'PRIVMSG #hoge :�ۤ��ۤ�'
;;;   �Τ褦��ʸ�����ή������ȡ�bot�����Υ��ޥ�ɤ򥵡��Ф���������
;;;   ή������ݤ����ܸ쥳���ɤϡ�(gauche-character-encoding)�ˤ����
;;;   irc���ޥ�ɤξܺ٤ϡ�rfc���ǧ�����(�ä˰����ν���)
;;;   RFC: http://www.haun.org/kent/lib/rfc1459-irc-ja.html


(add-load-path "../lib")
(add-load-path "lib")
(add-load-path ".")

(use pib)
(use gauche.threads)

(define *irc-server-ip* "localhost")
(define *irc-server-port* 6667)
(define *irc-server-encoding* "iso-2022-jp")
(define *irc-server-pass* #f)
(define *nick* "ircbot")

(define *pipe-file* "ircbot.pipe")
(define *log-dir* "ircbot.logs")
(define *channels* '("#pib1" "#pib2"))

;;; ----


(define (write-error-to-stderr e)
  (write e (standard-error-port))
  (newline (standard-error-port)))


(define (irc-main)
  (let loop ()
    (call-with-input-file
      *pipe-file*
      (lambda (pipe)
        (let loop2 ()
          (let1 line (guard (e (else (read-from-string "")))
                       (read-line pipe))
            (unless (eof-object? line)
              (let1 event (guard (e (else e))
                            (message->event line #t))
                (if (condition? event)
                  (write-error-to-stderr event)
                  (irc-send-event! event)))
              (loop2)))))
      :buffering :modest)
    (loop)))


(define (recv-thunk)
  (let1 event (irc-recv-event! #f)
    (cond
      ((eof-object? event) #t) ; ��λ
      ((condition? event) ; stderr�˽��Ϥ���³��
       (write-error-to-stderr event)
       (thread-sleep! 1)
       (recv-thunk))
      (else ; event��ΤƤ�³��
        (recv-thunk)))))


(define (main-thunk)
  ;; �ǽ�ˡ�JOIN����
  (for-each
    (lambda (ch)
      (irc-send! `("JOIN" ,ch)))
    *channels*)
  ;; irc����message����������ΤƤ����ѤΥ���åɤ��Ѱդ���
  ;; (���󥰤�:logging-handler���Ԥ�)
  (let1 recv-thread (make-thread/report-error recv-thunk)
    (thread-start! recv-thread)
    (irc-main)))


(define (main args)
  (unless (file-exists? *pipe-file*)
    (sys-mkfifo *pipe-file* (+ (* 64 (+ (* 1 0) (* 2 1) (* 4 1)))
                               (*  8 (+ (* 1 0) (* 2 0) (* 4 0)))
                               (*  1 (+ (* 1 0) (* 2 0) (* 4 0))))))
  (with-irc
    *irc-server-ip*
    *irc-server-port*
    :irc-server-encoding *irc-server-encoding*
    :irc-server-pass *irc-server-pass*
    :base-nick *nick*
    :logging-handler (make-basic-irc-logger *log-dir*)
    main-thunk))



