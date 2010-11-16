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

;;; TODO: ���Τ���������åɤ��̤�Ƥ��ޤäơ��ɤ���Ȥ��ˤ����Τǡ�
;;;       �⤦�����ͤ�ľ��ɬ�פ�����

(add-load-path "../lib")
(add-load-path "lib")

(use pib)
(use gauche.threads)
(use file.util)
(use srfi-19)

(define *irc-server-ip* "localhost")
(define *irc-server-port* 6667)
(define *irc-server-encoding* "iso-2022-jp")
(define *irc-server-pass* #f)
(define *nick* "pib")

(define *pipe-file* "ircbot.pipe")
(define *log-dir* "ircbot.logs")
(define *global-dirname* "AUTH") ; �����ͥ�����̵��message��Ͽ����dir
(define *channels* '("#pib" "#pib2"))


;;; ----

(define (logging mode event)
  (let* ((channel (guard (e (else *global-dirname*))
                    (let ((command (cadr event))
                          (channel (caddr event)))
                      ;; command�����ͤʤ顢channel�Ȥϸ������ʤ�
                      (when (number? command)
                        (error "!"))
                      ;; �����command�ʤ顢channel�Ȥϸ������ʤ�
                      (when (equal? command "NICK")
                        (error "!"))
                      (when (equal? command "MODE")
                        (error "!"))
                      (when (equal? command "QUIT")
                        (error "!"))
                      ;; TODO: ¾�ˤ⤳�ξ���command������Ȧ
                      ;; channel����ʸ����"."��".."��"/"��ޤޤʤ������ǧ
                      (set! channel
                        (regexp-replace #/\.|\// channel "_"))
                      ;; channel����ʸ����ʤ���̾����
                      (when (equal? channel "")
                        (set! channel "_"))
                      channel)))
         (dir (build-path *log-dir* channel))
         (date (current-date))
         (file (date->string date "~Y-~m-~d.log"))
         (path (build-path dir file))
         )
    (make-directory* dir)
    (with-output-to-file
      path
      (lambda ()
        (write (list* (date->string date) mode event))
        (newline))
      :if-exists :append
      :buffering :full)))



(define (resolve-pipe pib)
  (let loop ()
    (call-with-input-file
      *pipe-file*
      (lambda (pipe)
        (let loop2 ()
          (let1 line (guard (e (else (read-from-string "")))
                       (read-line pipe))
            (unless (eof-object? line)
              (let* ((event-src (message->event line))
                     ;; ���󥰤ΰ٤ˡ�prefix�򺹤�������ɬ�פ�����
                     ;; TODO: ���Ȥޤ路�ˤ���
                     (event event-src)
                     )
                (send-message! pib event)
                (logging 'send event)
                (loop2))))))
      :buffering :modest)
    (loop)))


(define (main-proc pib)
  ;; �ǽ�ˡ�JOIN����
  (for-each
    (lambda (ch)
      (send-message! pib `(#f "JOIN" ,ch)))
    *channels*)
  ;; pipe����message����Ф��������������ѤΥ���åɤ��Ѱդ���
  (let1 pipe-thread (make-thread (lambda ()
                                   (resolve-pipe pib)))
    (dynamic-wind
      (lambda ()
        (thread-start! pipe-thread))
      (lambda ()
        (let loop ()
          (let1 event (receive-message! pib 1)
            (cond
              ((not event) (loop))
              ((eof-object? event) #t)
              ((condition? event)
               (report-error event)
               (thread-sleep! 1)
               (loop))
              (else
                (logging 'recv event)
                (loop))))))
      (lambda ()
        (thread-terminate! pipe-thread)))))


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
    main-proc))



