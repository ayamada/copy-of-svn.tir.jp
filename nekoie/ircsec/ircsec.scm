#!/usr/local/gauche-0.9/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; irc secretary

;;; ./ircsec.scm > std.log 2>&1 &

;;;   ή������ݤ����ܸ쥳���ɤϡ�(gauche-character-encoding)�ˤ����
;;;   irc���ޥ�ɤξܺ٤ϡ�rfc���ǧ�����(�ä˰����ν���)
;;;   RFC: http://www.haun.org/kent/lib/rfc1459-irc-ja.html

;;; TODO: �ץ饰����ޤ��ϥ��ȥ졼��������ץȤ��ɤ߽񤭵�ǽ��ɬ��
;;; TODO: �ʲ��ν�����Ԥ��ȥꥬ����ɬ��
;;; - �����
;;; - S������

;;; TODO: �̿��Ǥʤɡ����餫�θ���������Ƥ��ޤ������ʤΤǡ��ʲ��Τ褦�ˤ���
;;;       - ¿�ŵ�ư�ɻ߽����������(pid�ե������ߤ��ok)
;;;       - cron�������ư������

;;; TODO: irc.cgi��Ϣ�Ȥ���٤Υ��󥿡��ե���������ꤹ���

;;; NB: plugin.dump���ɤ߹��ޤ��Τϡ�recv-thread���ꡣ
;;;     ¾�Υ���åɤˤϱƶ���ڤܤ��ʤ�

(use pib)
(use gauche.threads)
(use gauche.parameter)

;;; ����ե������ɤ߹���
;;; (public�ˤ��٤��Ǥʤ�����Ϥ��Υե�����˵��Ҥ���)
(define *settings*
  (with-input-from-file
    "settings.txt"
    (lambda ()
      (let loop ((stack '()))
        (let1 sexpr (read)
          (if (eof-object? sexpr)
            (reverse stack)
            (loop (cons sexpr stack))))))))

(define (setting-of key)
  (get-keyword key *settings*))



(define *irc-server-ip* (setting-of :irc-server-ip))
(define *irc-server-port* 6667)
(define *irc-server-encoding* "iso-2022-jp")
(define *irc-server-pass* #f)
(define *nick* "nekoielog")

(define *pipe-file* "./ircsec.pipe")
(define *pid-file* "./ircsec.pid")
(define *log-dir* "./logs")
(define *channels* (setting-of :irc-channels))

(define *plugin-file* "./plugin.dump")


;;; ----


(define (load-plugin)
  (load *plugin-file*
        :paths '(".")
        :error-if-not-found #f
        :environment (current-module)))

;; ���μ�³���ϡ�plugin.dump��ǽ񤭴����Ƥ褤
(define (resolve-event-plugin event)
  #f)

;; ɸ�ൡǽ�ϥץ饰����Υ���ɤΤ�(���ΤȤ����)
(define (resolve-event-primal event)
  (when (eq? 'PRIVMSG (event->command event))
    (let ((reply-to (event->reply-to event))
          (msg (cadr (event->params event)))
          )
      (cond
        ((equal? "(reload!)" msg)
         (guard (e (else
                     (let1 res #`"����ɤ˼��Ԥ��ޤ���(,(ref e 'message))"
                       (irc-send! `(NOTICE ,reply-to ,res)))))
           (let* ((result (load-plugin))
                  (res #`"����ɤ��ޤ���(,result)")
                  )
             (irc-send! `(NOTICE ,reply-to ,res))
             #t)))
        (else
          #f)))))

(define (write-error-to-stderr e)
  (write e (standard-error-port))
  (newline (standard-error-port)))


;; pipe����
(define (pipe-thunk)
  (let loop ()
    (call-with-input-file
      *pipe-file*
      (lambda (pipe)
        (let loop2 ()
          (let1 line (guard (e (else (read-from-string "")))
                       (read-line pipe))
            (unless (eof-object? line)
              (guard (e (else
                          (write-error-to-stderr e)))
                (let1 event (message->event line #t)
                  (irc-send-event! event)))
              (loop2)))))
      :buffering :modest)
    (loop)))

;; ����
(define (recv-thunk)
  (load-plugin)
  (%recv-thunk))

(define (%recv-thunk)
  (let1 event (irc-recv-event! #f)
    (cond
      ((eof-object? event)
       (sys-kill *pid* SIGTERM)) ; ��λ
      ((condition? event) ; stderr�˽��Ϥ���³��
       (write-error-to-stderr event)
       (thread-sleep! 1)
       (%recv-thunk)) ; ³��
      (else
        ;; event�˱����ƽ�����Ԥ�
        (or
          (resolve-event-primal event)
          ;; resolve-event-plugin�������ǥ��顼�㳰���ꤲ���ǽ�����⤤�Τǡ�
          ;; ������guard���Ƥ���
          (guard (e (else
                      (irc-send!
                        `(NOTICE ,(event->reply-to event)
                                 ,(write-to-string e)))))
            (resolve-event-plugin event))
          #t)
        ;; ³��
        (%recv-thunk)))))


;;; ----




(define (main-thunk)
  ;; �ǽ�ˡ�JOIN����
  (for-each
    (lambda (ch)
      (irc-send! `(JOIN ,ch)))
    *channels*)
  ;; �����Ť�ircbot.scm�ι�¤�ˤ���ɬ�פ�����
  ;; - �ᥤ�󥹥�åɤϥ����ʥ����
  ;; - �ҥ���åɤ�pipe�ƻ�
  ;; - �̤λҥ���åɤǥ�å���������
  ;; -- eof���ɤ߽Ф����塢�ƥ���åɤ�λ������ˤϡ�����
  ;; - �����̤λҥ���åɤǡ�����
  ;; - cv��mutex��Ϣ��
  ;; TODO: �ɤ����롩
  (let ((recv-thread (make-thread/report-error recv-thunk))
        (pipe-thread (make-thread/report-error pipe-thunk))
        )
    (thread-start! recv-thread)
    (thread-start! pipe-thread)
    (let loop ()
      (sys-sigsuspend
        (sys-sigmask SIG_SETMASK #f))
      (loop))))


(define *pid* #f)

(define (main args)
  (dynamic-wind
    (lambda ()
      (set! *pid* (sys-getpid))
      (with-output-to-file
        *pid-file*
        (lambda ()
          (display *pid*)
          (newline)))
      (unless (file-exists? *pipe-file*)
        (sys-mkfifo *pipe-file* (+ (* 64 (+ (* 1 0) (* 2 1) (* 4 1)))
                                   (*  8 (+ (* 1 0) (* 2 0) (* 4 0)))
                                   (*  1 (+ (* 1 0) (* 2 0) (* 4 0))))))
      )
    (lambda ()
      (with-irc
        *irc-server-ip*
        *irc-server-port*
        :irc-server-encoding *irc-server-encoding*
        :irc-server-pass *irc-server-pass*
        :base-nick *nick*
        :logging-handler (make-basic-irc-logger *log-dir*)
        main-thunk))
    (lambda ()
      (sys-unlink *pid-file*)
      )))



