#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; basis on http://homepage3.nifty.com/oatu/gauche/try.html#ircbot

;;; 使い方:
;;; - 以下のパラメータを使いたいように変更する
;;; - 実行する(おそらく、以下のようなコマンドになる)
;;;   ./ircbot.scm > stdout.log 2>&1 &
;;; - 実行すると、S式書式のログが*log-dir*に、チャンネル別、日別で保存される
;;; - 実行すると、*pipe-file*が生成されるので、そこに
;;;   echo 'PRIVMSG #hoge :ほげほげ'
;;;   のように文字列を流し込むと、botがそのコマンドをサーバに送信する
;;;   流し込む際の日本語コードは、(gauche-character-encoding)にする事
;;;   ircコマンドの詳細は、rfcを確認する事(特に引数の書式等)
;;;   RFC: http://www.haun.org/kent/lib/rfc1459-irc-ja.html


(add-load-path "../lib")
(add-load-path "lib")
(add-load-path ".")

(use pib)
(use gauche.threads)
(use util.queue)

(define *irc-server-ip* "localhost")
(define *irc-server-port* 6667)
(define *irc-server-encoding* "iso-2022-jp")
(define *irc-server-pass* #f)
(define *nick* "ircbot")

(define *pipe-file* "ircbot.pipe")
(define *log-dir* "ircbot.logs")
(define *channels* '("#pib1" "#pib2"))

;;; ----

(define (resolve-pipe queue mutex)
  (let loop ()
    (call-with-input-file
      *pipe-file*
      (lambda (pipe)
        (let loop2 ()
          (let1 line (guard (e (else (read-from-string "")))
                       (read-line pipe))
            (unless (eof-object? line)
              (mutex-lock! mutex)
              (enqueue! queue line)
              (mutex-unlock! mutex)
              (condition-variable-broadcast! (get-recv-cv))
              (loop2)))))
      :buffering :modest)
    (loop)))


(define (irc-main queue mutex)
  (let loop ((finish? #f))
    (mutex-lock! mutex)
    (cond
      ;; 受信キューにデータがあるなら、それを処理する
      ((irc-recv-event! 0)
       => (lambda (event)
            (cond
              ((eof-object? event) (set! finish? #t))
              ((condition? event)
               (write event (standard-error-port))
               (thread-sleep! 1))
              (else #f))
            (mutex-unlock! mutex)))
      ;; pipeキューが空でないなら、こっちを処理する
      ((not (queue-empty? queue))
       (let1 line (dequeue! queue)
         (let1 event (guard (e (else e))
                       (message->event line #t))
           (unless (condition? event)
             (irc-send-event! event))
           (when (condition? event)
             (write event (standard-error-port)))
           (mutex-unlock! mutex))))
      ;; それ以外なら、再度待つ
      (else
        (mutex-unlock! mutex (get-recv-cv))))
    (unless finish?
      (loop #f))))


(define (main-thunk)
  ;; 最初に、JOINする
  (for-each
    (lambda (ch)
      (irc-send! `("JOIN" ,ch)))
    *channels*)
  ;; pipeからmessageを取り出し、送信する専用のスレッドを用意する
  (let* ((queue (make-queue))
         (mutex (make-mutex "pipe"))
         (pipe-thread (make-thread (lambda ()
                                     (resolve-pipe queue mutex))
                                   "pipe"))
         (irc-thread (make-thread (lambda ()
                                    (irc-main queue mutex))
                                  "irc"))
         )
    (dynamic-wind
      (lambda ()
        (thread-start! pipe-thread)
        (thread-start! irc-thread)
        #t)
      (lambda ()
        ;; 親スレッドは、シグナルを処理するだけ
        (let loop ()
          (sys-sigsuspend
            (sys-sigmask SIG_SETMASK #f))
          (loop)))
      (lambda ()
        (mutex-lock! mutex)
        (thread-terminate! pipe-thread)
        (thread-terminate! irc-thread)
        (mutex-unlock! mutex)
        #t))))


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



