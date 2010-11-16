#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; basis on http://homepage3.nifty.com/oatu/gauche/try.html#ircbot

;;; 使い方:
;;; - 以下のパラメータを使いたいように変更する
;;; - 実行する(おそらく、以下のようなコマンドになる)
;;;   ./logger.scm > stdout.log 2>&1 &
;;; - 実行すると、S式書式のログが*log-dir*に、チャンネル別、日別で保存される
;;; - INVITEされると、そのチャンネルに入る


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
  ;; 最初に、JOINする
  (for-each
    (lambda (ch)
      (irc-send! `("JOIN" ,ch)))
    *channels*)
  (let/cc finish
    (let loop ()
      (let1 event (irc-recv-event! 1)
        (cond
          ((not event) #f) ; イベント無し
          ((eof-object? event) (finish)) ; 接続断
          ;; inviteのみ処理する、あとはスルー
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



