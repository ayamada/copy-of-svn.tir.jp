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

;;; TODO: 本体と送信スレッドが別れてしまって、どうも使いにくいので、
;;;       もう少し考え直す必要がある

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
(define *global-dirname* "AUTH") ; チャンネル等の無いmessageを記録するdir
(define *channels* '("#pib" "#pib2"))


;;; ----

(define (logging mode event)
  (let* ((channel (guard (e (else *global-dirname*))
                    (let ((command (cadr event))
                          (channel (caddr event)))
                      ;; commandが数値なら、channelとは見做さない
                      (when (number? command)
                        (error "!"))
                      ;; 特定のcommandなら、channelとは見做さない
                      (when (equal? command "NICK")
                        (error "!"))
                      (when (equal? command "MODE")
                        (error "!"))
                      (when (equal? command "QUIT")
                        (error "!"))
                      ;; TODO: 他にもこの条件のcommandがある筈
                      ;; channelが空文字列、"."、".."、"/"を含まない事を確認
                      (set! channel
                        (regexp-replace #/\.|\// channel "_"))
                      ;; channelが空文字列なら変名する
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
                     ;; ロギングの為に、prefixを差し換える必要がある
                     ;; TODO: あとまわしにする
                     (event event-src)
                     )
                (send-message! pib event)
                (logging 'send event)
                (loop2))))))
      :buffering :modest)
    (loop)))


(define (main-proc pib)
  ;; 最初に、JOINする
  (for-each
    (lambda (ch)
      (send-message! pib `(#f "JOIN" ,ch)))
    *channels*)
  ;; pipeからmessageを取り出し、送信する専用のスレッドを用意する
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



