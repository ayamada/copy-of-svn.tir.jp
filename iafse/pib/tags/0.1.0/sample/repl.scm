#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; basis on http://homepage3.nifty.com/oatu/gauche/try.html#ircbot

;;; ※注意:
;;; このスクリプトは非常に危険です。
;;; セキュリティホールになります。
;;; 決して、安易な設定のまま使用しないでください。

;;; 使い方:
;;; - 以下のパラメータを使いたいように変更する
;;; - 実行する(おそらく、以下のようなコマンドになる)
;;;   ./repl.scm > stdout.log 2>&1 &
;;; - botがjoinしているチャンネル、または直接botにPRIVMSGで、
;;;   「(eval! '(+ 1 2))」のようなテキストを送ると、評価を行い、その返り値を
;;;   返信してくれる
;;;   (この評価の際のprefix/suffixは変更可能、例えば「bot評価＞(+ 1 2)」等)


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

;; (eval! '(+ 1 2)) のようなテキストを送ると、評価してくれる
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


;; get-expr-stringは、ircの他人の発言の文字列を引数に取り、
;; そこから評価すべき文字列だけを取り出すか、#fを返すかする手続き
(define (get-expr-string msg)
  (and-let* ((match (*get-expr-string-re* msg)))
    (match 1)))


;; allowed-prefix-predは、ircのprefix文字列を引数に取り、
;; それがevalが許可された人物であるなら#tを、そうでないなら#fを返す手続き
(define (allowed-prefix-pred prefix)
  (let/cc return
    (unless prefix
      (return #f))
    ;; prefixは、"repl!~repl@fqdn" のような文字列なので、まずはこれを分解する
    (and-let* ((match (#/^(.*?)\!(.*?)\@(.*?)$/ prefix))
               (nick (match 1))
               (username/tilda (match 2)) ; identにもよるが、先頭に~がつく
               (hostname (match 3))
               )
      ;; もっと細かく権限チェックを行ってもよい
      (for-each
        (lambda (allowed-host)
          (when (equal? hostname allowed-host)
            (return #t)))
        *allowed-hosts*)
      #f)))


;;; ----

;; この手続きは、textが変換後に512byteを越えていたら、
;; 少しずつ切り詰めて送れるサイズに変換する
(define (irc-privmsg/trim channel text)
  (guard (e
           ((equal? (ref e 'message) "message too long")
            (irc-privmsg/trim channel
                              (string-copy text 0 (- (string-length text) 2))))
           (else (error e)))
    (irc-send!
      `("NOTICE" ,channel ,text))))


;; 直接nickにPRIVMSGが送られた場合、送り返し先をそのまま
;; (car (event->params event))で決定すると自分自身になってしまう為、
;; その場合は相手のnickに送る必要がある。
;; 尚、この手続きを呼ぶのはdoitだけなので、commandがPRIVMSGというのは
;; 既に確定している為、チェックを省いている。
;; (改造したり、コードを流用する際は注意)
(define (get-channel event)
  (let1 channel (car (event->params event))
    (if (not (equal? channel (get-current-nick)))
      channel
      ((#/\!/ (event->prefix event)) 'before))))

(define (doit event)
  ;; prefixがevalを許可された人のみ、この先の処理を行う
  (when (allowed-prefix-pred (event->prefix event))
    ;; メッセージから式部分を取り出す
    (let* ((channel (get-channel event))
           (msg (cadr (event->params event)))
           (expr-string (get-expr-string msg))
           )
      ;; expr-stringの取得に成功したら、続行する
      (when expr-string
        ;; ここから先は失敗する可能性があるので、guardしておく
        (guard (e (else
                    (irc-privmsg/trim channel (write-to-string e))))
          ;; readする
          (let1 sexpr (read-from-string expr-string)
            ;; evalする
            ;; TODO: evalを実行する環境は、可能ならeval/sv等の、
            ;;       安全な環境にした方が良い
            (receive results (eval sexpr (current-module))
              ;; 多値は、複数のメッセージを送る事で対応する事にする
              (for-each
                (lambda (x)
                  ;; printする
                  (irc-privmsg/trim channel (write-to-string x)))
                results))))))))

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



