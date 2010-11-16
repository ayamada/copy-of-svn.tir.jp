#!/usr/local/gauche-0.9/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; irc secretary

;;; ./ircsec.scm > std.log 2>&1 &

;;;   流し込む際の日本語コードは、(gauche-character-encoding)にする事
;;;   ircコマンドの詳細は、rfcを確認する事(特に引数の書式等)
;;;   RFC: http://www.haun.org/kent/lib/rfc1459-irc-ja.html

;;; TODO: プラグインまたはストレージスクリプトの読み書き機能が必要
;;; TODO: 以下の処理を行うトリガーが必要
;;; - リロード
;;; - S式入力

;;; TODO: 通信断など、何らかの原因で落ちてしまいがちなので、以下のようにする
;;;       - 多重起動防止処理を入れる(pidファイルをみればok)
;;;       - cronで定時起動させる

;;; TODO: irc.cgiと連携を取る為のインターフェースを決定する事

;;; NB: plugin.dumpが読み込まれるのは、recv-thread限定。
;;;     他のスレッドには影響を及ぼさない

(use pib)
(use gauche.threads)
(use gauche.parameter)

;;; 設定ファイル読み込み
;;; (publicにすべきでない設定はこのファイルに記述する)
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

;; この手続きは、plugin.dump内で書き換えてよい
(define (resolve-event-plugin event)
  #f)

;; 標準機能はプラグインのリロードのみ(今のところは)
(define (resolve-event-primal event)
  (when (eq? 'PRIVMSG (event->command event))
    (let ((reply-to (event->reply-to event))
          (msg (cadr (event->params event)))
          )
      (cond
        ((equal? "(reload!)" msg)
         (guard (e (else
                     (let1 res #`"リロードに失敗しました(,(ref e 'message))"
                       (irc-send! `(NOTICE ,reply-to ,res)))))
           (let* ((result (load-plugin))
                  (res #`"リロードしました(,result)")
                  )
             (irc-send! `(NOTICE ,reply-to ,res))
             #t)))
        (else
          #f)))))

(define (write-error-to-stderr e)
  (write e (standard-error-port))
  (newline (standard-error-port)))


;; pipe処理
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

;; 受信
(define (recv-thunk)
  (load-plugin)
  (%recv-thunk))

(define (%recv-thunk)
  (let1 event (irc-recv-event! #f)
    (cond
      ((eof-object? event)
       (sys-kill *pid* SIGTERM)) ; 終了
      ((condition? event) ; stderrに出力して続行
       (write-error-to-stderr event)
       (thread-sleep! 1)
       (%recv-thunk)) ; 続行
      (else
        ;; eventに応じて処理を行う
        (or
          (resolve-event-primal event)
          ;; resolve-event-pluginは内部でエラー例外を投げる可能性が高いので、
          ;; ちゃんとguardしておく
          (guard (e (else
                      (irc-send!
                        `(NOTICE ,(event->reply-to event)
                                 ,(write-to-string e)))))
            (resolve-event-plugin event))
          #t)
        ;; 続行
        (%recv-thunk)))))


;;; ----




(define (main-thunk)
  ;; 最初に、JOINする
  (for-each
    (lambda (ch)
      (irc-send! `(JOIN ,ch)))
    *channels*)
  ;; 少し古いircbot.scmの構造にする必要がある
  ;; - メインスレッドはシグナル処理
  ;; - 子スレッドでpipe監視
  ;; - 別の子スレッドでメッセージ受信
  ;; -- eofを読み出した後、親スレッドを終了させるには？？？
  ;; - 更に別の子スレッドで？？？
  ;; - cvとmutexで連携
  ;; TODO: どうする？
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



