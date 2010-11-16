;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; sdssクライアントモジュール

;;; ToDo: どのようにする？？？
;;; - とりあえず、dbm方式で行く事にしてみる。再利用可能方式。
;;; - 或いは、毎回接続切断方式にする？

;;; note:
;;; - このモジュールは、sdss:table-get等の関数を提供する？？？
;;; -- とりあえず、透過的に使えるようにしたい。そうできれば、すごく便利な為。


(define-module tir03.sdss.client
  (use gauche.parameter)
  (use gauche.net)

  (use srfi-1)

  (use tir03.sdss.command)

  (export
    <sdss-client>
    sdss-client-connect
    sdss-client-disconnect
    sdss-client-connected?
    sdss-client-connection-is-alive?
    sdss-client-reconnect
    with-sdss-client

    ;; came from gauche.net
    <sockaddr>
    <sockaddr-in>
    <sockaddr-un>
    sockaddr-family
    sockaddr-name
    sockaddr-addr
    sockaddr-port
    ))
(select-module tir03.sdss.client)


;;; --------


(define sdss-client (make-parameter #f))


;;; --------


(define-class <sdss-client> ()
  (
   ;; settings
   (sockaddr
     :accessor sockaddr-of
     :init-keyword :sockaddr
     :init-value #f)

   ;; internal slots
   (client-socket
     :accessor client-socket-of
     :init-value #f)
   ))


(define-method initialize ((self <sdss-client>) initargs)
  (next-method)
  ;; check slot
  (unless (sockaddr-of self)
    (errorf "~a must be need to :sockaddr" (class-name (class-of self))))
  )


;;; --------


(define-method sdss-client-connect ((self <sdss-client>))
  (when (client-socket-of self)
    (error "already opened" self))
  (set!
    (client-socket-of self)
    (make-client-socket (sockaddr-of self))))


(define-method sdss-client-disconnect ((self <sdss-client>))
  (unless (client-socket-of self)
    (error "not opened" self))
  ;; ToDo: 可能なら、flushしておいた方が良いかも知れない？
  (guard (e (else #f))
    (socket-shutdown (client-socket-of self) 2))
  (guard (e (else #f))
    (socket-close (client-socket-of self)))
  #t)


(define-method with-sdss-client ((self <sdss-client>) thunk)
  (parameterize ((sdss-client self))
    (thunk)))


(define-method sdss-client-connected? ((self <sdss-client>))
  (not
    (not
      (client-socket-of self))))


(define-method sdss-client-connection-is-alive? ((self <sdss-client>))
  (and
    (client-socket-of self)
    (with-error-handler
      (lambda (e)
        ;; ToDo:
        ;; エラー時には、close処理した方が良い？？？
        ;; それとも、再接続処理を行うべき？？？
        #f)
      (lambda ()
        (let1 identifier (list (sys-time) (sys-getpid))
          (equal? identifier (with-sdss-client
                               self
                               (lambda ()
                                 (sdss:echo identifier)))))))))


(define-method sdss-client-reconnect ((self <sdss-client>))
  (guard (e (else #f))
    (sdss-client-disconnect self))
  (sdss-client-connect self))


;;; --------


;; listの長さを返す。不完全listの場合は、完全な部分のみの長さを返す。
(define (length* target)
  (let loop ((result 0)
             (left target))
    (if (pair? left)
      (loop
        (+ result 1)
        (cdr left))
      result)))


;;; 実際の、コマンド実行関数(各sdssコマンド関数は、これのラッパー)
(define (command-execute command-symbol args)
  (let1 sdss-args (hash-table-get sdss-args-table command-symbol)
    (define (args-is-valid?)
      (if (proper-list? sdss-args)
        (= (length sdss-args) (length args))
        (<= (length* sdss-args) (length args))))
    ;; まず、引数チェックを行う
    (unless (args-is-valid?)
      (errorf
        "wrong number of arguments for ~s (required ~d, got ~d)"
        command-symbol
        (length* sdss-args)
        (length args)))
    ;; with-sdss-client内かどうかをチェック
    (unless (sdss-client)
      (errorf "~s must be use into with-sdss-client" command-symbol))
    (let ((in (socket-input-port (client-socket-of (sdss-client))))
          (out (socket-output-port (client-socket-of (sdss-client)))))
      ;; 今のところ、同時に複数のコマンドを実行する機能は無し
      ;; write/ss時にエラーが出る=切断、の筈なので、エラー例外はそのまま投げる
      ;; ToDo: 通信エラー時は、何らかの後処理を行ってから
      ;;       例外を投げた方が良くないか？？？
      (write/ss (cons command-symbol args) out)
      (newline out)
      (flush out)
      ;; read-line時にエラーが出る=切断、の筈なので、エラー例外はそのまま投げる
      (let1 line (read-line in)
        (if (eof-object? line)
          (error "disconnected from server")
          (let1 result (with-error-handler
                         (lambda (e) e)
                         (lambda ()
                           ;; note: 今のところ、単独コマンド実行、という前提
                           (read-from-string line)))
            (cond
              ((is-a? result <error>)
               (error "read error" result))
              ((not (pair? result))
               (error "invalid response accepted" result))
              ((eq? 'error (car result))
               (error (cdr result)))
              (else
                (apply values (cdr result))))))))))


;;; sdss-command-tableとsdss-args-tableから、ラッパー関数を生成し、exportする
(for-each
  (lambda (command-symbol)
    (eval
      ;; ToDo: eval内をなるべく小さくする(実際の処理をevalの外に追い出す)
      `(begin
         (define (,command-symbol . args)
           ;; ココに入ったという事は、引数の数は合っているので、実行
           ;; (引数の数に問題がある場合は、通常の関数と同様に、
           ;;  エラー例外を投げたい為)
           (,command-execute ',command-symbol args))
         ;; 定義後、export
         (export ,command-symbol))
      (current-module)))
  (hash-table-keys sdss-command-table))


;;; --------
;;; came from gauche.net


(define <sockaddr> <sockaddr>)
(define <sockaddr-in> <sockaddr-in>)
(define <sockaddr-un> <sockaddr-un>)
(define sockaddr-family sockaddr-family)
(define sockaddr-name sockaddr-name)
(define sockaddr-addr sockaddr-addr)
(define sockaddr-port sockaddr-port)


;;; --------


(provide "tir03/sdss/client")


