;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; setpサーバと通信を行うクライアント。
;;; 現在のところは非同期通信には対応していない。

;;; ToDo: 使い方を書いておく事

;;; ToDo: connect時にプロトコルチェックを行うコードを追加する事

;;; ToDo: 通信切断時の処理をどうするか考える必要がある

;;; ToDo: request->response等の実行前にconnectしたりするか？
;;;       少なくとも、事前にconnectしているかのチェックは行い、
;;;       適正なエラーメッセージを出すようにはした方がいい。


(define-module tir03.setp.client
  (use gauche.net)

  (use srfi-1)

  (use util.list)

  (use tir03.setp.protocol)

  (export
    <setp-client>
    setp-client-connect
    setp-client-disconnect
    setp-client-connected?
    setp-client-connection-is-alive?
    setp-client-reconnect
    request->response
    requests->responses

    ;; came from gauche.net
    <sockaddr>
    <sockaddr-in>
    <sockaddr-un>
    sockaddr-family
    sockaddr-name
    sockaddr-addr
    sockaddr-port
    ))
(select-module tir03.setp.client)


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


(define-class <setp-client> ()
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


(define-method initialize ((self <setp-client>) initargs)
  (next-method)
  ;; check slot
  (unless (sockaddr-of self)
    (errorf "~a must be need to :sockaddr" (class-name (class-of self))))
  )


;;; --------


(define-method setp-client-connect ((self <setp-client>))
  (when (client-socket-of self)
    (error "already opened" self))
  (set!
    (client-socket-of self)
    (make-client-socket (sockaddr-of self))))


(define-method setp-client-disconnect ((self <setp-client>))
  (unless (client-socket-of self)
    (error "not opened" self))
  ;; ToDo: 可能なら、flushしておいた方が良いかも知れない？
  (guard (e (else #f))
    (socket-shutdown (client-socket-of self) 2))
  (guard (e (else #f))
    (socket-close (client-socket-of self)))
  #t)


(define-method setp-client-connected? ((self <setp-client>))
  (not (not (client-socket-of self))))


(define-method setp-client-connection-is-alive? ((self <setp-client>))
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
          (equal? identifier (with-setp-client
                               self
                               (lambda ()
                                 (setp:echo identifier)))))))))


(define-method setp-client-reconnect ((self <setp-client>))
  (guard (e (else #f))
    (setp-client-disconnect self))
  (setp-client-connect self))


;;; --------


(define-method requests->responses ((self <setp-client>) request
                                                         . other-requests)
  ;; 多値としてrequestsを受け取り、多値としてresponsesを返す。
  ;; 但し、requestsの途中でエラーがあった場合は、
  ;; それ以降のrequestの評価は行わず、返ってくるresponsesの数も減る為、
  ;; responsesの長さは不定となる。
  ;; (但し、エラーは返るので、最低でも一つの要素は含まれている)
  ;; ToDo: 最適化を行う余地がある
  (apply
    values
    (let loop ((requests (cons request other-requests)))
      (if (null? requests)
        '()
        (let1 response (request->response self (car requests))
          (if (condition? response)
            (list response)
            (cons
              response
              (loop (cdr requests)))))))))




(define-method request->response ((self <setp-client>) request)
  ;; ToDo: 通信切断対応が必要
  (guard (e (else e)) ; 仮の通信切断対策
    (let ((in (socket-input-port (client-socket-of self)))
          (out (socket-output-port (client-socket-of self))))
      ;; write/ss時にエラーが出る=切断、の筈なので、エラー例外はそのまま投げる
      ;; ToDo: 通信エラー時は、何らかの後処理を行ってから
      ;;       例外を投げた方が良くないか？？？
      (write/ss request out)
      (newline out)
      (flush out)
      ;; read-line時にエラーが出る=切断、の筈なので、エラー例外はそのまま投げる
      (let1 line (read-line in)
        (if (eof-object? line)
          (error "disconnected from server")
          (receive (response headers) (guard (e (else (error "read error" e)))
                                        (call-with-input-string
                                          line
                                          (lambda (p)
                                            (let* ((v1 (read p))
                                                   (v2 (read p)))
                                              (values v1 v2)))))
            (cond
              ((eof-object? response)
               (values)) ; ToDo: 他に返せる値は無いのか？？？
              ((and
                 (pair? headers)
                 (assoc-ref headers "error"))
               => (cut error <>)) ; ToDo: もっとちゃんとエラーを構築する？
              (else
                response))))))))



;;; --------


(provide "tir03/setp/client")


