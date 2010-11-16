#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; tester module of socket-server

(add-load-path ".")
(use tir.socket-cs)

;;; ----

;; 簡単なechoサーバ。
;; "!shutdown"が送られてきた場合は終了する。
(define (ss-thunk)
  (let1 line (read-line)
    (cond
      ((eof-object? line) #f)
      ((string=? line "!quit") #f)
      ((string=? line "!shutdown") (socket-server-shutdown *socket-server*) #f)
      (else
        (print line)
        #t))))

;(define *socket-spec* '(unix "/tmp/ss-test.sock" :reuse-addr? #t))
(define *socket-spec* '(tcp "127.0.0.1" 12345 :reuse-addr? #t))

(define *socket-server*
  (make
    <socket-server>
    :socket-spec *socket-spec*
    :thunk ss-thunk
    ))
(define *socket-client*
  (make
    <socket-client>
    :socket-spec *socket-spec*
    ))

(define (server-mode)
  (socket-server-start *socket-server*))

(define (client-mode)
  (let1 socket-client *socket-client*
    (define (close)
      (socket-client-close socket-client))

    (socket-client-open socket-client)
    (receive (sock-in sock-out)
      (get-socket-client-io socket-client)
      (let loop ()
        (let1 line (read-line)
          (cond
            ((string=? line "#close") (close))
            (else
              (display line sock-out)
              (newline sock-out)
              (flush sock-out)
              (let1 sock-line (read-line sock-in)
                (if (eof-object? sock-line)
                  (close)
                  (begin
                    (display sock-line)
                    (newline)
                    (flush)
                    (loop)))))))))))

(define (usage)
  (print "usage: ./tester.scm [client|server]"))

(define (main args)
  (if (= 1 (length args))
    (usage)
    (let1 mode (cadr args)
      (cond
        ((string=? mode "server") (server-mode))
        ((string=? mode "client") (client-mode))
        (else (usage)))))
  0)


