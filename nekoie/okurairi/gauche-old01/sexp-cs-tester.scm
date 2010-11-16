#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; tester module of sexp-cs

(add-load-path "/home/nekoie/Gauche-tir/trunk")
(add-load-path ".")
(use tir.sexp-cs)
(use tir.dbmwl)
(use dbm.qdbm)

(use srfi-2) ; and-let*

;;; ----

(define *dbmwl*
  (make
    <dbmwl>
    :dbm-type <qdbm>
    :dbm-path "tmp/test"))

;;; ----

(define *counter* (dbm-get *dbmwl* "counter" 0))
(define (ss-proc request)
  (inc! *counter*)
  (dbm-put! *dbmwl* "counter" *counter*)
  (dbm-put! *dbmwl* (x->string *counter*) request)
  (receive result
    (cond
      ((#/^\!request (\d+)/ request)
       (let* ((m (#/^\!request (\d+)/ request))
              (num (x->number (m 1)))
              (val (dbm-get *dbmwl* (x->string num) #f))
              )
         (values (list num val) #t)))
      ((equal? request "!quit")
       (values (list *counter* "quited.") #f))
      ((equal? request "!shutdown")
       (sexp-server-shutdown *sexp-server*)
       (values (list *counter* "server shutdowned.") #f))
      (else
        (values (list *counter* request) #t)))
    (display "request: ")
    (write request)
    (newline)
    (display "response: ")
    (write (car result))
    (newline)
    (apply values result)))

;;; ----

(define *socket-spec* '(tcp "127.0.0.1" 12345 :reuse-addr? #t))
(define *sexp-server*
  (make
    <sexp-server>
    :socket-spec *socket-spec*
    :proc ss-proc
    ))
(define *sexp-client*
  (make
    <sexp-client>
    :socket-spec *socket-spec*
    ))


(define (server-mode)
  (sexp-server-start *sexp-server*))

(define (client-mode)
  (let1 sexp-client *sexp-client*

    (sexp-client-connect sexp-client)

    (with-error-handler
      (lambda (e)
        ;(report-error e)
        #f)
      (lambda ()
        (let loop ()
          (let1 line (read-line)
            (cond
              ((string=? line "#close") #f)
              (else
                (write (request->response sexp-client line))
                (newline)
                (flush)
                (when (sexp-connection-live? sexp-client)
                  (loop))))))
        ;; ToDo: サーバ側から切断された時の対処が
        ;;       モジュール内部でなされているかのチェック
        (sexp-client-disconnect sexp-client)))))


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


