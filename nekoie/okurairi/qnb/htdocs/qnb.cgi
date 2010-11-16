#!/usr/local/gauche/bin/gosh
;;; coding: iso-2022-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(add-load-path "/home/nekoie/ktest/lib")
(add-load-path "/home/nekoie/qnb/lib")
(use qnb.client.cgi)

;;; ----

(define *client-cgi*
  (make
    <client-cgi>
    ;; ToDo: more keywords
    ))

;; for cgi
;(define (main args)
;  (qnb-main *client-cgi*)
;  0)

;; for fastcgi
;(use www.fastcgi)
;(define (main args)
;  (with-fastcgi
;    (lambda ()
;      (qnb-main *client-cgi*)))
;  0)

;; for reverse-proxy and fastcgi
(use www.cgi)
(use www.fastcgi)
(use gauche.parameter)
(use srfi-1)
(define (main args)
  (with-fastcgi
    (lambda ()
      ;; rewrite to cgi-metavariables (hack for reverse proxy)
      (parameterize ((cgi-metavariables
                       (list*
                         '("SERVER_NAME" "e.tir.jp")
                         '("SERVER_PORT" "80")
                         (remove
                           (lambda (key+val)
                             (#/^SERVER_(?:NAME|PORT)$/ (car key+val)))
                           (or (cgi-metavariables) '())))))
        (qnb-main *client-cgi*))))
  0)

