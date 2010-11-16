#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(add-load-path "/home/nekoie/Gauche-tir/trunk")
(add-load-path "/home/nekoie/nekoie/tmp/scheme")
(add-load-path ".")

(use cgi-client)

(define *cgi-client*
  (make <cgi-client>
    :session-dbm-path "./hoge"
    ))

(define (main args)
  (cgi-client-main *cgi-client*))


