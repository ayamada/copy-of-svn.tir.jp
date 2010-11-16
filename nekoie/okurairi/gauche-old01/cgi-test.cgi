#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(add-load-path "/home/nekoie/Gauche-tir/trunk")
(add-load-path "/home/nekoie/nekoie/tmp/scheme")
(add-load-path ".")

;(use tir.cgi-framework)
(use cgi-framework)

(define (hoge)
  #f)

(define *cgi-framework*
  (make <cgi-framework>
    :session-dbm-path "./hoge"
    ))

(define (main args)
  (cgi-framework-main *cgi-framework*))


