#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(add-load-path "/home/nekoie/Gauche-tir/trunk")
(add-load-path "/home/nekoie/nekoie/tmp/scheme")
(add-load-path ".")

(use tir.cgi-login-framework-lite)

(define (main-proc params)
  "test")

(define *clfl*
  (make <clfl>
    :dbm-root "./login"
    :main-proc main-proc
    ))

(define (main args)
  (clfl-main *clfl*))


