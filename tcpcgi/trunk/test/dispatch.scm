#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(use gauche.test)

;;; ----

(test-start "tcpcgi.dispatch")

(use tcpcgi.dispatch)

(test-module 'tcpcgi.dispatch)

;;; ----

(test-section "check dispatch")

;(test* "case" #f
;  hoge)


;;; ----

(test-end)
