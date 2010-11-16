#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(use gauche.test)

;;; ----

(test-start "tcpcgi.execute")

(use tcpcgi.execute)

(test-module 'tcpcgi.execute)

;;; ----

(test-section "check CGI/1.1")

;(test* "case" #f
;  hoge)


;;; ----

(test-end)
