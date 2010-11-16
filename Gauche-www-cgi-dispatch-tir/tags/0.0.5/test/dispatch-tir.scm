#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(use gauche.test)

;;; ----

(test-start "www.cgi.dispatch-tir")

(use www.cgi.dispatch-tir)

(test-module 'www.cgi.dispatch-tir)

;;; ----

(test-end)
