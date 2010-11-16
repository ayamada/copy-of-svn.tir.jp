#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(use gauche.test)

;;; ----

(test-start "www.cgi.dispatch-tir.version")

(use www.cgi.dispatch-tir.version)

(test-module 'www.cgi.dispatch-tir.version)

;;; ----

;(test-section "check version")

(test* "(www.cgi.dispatch-tir-version)" (call-with-input-file
                                          "../VERSION" read-line)
  (www.cgi.dispatch-tir-version))

;;; ----

(test-end)
