#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(use gauche.test)

;;; ----

(test-start "tir03.version")

(use tir03.version)

(test-module 'tir03.version)

;;; ----

;(test-section "check version")

(test* "(tir03-version)" (call-with-input-file "../VERSION" read-line)
  (tir03-version))

;;; ----

(test-end)
