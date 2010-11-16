#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(use gauche.test)

;;; ----

(test-start "eval-sv.version")

(use eval-sv.version)

(test-module 'eval-sv.version)

;;; ----

;(test-section "check version")

(test* "(eval-sv-version)" (call-with-input-file "../VERSION" read-line)
  (eval-sv-version))

;;; ----

(test-end)
