#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(use gauche.test)

;;; ----

(test-start "pib")

(use pib)

(test-module 'pib)

;;; ----

;(test-section "check version")

;(test* "(tir04-version)" (call-with-input-file "../VERSION" read-line)
;  (tir04-version))

;;; ----

(test-end)
