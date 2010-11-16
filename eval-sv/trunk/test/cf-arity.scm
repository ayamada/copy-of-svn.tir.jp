#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(use gauche.test)
;;; 普通にtest caseを書く
;;; (問題があるかどうかはあとで考える)

(set! *test-report-error* #f)

;;; ----

(test-start "eval-sv.cf-arity")

(use eval-sv.cf-arity)

(test-module 'eval-sv.cf-arity)

;;; ----

(test-section "check solve-arity")

(test* "(solve-arity +)" '(0 #t)
  (receive r (solve-arity +) r))

(test* "(solve-arity cons)" '(2 #f)
  (receive r (solve-arity cons) r))

(test* "(solve-arity make)" '(1 #t)
  (receive r (solve-arity make) r))

(test-section "check camouflage-arity")

(test* "(camouflage-arity 0 #f proc)" '(0 #f)
  (receive r (solve-arity (camouflage-arity 0 #f (lambda args 'dummy))) r))

(test* "(camouflage-arity 0 #t proc)" '(0 #t)
  (receive r (solve-arity (camouflage-arity 0 #t (lambda args 'dummy))) r))

(test* "(camouflage-arity 3 #f proc)" '(3 #f)
  (receive r (solve-arity (camouflage-arity 3 #f (lambda args 'dummy))) r))

(test* "(camouflage-arity 5 #t proc)" '(5 #t)
  (receive r (solve-arity (camouflage-arity 5 #t (lambda args 'dummy))) r))

(test-section "cf-arity ok")

;;; ----

(test-end)
