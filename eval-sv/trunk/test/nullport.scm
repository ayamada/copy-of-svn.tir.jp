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

(test-start "eval-sv.nullport")

(use eval-sv.nullport)

(test-module 'eval-sv.nullport)

;;; ----

(test-section "check nullport")

(test* "read nullport" (read-from-string "")
  (read (open-input-nullport)))

(test* "write nullport" (flush)
  (let1 out (open-output-nullport)
    (write 3 out)
    (flush out)))

(test* "nullificate-current-port" (flush)
  (nullificate-current-port
    (lambda ()
      (write (read))
      (flush))))

(test-section "check exceptionport")

(test* "read exceptionport" *test-error*
  (read (open-input-exceptionport)))

(test* "write exceptionport" *test-error*
  (let1 out (open-output-exceptionport)
    (write 3 out)
    (flush out)))

(test* "isolate-current-port" *test-error*
  (isolate-current-port
    (lambda ()
      (write (read))
      (flush))))

(test-section "nullport ok")

;;; ----

(test-end)
