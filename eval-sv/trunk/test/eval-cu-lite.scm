#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(use gauche.test)
(use gauche.parameter)
;;; 普通にtest caseを書く
;;; (問題があるかどうかはあとで考える)

(set! *test-report-error* #f)

;;; ----

(test-start "eval-cu-lite")

(use eval-cu-lite)

(test-module 'eval-cu-lite)

;;; ----

(define-values (eval/cu env)
  (make-eval/cu :isolate-port? #f))

;;; ----

(test-section "check eval-cu-lite")

(test* "(eval/cu '(+ 1 3))" (+ 1 3)
  (eval/cu '(+ 1 3)))


(test* "eval-cu-last-count (1)" 1
  (eval-cu-last-count))

(test* "(eval/cu '(+ 1 (* 2 (- 5 3)))" (+ 1 (* 2 (- 5 3)))
  (eval/cu '(+ 1 (* 2 (- 5 3)))))

(test* "eval-cu-last-count (2)" 3
  (eval-cu-last-count))

(test* "proc exceeded" *test-error*
  (eval/cu '(+ 1 (* 2 (- 5 3))) 3))

(test* "eval-cu-last-count (3)" 3
  (eval-cu-last-count))

(test* "<eval-cu-exceeded> check (1)" '(proc 3)
  (guard (e ((<eval-cu-exceeded> e)
             (let ((type (ece->type e))
                   (count (ece->count e))
                   (continue (ece->continue e)))
               (list type 3))))
    (eval/cu '(+ 1 (* 2 (- 5 3))) 3)))

(test* "<eval-cu-exceeded> check (2)" (list
                                        (+ 1 (* 2 (- 5 3)))
                                        #t)
  (let1 flag #f
    (guard (e ((<eval-cu-exceeded> e)
               (let ((type (ece->type e))
                     (count (ece->count e))
                     (continue (ece->continue e)))
                 (set! flag #t)
                 (continue))))
      (list
        (eval/cu '(+ 1 (* 2 (- 5 3))) 1)
        flag))))

(import/cu env 'eval/cu eval/cu)
(import/cu env 'eval-cu-last-count (lambda ()
                                     (eval-cu-last-count)))

(import/cu env 'guard guard)

(test* "nested eval/cu (1)" 3
  (eval/cu `(eval/cu '(+ 1 2))))

(test* "nested eval/cu (2)" '(5 1 9 8)
  (let* ((r (eval/cu `(let* ((r (eval/cu '(list 9 8)))
                             (c (eval-cu-last-count)))
                        (cons c r))))
         (c (eval-cu-last-count)))
    (cons c r)))

(test* "nested eval/cu (3)" 333
  (guard (e ((<eval-cu-exceeded> e) 333))
    (eval/cu `(guard (e (else 22))
                (eval/cu '(+ 10 2) 100))
             2)))

(test-section "eval-cu-lite ok")

;;; ----

(test-end)
