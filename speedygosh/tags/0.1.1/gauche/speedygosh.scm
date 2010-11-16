;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(define-module speedygosh
  (use gauche.parameter)
  (use srfi-1)

  (export
    on-speedygosh?
    speedygosh-safety-shutdown
    speedygosh-add-termination-handler!
    speedygosh-delete-termination-handler!
    ))
(select-module speedygosh)


(define powered-by-speedygosh (make-parameter #f))
(define terminate-request (make-parameter #f))
(define speedygosh-termination-handlers (make-parameter '()))


(define (on-speedygosh?)
  (powered-by-speedygosh))

(define (speedygosh-safety-shutdown)
  (terminate-request #t))

(define (speedygosh-add-termination-handler! thunk)
  (speedygosh-termination-handlers
    (cons
      thunk
      (speedygosh-termination-handlers)))
  #t)
(define (speedygosh-delete-termination-handler! thunk)
  (speedygosh-termination-handlers
    (delete
      thunk
      (speedygosh-termination-handlers)))
  #t)


(provide "speedygosh")

