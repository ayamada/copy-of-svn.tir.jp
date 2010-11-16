#!/usr/local/gauche-0.8.14/bin/speedygosh --goshpath=/usr/local/gauche-0.8.14/bin/gosh
;#!/usr/local/gauche-0.8.14/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(define-macro (system-path . args)
  (string-join (cons "/home/nekoie/zi" args) "/"))

(define-macro (alp)
  `(add-load-path ,(system-path "lib")))
(alp)
(use zi-item)
(use gauche.charconv)

(define *src-file* (system-path "data" "item.txt"))
(define *encoding* (ces-guess-from-string "" "*JP"))


(define (main args)
  (item-dump
    :src-path *src-file*
    :encoding *encoding*
    ))


