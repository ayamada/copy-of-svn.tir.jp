#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ��¿�ʽ��Ѵ��ؿ���


(define-module tir.format
  ;(use srfi-2) ; and-let*

  (export
    number->string-japanese-wday
    number->string-japanese-weekday
    epoch->japanese-date-string
    ))
(select-module tir.format)


(define (number->string-japanese-count num)
  ;; ToDo : ���Ȥ�
  num)


(define *japanese-weekday-vector*
  #("������" "������" "������" "������" "������" "������" "������"))
(define *japanese-wday-vector*
  #("��" "��" "��" "��" "��" "��" "��"))

(define (number->string-japanese-wday num)
  (or
    (vector-ref *japanese-wday-vector* num #f)
    (error "invalid number")))

(define (number->string-japanese-weekday num)
  (string-append
    (number->string-japanese-wday num)
    "����"))


(define (epoch->japanese-date-string epoch)
  (let1 tm (sys-localtime epoch)
    (format
      "~aǯ~a��~a��(~a)"
      (+ 1900 (number->string-japanese-count (ref tm 'year)))
      (+ 1 (number->string-japanese-count (ref tm 'mon)))
      (number->string-japanese-count (ref tm 'mday))
      (number->string-japanese-wday (ref tm 'wday))
      )))


;;; ----


(provide "tir/format")

