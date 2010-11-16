#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; 雑多な書式変換関数群


(define-module tir.format
  ;(use srfi-2) ; and-let*

  (export
    number->string-japanese-wday
    number->string-japanese-weekday
    epoch->japanese-date-string
    ))
(select-module tir.format)


(define (number->string-japanese-count num)
  ;; ToDo : あとで
  num)


(define *japanese-weekday-vector*
  #("日曜日" "月曜日" "火曜日" "水曜日" "木曜日" "金曜日" "土曜日"))
(define *japanese-wday-vector*
  #("日" "月" "火" "水" "木" "金" "土"))

(define (number->string-japanese-wday num)
  (or
    (vector-ref *japanese-wday-vector* num #f)
    (error "invalid number")))

(define (number->string-japanese-weekday num)
  (string-append
    (number->string-japanese-wday num)
    "曜日"))


(define (epoch->japanese-date-string epoch)
  (let1 tm (sys-localtime epoch)
    (format
      "~a年~a月~a日(~a)"
      (+ 1900 (number->string-japanese-count (ref tm 'year)))
      (+ 1 (number->string-japanese-count (ref tm 'mon)))
      (number->string-japanese-count (ref tm 'mday))
      (number->string-japanese-wday (ref tm 'wday))
      )))


;;; ----


(provide "tir/format")

