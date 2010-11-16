;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; "nullificate (or isolate) current port" module
;;;
;;;  Copyright (c) 2008 Atsuo Yamada, All rights reserved.
;;;
;;;  Permission is hereby granted, free of charge, to any person
;;;  obtaining a copy of this software and associated documentation
;;;  files (the "Software"), to deal in the Software without restriction,
;;;  including without limitation the rights to use, copy, modify,
;;;  merge, publish, distribute, sublicense, and/or sell copies of
;;;  the Software, and to permit persons to whom the Software is
;;;  furnished to do so, subject to the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;  AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
;;;  OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;;;  IN THE SOFTWARE.


(define-module eval-sv.nullport
  (use gauche.uvector)
  (use gauche.vport)

  (export
    open-input-nullport
    open-output-nullport
    open-input-exceptionport
    open-output-exceptionport
    nullificate-current-port
    isolate-current-port
    ))
(select-module eval-sv.nullport)


(define (open-input-nullport)
  (open-input-string ""))
(define (open-output-nullport)
  (make <buffered-output-port>
        :flush (lambda (buf flag)
                 (u8vector-length buf))))

(define (open-input-exceptionport)
  (make <virtual-input-port>
        :getb (lambda ()
                (error "restricted to access to port"))))
(define (open-output-exceptionport)
  (make <virtual-output-port>
        :putb (lambda (x)
                (error "restricted to access to port"))))


;; TODO: マクロを使って共通化可能部分を共通化する


;; current-*-portを、入力はEOF、出力とエラーはすぐ捨てるものに差し換える
(define nullificate-current-port
  (lambda (thunk)
    (let ((in #f)
          (out #f))
      (dynamic-wind
        (lambda ()
          (set! in (open-input-nullport))
          (set! out (open-output-nullport))
          #t)
        (lambda ()
          (with-ports in out out thunk))
        (lambda ()
          (set! in #f)
          (set! out #f)
          (guard (e (else #f))
            (close-input-port in))
          (guard (e (else #f))
            (close-output-port out))
          #t)))))


;; current-*-portを、アクセスするとエラー例外を返すものに差し換える
;; 但し、current-error-portのみ、そのままスルーする
;; (current-error-portがexceptionportの場合、
;;  エラー例外が投げられた時にそれをcurrent-error-portに出力しようとして
;;  再帰エラーになり、Gaucheプロセス自体がabortしてしまう為)
(define isolate-current-port
  (lambda (thunk)
    (let ((in #f)
          (out #f)
          ;(err #f)
          )
      (dynamic-wind
        (lambda ()
          (set! in (open-input-exceptionport))
          (set! out (open-output-exceptionport))
          ;(set! err (open-output-nullport))
          #t)
        (lambda ()
          (with-ports in out (current-error-port) thunk))
        (lambda ()
          (set! in #f)
          (set! out #f)
          ;(set! err #f)
          (guard (e (else #f))
            (close-input-port in))
          (guard (e (else #f))
            (close-output-port out))
          ;(guard (e (else #f))
          ;  (close-output-port err))
          #t)))))



(provide "eval-sv/nullport")

