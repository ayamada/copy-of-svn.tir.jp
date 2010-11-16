#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$


(define-module tir.handler
  ;(use srfi-2) ; and-let*

  (export
    ;; thunkの中でのみ、一時的に特定の何かを変更したい際に使う。
    ;; thunkを脱出すると元に戻る。
    ;; レキシカル変数に対するfluid-letのようなもの。
    ;; dynamic-wind使用。
    set-temporary-state!
    ;; thunkの中でのみ、一時的にportのbuffering-modeを変更。
    ;; thunkを脱出すると元の戻る。
    with-port-buffering
    ;; with-signal-handlersの代用品。
    ;; with-signal-handlersが正常に動作するようになったら不要。
    with-signal-handler
    ))
(select-module tir.handler)


(define-syntax set-temporary-state!
  (syntax-rules ()
    ((_ old-value new-value value-setter thunk)
     (dynamic-wind
       (lambda ()
         (value-setter new-value))
       thunk
       (lambda ()
         (value-setter old-value))))))

(define (with-port-buffering port mode thunk)
  (if port
    (set-temporary-state!
      (port-buffering port)
      mode
      (lambda (new)
        (with-error-handler
          (lambda (e) #f)
          (lambda ()
            (set! (port-buffering port) new))))
      thunk)
    (thunk)))

(define (with-signal-handler signal handler thunk)
  (set-temporary-state!
    (get-signal-handler signal)
    handler
    (cut set-signal-handler! signal <>)
    thunk))


;;; ----


(provide "tir/handler")

