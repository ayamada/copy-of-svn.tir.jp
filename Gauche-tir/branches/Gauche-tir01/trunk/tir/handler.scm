#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$


(define-module tir.handler
  ;(use srfi-2) ; and-let*

  (export
    ;; thunk����ǤΤߡ����Ū������β������ѹ��������ݤ˻Ȥ���
    ;; thunk��æ�Ф���ȸ�����롣
    ;; �쥭�������ѿ����Ф���fluid-let�Τ褦�ʤ�Ρ�
    ;; dynamic-wind���ѡ�
    set-temporary-state!
    ;; thunk����ǤΤߡ����Ū��port��buffering-mode���ѹ���
    ;; thunk��æ�Ф���ȸ�����롣
    with-port-buffering
    ;; with-signal-handlers�������ʡ�
    ;; with-signal-handlers�������ư���褦�ˤʤä������ס�
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

