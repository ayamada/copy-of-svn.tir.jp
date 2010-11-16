;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$


;;; ToDo: プロトコル部分をclient.scmとserver.scmから、
;;;       ここに移動させる事
;;;       (共用可能な部分が多い筈なので)


(define-module tir03.setp.protocol
  (export
    get-setp-protocol-version
    ))
(select-module tir03.setp.protocol)


(define (get-setp-protocol-version)
  0.0)


(provide "tir03/setp/protocol")


