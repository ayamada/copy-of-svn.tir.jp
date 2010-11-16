#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; 日本語識別子

;(use gauche.parameter)
;(use www.cgi)
;(use www.fastcgi)
(use xsm.xml-rpc.server.cgi)

;;; ----

(define (main args)
  (xml-rpc-server-cgi-main
   `(("echo" ,(lambda args args))
     ("math.add" ,(lambda (x y) (+ x y)))
     ("math.sub" ,(lambda (x y) (- x y)))
     ("math.mul" ,(lambda (x y) (* x y)))
     ("math.div" ,(lambda (x y) (/ x y))))))

