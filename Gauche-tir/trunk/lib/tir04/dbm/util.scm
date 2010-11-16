;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; dbm�ΰ٤Υ桼�ƥ���ƥ���³������
;;; ���ȤǤ⤦�����ɲä��롣

(define-module tir04.dbm.util
  (use srfi-1)
  (use dbm)

  (export
    with-dbm-open
    ))
(select-module tir04.dbm.util)


;; usage:
;; (with-dbm-open
;;   <qdbm>
;;   :path "/path/to/dbm.file"
;;   :rw-mode :read
;;   :key-convert #f
;;   :value-convert #t
;;   (lambda (dbm)
;;     ;; dbm��Ȥä�����Ԥ�
;;     ;; ����proc�μ¹Ԥ���λ����ȡ���ưŪ��dbm���Ĥ�����
;;     ))
(define (with-dbm-open dbm-type . dbm-open-args&proc)
  (let ((dbm-open-args (drop-right dbm-open-args&proc 1))
        (proc (car (last-pair dbm-open-args&proc))))
    (let1 dbm #f
      (dynamic-wind
        (lambda ()
          (set!
            dbm
            (apply dbm-open dbm-type dbm-open-args)))
        (lambda ()
          (proc dbm))
        (lambda ()
          (dbm-close dbm)
          (set! dbm #f))))))


(provide "tir04/dbm/util")
