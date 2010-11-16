;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; �������⥸�塼�롣

;;; usage:
;;; - (dice) => 1-6�Τɤ줫���Ф�
;;; - (dice 10) => 1-10�Τɤ줫���Ф�
;;; - (dice-1 10) => 0-9�Τɤ줫���Ф�
;;; - (coin-flip) => #t��#f�Τɤ��餫���֤�

(define-module tir04.util.dice
  (use math.mt-random)

  (export
    dice-1
    ;; (dice-1) �ϡ�0-5��������֤���
    ;; (dice-1 10) �ϡ�0-9��������֤���
    dice
    ;; (dice) �ϡ�1-6��������֤���
    ;; (dice 10) �ϡ�1-10��������֤���
    list-dice
    ;; (list-dice target-list) �ϡ�target-list�����Ǥΰ�Ĥ��������֤���
    ;; target-list�ϡ�������list�Ǥ������
    coin-flip
    ;; #t��#f�Τɤ��餫���֤���
    ))
(select-module tir04.util.dice)


;; �ͤ�����̡�use����*mt*�򥻥åȤ���褦�ˤ�����ˤ�����
;; ��������ȡ����make-mt!��¹Ԥ��ƥ����å�����ɬ�פ�̵���ʤ롣
;; (����ˡ�����dice����Ȥ�ʤ����ˤ�*mt*������cpu�ȥ������񤹤뤬)
(define *mt*
  (make <mersenne-twister>
        :seed (receive (epoch micro) (sys-gettimeofday)
                (receive (q r) (quotient&remainder epoch (+ (sys-getpid) 1))
                  (+ q r micro)))))

(define (dice-1 . opt-args)
  (let1 num (get-optional opt-args 6)
    (if (zero? num)
      0
      (mt-random-integer *mt* num))))

(define (dice . opt-args)
  (+ 1 (apply dice-1 opt-args)))

(define (list-dice target-list)
  (if (null? target-list)
    (error "null-list cannot use to list-dice")
    (let* ((list-length (length target-list))
           (selected (dice-1 list-length)))
      (list-ref target-list selected))))

(define (coin-flip)
  (= 0 (dice-1 2)))


(provide "tir04/util/dice")

