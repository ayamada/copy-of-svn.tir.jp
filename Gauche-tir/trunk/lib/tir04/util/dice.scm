;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ダイスモジュール。

;;; usage:
;;; - (dice) => 1-6のどれかが出る
;;; - (dice 10) => 1-10のどれかが出る
;;; - (dice-1 10) => 0-9のどれかが出る
;;; - (coin-flip) => #tか#fのどちらかが返る

(define-module tir04.util.dice
  (use math.mt-random)

  (export
    dice-1
    ;; (dice-1) は、0-5の乱数を返す。
    ;; (dice-1 10) は、0-9の乱数を返す。
    dice
    ;; (dice) は、1-6の乱数を返す。
    ;; (dice 10) は、1-10の乱数を返す。
    list-dice
    ;; (list-dice target-list) は、target-listの要素の一つをランダムに返す。
    ;; target-listは、正規のlistである事。
    coin-flip
    ;; #tか#fのどちらかを返す。
    ))
(select-module tir04.util.dice)


;; 考えた結果、use時に*mt*をセットするようにする事にした。
;; こうすると、毎回make-mt!を実行してチェックする必要が無くなる。
;; (代わりに、全くdice等を使わない場合にも*mt*生成でcpuとメモリを消費するが)
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

