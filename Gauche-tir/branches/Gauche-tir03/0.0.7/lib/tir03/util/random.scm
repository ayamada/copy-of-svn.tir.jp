;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; 汎用乱数関連モジュール。

;;; usage:
;;; - (dice) => 1-6のどれかが出る
;;; - (dice 10) => 1-10のどれかが出る
;;; - (dice-1 10) => 0-9のどれかが出る

;;; note: 乱数のseedを保存する為に取り出したり、セットしたりする
;;;       インターフェースを用意する？？？

;;; note: normal-distribution-randomについては、以下のurlの正規乱数を参照
;;; - http://ja.wikipedia.org/wiki/%E4%B9%B1%E6%95%B0
;;; -- ボックス=ミューラー法による実装



(define-module tir03.util.random
  (use math.mt-random)
  (use math.const)

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
    two-standard-normal-distribution-random
    ;; (two-standard-normal-distribution-random) は、
    ;; 二つの標準正規分布乱数を多値で返す。
    ;; 高速に大量の正規乱数を取得する際に利用可能。
    standard-normal-distribution-random
    ;; (standard-normal-distribution-random) は、
    ;; 一つの標準正規分布乱数を返す。
    normal-distribution-random
    ;; (normal-distribution-random . keywords) は、
    ;; 与えられたkeywordsから、要求通りの正規乱数を返す。
    ;; keywordsの種類はこの関数の冒頭部を参照する事。
    ))
(select-module tir03.util.random)


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



(define (two-standard-normal-distribution-random)
  ;; まず、「（0, 1］」の乱数を二つ用意する
  (define (get-r)
    (- 1 (mt-random-real0 *mt*)))
  (let ((alpha (get-r))
        (beta (get-r)))
    ;; 以下の計算を行う事で、二つの相関の無い正規乱数が得られる。
    ;; ((-2 * log(α))^0.5) * sin(2 * π * β)
    ;; ((-2 * log(α))^0.5) * cos(2 * π * β)
    ;; 左辺(result-alpha-part)と、sin/cosの中味(result-beta-part)を先に求める
    (let ((result-alpha-part (expt
                               (* -2 (log alpha))
                               0.5))
          (result-beta-part (* 2 pi beta)))
      ;; 二つの正規乱数を返す
      (values
        (* result-alpha-part (sin result-beta-part))
        (* result-alpha-part (cos result-beta-part))))))

(define (get-sndr-1)
  (receive (r1 r2) (two-standard-normal-distribution-random)
    (set! *sndr-maker* (lambda ()
                         (set! *sndr-maker* get-sndr-1)
                         r2))
    r1))

(define *sndr-maker* get-sndr-1)


(define (standard-normal-distribution-random)
  (*sndr-maker*))


(define (normal-distribution-random . keywords)
  (let-keywords* keywords (
                           (sigma 1) ; 分散度を指定。
                           ;; sigmaが大きいと平均的に分散し、
                           ;; sigmaが0に近いとmu近辺しか出なくなる。
                           (sigma-plus #f) ; +方向と-方向で、sigmaの値を
                           (sigma-minus #f) ; 変更したい場合に個別に指定する。
                           ;; 尚、sigma-plusかsigma-minusにマイナスの値を
                           ;; 指定する事で、半分に折り返した正規分布を
                           ;; 作る事も可能。
                           (mu 0) ; 一番出やすい期待値(平均の中央)を指定。
                           (clamp-min #f) ; 結果をclampする場合に指定
                           (clamp-max #f) ; 結果をclampする場合に指定
                           )
    (let1 ndr (standard-normal-distribution-random)
      (clamp
        (+ mu
           (* ndr
              (or
                (if (positive? ndr) sigma-plus sigma-minus)
                sigma)))
        clamp-min
        clamp-max))))


(provide "tir03/util/random")

