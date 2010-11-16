;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; 正規乱数取得モジュール。

;;; note: normal-distribution-randomについては、以下のurlの「正規乱数」を参照
;;; - http://ja.wikipedia.org/wiki/%E4%B9%B1%E6%95%B0
;;; -- ボックス=ミューラー法による実装



(define-module tir04.util.sndr
  (use math.mt-random)

  (export
    <sndr>
    set-seed! ; seed値を再設定する。
    get-state ; 内部のmtの状態を読み出す。
    set-state! ; 内部のmtの状態を再設定する。

    ;; methods
    two-standard-normal-distribution-random
    ;; (two-standard-normal-distribution-random sndr) は、
    ;; 二つの標準正規分布乱数を多値で返す。
    ;; 高速に大量の正規乱数を取得する際に利用する。
    standard-normal-distribution-random
    ;; (standard-normal-distribution-random sndr) は、
    ;; 一つの標準正規分布乱数を返す。
    ;; 効率はtwo-standard-normal-distribution-randomの半分。
    normal-distribution-random
    ;; (normal-distribution-random sndr . keywords) は、
    ;; (sndr-translation (standard-normal-distribution-random sndr))
    ;; と同義。sndr-translationについては後述。

    ;; procs
    sndr-translation
    ;; (sndr-translation sndr-number . keywords) は、
    ;; 標準正規分布乱数(sndr-number)を、keywordsで指定されたパラメータで
    ;; 特定の用途に適するように変形させるユーティリティ手続き。
    ;; keywordsの種類はこの手続きの冒頭部を参照する事。
    seed->standard-normal-distribution-random
    seed->normal-distribution-random
    ;; (seed->standard-normal-distribution-random seed) と、
    ;; (seed->normal-distribution-random seed . keywords) は、
    ;; seed値から直接、正規乱数を生成するユーティリティ手続き。
    ;; seed値とkeywordsが同じであれば、生成される正規乱数も常に同じになる為、
    ;; 一種のマッピングとして利用できる。
    ;; 但し、効率はかなり悪い。
    ))
(select-module tir04.util.sndr)


(define-class <sndr> ()
  (
   (mt
     :accessor mt-of
     :init-keyword :mt
     :init-value #f)
   ))


(define-method initialize ((self <sndr>) initargs)
  (next-method)
  (set! (mt-of self) (make <mersenne-twister>))
  (set-seed! self (or
                    (get-keyword :seed initargs #f)
                    (receive (epoch micro) (sys-gettimeofday)
                      (receive (q r) (quotient&remainder epoch
                                                         (+ (sys-getpid) 1))
                        (+ q r micro)))))
  #t)

(define-method set-seed! ((self <sndr>) seed)
  (mt-random-set-seed! (mt-of self) seed))

(define-method get-state ((self <sndr>))
  (mt-random-get-state (mt-of self)))

(define-method set-state! ((self <sndr>) state)
  (mt-random-set-state! (mt-of self) state))



(define-method two-standard-normal-distribution-random ((self <sndr>))
  ;; まず、「（0, 1］」の乱数を二つ用意する
  (define (get-r)
    (- 1 (mt-random-real0 (mt-of self))))
  (let* ((alpha (get-r)) ; 一応念の為、let*で評価順を保証しておく
         (beta  (get-r)))
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


(define-method standard-normal-distribution-random ((self <sndr>))
  (values-ref (two-standard-normal-distribution-random self) 0))


(define (sndr-translation sndr-number . keywords)
  (let-keywords* keywords (
                           (sigma 1) ; 分散度を指定。
                           ;; sigmaが大きいと平均的に分散し、
                           ;; sigmaが0に近いとmu近辺しか出なくなる。
                           (sigma-plus #f)  ; +方向と-方向で、sigmaの値を
                           (sigma-minus #f) ; 変更したい場合に個別に指定する。
                           ;; 尚、sigma-plusかsigma-minusにマイナスの値を
                           ;; 指定する事で、半分に折り返した正規分布を
                           ;; 作る事も可能。
                           (mu 0) ; 一番出やすい期待値(平均の中央)を指定。
                           (clamp-min #f) ; 結果をclampする場合に指定
                           (clamp-max #f) ; 結果をclampする場合に指定
                           )
    (clamp
      (+ mu
         (* sndr-number
            (or
              (if (positive? sndr-number) sigma-plus sigma-minus)
              sigma)))
      clamp-min
      clamp-max)))


(define-method normal-distribution-random ((self <sndr>) . keywords)
  (apply
    sndr-translation
    (standard-normal-distribution-random self)
    keywords))


(define (seed->standard-normal-distribution-random seed)
  (standard-normal-distribution-random (make <sndr> :seed seed)))

(define (seed->normal-distribution-random seed . keywords)
  (apply
    normal-distribution-random
    (make <sndr> :seed seed)
    keywords))




(provide "tir04/util/sndr")

