;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;; continual constant state holder

;; TODO:
;; 今のところ、インスタンスの同一性判定は、identity-symbolに頼っているが、
;; これでは問題がある。
;; 過去のインスタンスがあり、そして、そこから派生した未来のインスタンスが
;; ある場合、これは現在の実装でも正しく比較できる。
;; しかし、ある過去のインスタンスから複数の未来のインスタンスが生成される場合、
;; それぞれの未来のインスタンス同士は同一判定されるべきでないように思える。
;; (これは同一判定の用途による、が、詳細は省略)
;; これを実装する場合、identity-symbolによる実装ではなく、
;; 未来のインスタンス内に、過去のインスタンスへのリファレンスを保持する
;; ようにする必要がある。
;; しかし、そうすると、いつまでたっても過去のインスタンスをGCする事が
;; できなくなってしまう問題もある。
;; (何らかのweak reference実装にすれば、一応は回避可能だが)


;; 何をするものか:
;; - 時間経過(状況の変化)に応じて、オブジェクトの状態は変化するが、
;;   過去の特定ポイントでのオブジェクトそのものは変化しない、
;;   (つまり、履歴的な過去へのアクセスを保証する)
;;   という性質を実現する為に、オブジェクトの状態の変化を、破壊的変更ではなく、
;;   「状態の違う新しいオブジェクトをspawnし、今後はそっちを指すようにする」
;;   という実装にしたい。
;; - つまり、副作用の無い、オブジェクト更新を提供する。
;; - 副作用を追放する事で、履歴的アクセスが可能なインスタンスを実現する。

;; 使い方:
;; - <continual-constant-state-holder>を継承したクラスを作る。

;; 特徴:
;; - immutableである。
;; - インスタンスの状態の変化は、新しいインスタンスをspawnし、
;;   新しいインスタンスの状態を変化させ、それを新しい状態とする。
;; -- 古いインスタンスを指しているものは、古いインスタンスを指したままに
;;    しておく事ができる。
;; - インスタンス毎に、自己同一性をチェックする為の識別子を持つ。
;; -- spawnで生成されたインスタンスとその生成元のインスタンスは、
;;    同一の識別子を持つ。
;; -- この識別子は、空にする事が可能とする。
;; - このクラスのインスタンスは「空」の状態が可能とする。
;; -- 識別子を持たないインスタンスは全て「空」扱いとする。
;; -- 「空」同士の比較は#fを返すものとする。

;; 仕様:
;; - spawn時にスロットの値(の参照)をコピーする関係上、
;;   仮想スロットを実装すると問題が発生する可能性がある。
;;   仮想スロットは使わない方が無難。
;; - spawn時にスロットに値をセットする為に:init-keywordを使う為、
;;   スロットには必ず:init-keywordを設定しなくてはならない事に注意する事！

(define-module tir04.util.ccsh
  (use srfi-1)
  (use srfi-2)

  (export
    <continual-constant-state-holder>
    identity-symbol-of
    void?
    spawn ; 少しだけスロット内容の違う別インスタンスを新たに生成する。
    ))
(select-module tir04.util.ccsh)


(define-class <continual-constant-state-holder> ()
  (
   (identity-symbol ; instanceの自己同一性を判断する為の識別子
     :accessor identity-symbol-of
     :init-keyword :identity-symbol
     :init-form (gensym))
   ))

;; TODO: :identity-symbolが#fのインスタンスの扱いをどうするかがまだ未定
;;       (おそらく、無効になったインスタンスを意味させる？)
(define-method void? ((self <continual-constant-state-holder>))
  (not
    (not
      (identity-symbol-of self))))

;; equal?で、identity-symbolが同一(つまり、由来が同じ)かどうかを
;; 調べられるようにする。
;; 完全に同一かを調べるにはeq?を使う事。
;; NB: これはトラブルの元になる可能性がある。
(define-method object-equal? ((obj1 <continual-constant-state-holder>)
                              (obj2 <continual-constant-state-holder>))
  (and
    (not (void? obj1))
    (not (void? obj2))
    (eq?
      (identity-symbol-of obj1)
      (identity-symbol-of obj2))))

(define (keywords-merge self keywords)
  ;; 概要:
  ;; selfの持つスロットからキーワードを抽出し、それをkeywordsで上書きし、
  ;; その結果のlistを返す。
  ;; (結果として、(apply make (class-of self) result)として適用できる
  ;;  resultが返される事になる)
  ;; ※但し、スロットが:init-keywordを持たない場合、それは無視される
  ;;   (:init-keyword無し = spawn時に上書きしなくてよい、という意味にとらえる)
  (apply
    append
    (filter-map
      (lambda (slot-define)
        ;; :init-keyword指定のないスロットはコピーしない
        (and-let* ((init-keyword (get-keyword :init-keyword
                                              (cdr slot-define)
                                              #f)))
          (let1 slot-val (get-keyword init-keyword
                                      keywords
                                      (slot-ref self (car slot-define)))
            (list init-keyword slot-val))))
      (class-slots (class-of self)))))

(define-method spawn ((self <continual-constant-state-holder>) . keywords)
  (let1 merged-keywords (keywords-merge self keywords)
    (apply
      make
      (class-of self)
      merged-keywords)))




(provide "tir04/util/ccsh")
