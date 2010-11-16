;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; "eval with counting up (lite)" module
;;;
;;;  Copyright (c) 2008 Atsuo Yamada, All rights reserved.
;;;
;;;  Permission is hereby granted, free of charge, to any person
;;;  obtaining a copy of this software and associated documentation
;;;  files (the "Software"), to deal in the Software without restriction,
;;;  including without limitation the rights to use, copy, modify,
;;;  merge, publish, distribute, sublicense, and/or sell copies of
;;;  the Software, and to permit persons to whom the Software is
;;;  furnished to do so, subject to the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;  AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
;;;  OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;;;  IN THE SOFTWARE.


;;; このモジュールは、実行カウント付き評価器(のジェネレータ)を提供する。

;;; 使い方:
;;; - (make-eval/cu ...)によって、eval/cu及び、eval/cuが内部に保持している
;;;   環境が得られる。
;;; - (eval/cu expr threshold)で、exprを評価する。
;;;   thresholdカウント内にexprが終了しない場合は、規定の例外が投げられる。
;;;   (これとは別に、マクロ展開や、do等のループが一定回数に達した場合も、
;;;    規定の例外が投げられる。)
;;; -- 尚、例外と共に続行する為の継続も投げられるので、
;;;    その継続を実行すれば評価を再開する事も一応可能。
;;; - (eval-cu-last-count)で、最後に正常終了した際の、
;;;   最後の実行値が得られる。
;;;   (例外終了した時は正しい実行値は得られない。必要なら、
;;;    例外オブジェクトから得る事。)
;;; - あとは、大体、eval/svと同じ。eval/cu自身をimport/svする事も可能。


;;; usage
#|
(define-values (eval/cu env)
  (make-eval/cu :isolate-port? bool
                :parent-module [atmn 'r5rs+]
                :bind-alist '()
                :default-threshold 200
                :macro-threshold 200
                :loop-threshold 200
                ))
(guard (e ((<eval-cu-exceeded> e)
           (let ((type (ece->type e)) ; 'proc or 'macro or 'loop
                 (count (ece->count e)) ; その時点でのカウンタ値
                 (continue (ece->continue e))) ; 続行する為の継続
                 ;; ※continueを使って続行した場合も、カウンタ値は
                 ;;   続行され続ける事に注意
                 ;;   (次回に例外が呼び出されるのはthresholdの次の倍数)
             ;; 実際のオーバーカウント処理をここに書く
             ...))
          (else
            ;; その他のエラー処理をここに書く
            ...))
  ;; exprにはevalしたい式を、thresholdにはカウントの閾値を設定する
  (eval/cu expr threshold))

(eval-cu-last-count)
;; これを実行する事で、最後に実行したeval/cuのカウント値が得られる。
;; (これは、eval/cuは正常に終了したが、
;;  カウントがどこまで進んだのか知りたい時に使える)
;; これはparameterなので、入れ子やthread動作等で不安がある場合は、
;; parameterizeしておけば安全に参照する事が出来る。
|#


(define-module eval-cu-lite
  (use gauche.parameter)
  (use eval-sv)

  (export
    make-eval/cu
    import/sv
    import/cu
    enfold-entity
    eval-cu-last-count
    atmn
    <eval-cu-exceeded>
    ece->type
    ece->count
    ece->continue
    ))
(select-module eval-cu-lite)

;;; ----

(define import/sv import/sv)
(define import/cu import/sv)
(define enfold-entity enfold-entity)
(define atmn atmn)

;; デフォルトの閾値
(define *default-threshold* 200)
(define *macro-threshold* 200)
(define *loop-threshold* 200)

(define *unspecified* (gensym))

;; parameter
(define p:nested? (make-parameter #f))
(define eval-cu-last-count (make-parameter #f))

;;; ----

;; コンディションを定義する
(define-condition-type <eval-cu-exceeded> <error>
  eval-cu-exceeded?
  (type ece->type)
  (count ece->count)
  (continue ece->continue))


(define-macro (ke symbol) ; keyword-expand
  `(if (eq? ,symbol *unspecified*)
     '()
     (list ,(make-keyword symbol) ,symbol)))

(define (make-eval/cu . keywords)
  (let-keywords keywords ((isolate-port? *unspecified*)
                          (parent-module *unspecified*)
                          (bind-alist *unspecified*)
                          (default-threshold *default-threshold*)
                          (macro-threshold *macro-threshold*)
                          (loop-threshold *loop-threshold*)
                          )
    ;; 内部で保持するeval/svとenvを用意する
    (receive (eval/sv env) (apply
                             make-eval/sv
                             `(
                               ,@(ke isolate-port?)
                               ,@(ke parent-module)
                               ,@(ke bind-alist)
                               ))
      ;; 返り値として返すeval/cuを用意する
      (define (eval/cu expr . opt-threshold)
        (let ((threshold (get-optional opt-threshold default-threshold))
              ;; 内部で保持する各値を用意する
              (proc-count 0) ; これがthresholdに達したらproc実行、0に戻る
              (proc-total 0) ; procの総カウント数(procは減るので、別途用意)
              (macro-count 0)
              (macro-total 0)
              (loop-count 0)
              (loop-total 0)
              )
          ;; eval/svに渡す手続きを生成する
          (define (supervise-proc type symbol expr args return except)
            ;; 無限再帰実行を防ぐ為に、実行よりも先にカウントを行う必要がある
            (let/cc continue
              ;; TODO: 類似の処理が多いのでlet-syntaxとかでマクロ化したい
              (cond
                ((or (eq? type 'proc) (eq? type 'syntax))
                 (inc! proc-count)
                 (inc! proc-total)
                 (when (<= threshold proc-count)
                   (set! proc-count (- proc-count threshold))
                   (except <eval-cu-exceeded>
                           :type 'proc
                           :count (- proc-total 1)
                           :continue continue
                           "proc limit exceeded")))
                ((eq? type 'macro)
                 (inc! macro-count)
                 (inc! macro-total)
                 (when (<= macro-threshold macro-count)
                   (set! macro-count (- macro-count macro-threshold))
                   (except <eval-cu-exceeded>
                           :type 'macro
                           :count (- macro-total 1)
                           :continue continue
                           "macro limit exceeded")))
                ((eq? type 'loop)
                 (inc! loop-count)
                 (inc! loop-total)
                 (when (<= loop-threshold loop-count)
                   (set! loop-count (- loop-count loop-threshold))
                   (except <eval-cu-exceeded>
                           :type 'loop
                           :count (- loop-total)
                           :continue continue
                           "loop limit exceeded")))
                (else
                  (error "assertion"))))
            ;; カウント処理後、プロセスを実行する
            (apply expr args))

          ;; 実行前に、ネストフラグを立てる
          (parameterize ((p:nested? #t))
            (receive r (eval/sv expr supervise-proc)
              (eval-cu-last-count proc-total)
              (apply values r)))))

      ;; 生成したeval/cuとenvを返す
      (values eval/cu env))))


;;; ----

(provide "eval-cu-lite")


