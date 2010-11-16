;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; "eval with supervise" common define module
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



(define-module eval-sv.common
  (use gauche.parameter)

  (export
    param:isv-proc
    param:eval/sv
    param:env
    param:is-toplevel?

    atmn
    invoke-supervisor
    bind-in-module-entity
    spawn-module
    ))
(select-module eval-sv.common)


;;; ----
;;; parameter変数、及び、ユーティリティ手続き類など

;; テンプレート親モジュールの組み込み手続き類が、envに割り当てられたproc等を
;; 参照できるようにする為のパラメータを用意する
(define param:isv-proc (make-parameter #f))
(define param:eval/sv (make-parameter #f))
(define param:env (make-parameter #f))

;; eval/sv内での、トップレベル判定用のパラメータ
(define param:is-toplevel? (make-parameter #f))


;; テンプレートモジュールのシンボルを短く表現する為のマクロ
(define-macro (abbr-template-module-name name)
  (let1 append-root (lambda (symbol)
                      (string->symbol
                        (string-append
                          "eval-sv.template."
                          (symbol->string symbol))))
    (cond
      ((and ; name => (quote symbol)
         (list? name)
         (= 2 (length name))
         (eq? 'quote (car name))
         (symbol? (cadr name)))
       `(quote ,(append-root (cadr name))))
      ((symbol? name) ; name => symbol
       (append-root name))
      (else
        (error "invalid symbol" name)))))
(define atmn abbr-template-module-name)
;; [atmn hoge] => eval-sv.template.hoge
;; [atmn 'hoge] => 'eval-sv.template.hoge


;; 監視手続きの実体を用意する
;; (実際には、param:isv-procに引数を渡しているだけ)
(define (invoke-supervisor type symbol entity args)
  (if (param:isv-proc)
    ((param:isv-proc) type symbol entity args)
    (apply entity args)))



;; define-in-moduleはsyntaxなのでモジュールオブジェクトを渡せないので、
;; 手続き化してモジュールオブジェクトを渡せるようにする
;; また、exprがlistの時にS式として実行されると困るので、
;; 間違った解釈をされないように、明示的にquoteして渡す。
;; (即値等を渡す場合でも、quoteしてしまって問題ない)
(define (bind-in-module-entity module symbol expr)
  (eval `(define-in-module ,module ,symbol (quote ,expr)) (current-module)))



;; 親としてparent-moduleのみを持つ、新しい無名moduleを生成する
;; 引数として、複数の(module名の)symbolを受け取る。
(define (spawn-module . parent-modules)
  (let1 m (make-module #f)
    (eval `(extend ,@parent-modules) m)
    m))



(provide "eval-sv/common")

