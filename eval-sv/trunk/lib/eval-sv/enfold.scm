;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; "eval with supervise" enfold with superviser module
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


(define-module eval-sv.enfold
  (use srfi-1)
  (use util.list)
  (use gauche.parameter)
  (use eval-sv.common)
  (use eval-sv.genroot)
  (use eval-sv.gensyntax)
  (use eval-sv.genproc)

  (export
    import/sv ; 実体をenfold-entityしてからモジュール内にglobal束縛する
    enfold-entity ; 実体がproc/syntaxなら、実行時監視を追加したものを返す
    ))
(select-module eval-sv.enfold)


;;; ----

;; 特定モジュール内で、symbolに対してentityを束縛する。
;; 但し、entityが手続き/special form等だった場合は、
;; 適切に監視手続きを組み込む。
;; ※entityがlistやvectorやhash-table等で、内部にproc等を含んでいた場合、
;;   それらには監視手続きは組み込まれない！
;;   監視手続きを組み込みたい場合は一つずつ持ち込む事。
(define (import/sv module-entity symbol entity . opt-through-sv)
  (let-optionals* opt-through-sv ((through-sv #f))
    (bind-in-module-entity
      module-entity
      symbol
      (if through-sv
        entity
        (enfold-entity symbol entity)))))


;; procやsyntaxに、実行時監視を付加する手続き。
;; import/svではモジュール内にグローバル束縛を作ってしまうが、
;; それが嫌な時はこちらを直に使えば、監視を付加できる。
(define (enfold-entity symbol entity)
  (cond
    ((or
       (is-a? entity <procedure>)
       (is-a? entity <generic>)
       )
     (enfold-proc symbol entity))
    ((is-a? entity <macro>)
     (enfold-macro symbol entity))
    ((is-a? entity <syntax>)
     (enfold-syntax symbol entity))
    (else
      entity)))


;; この手続きは、普通のprocを、eval/sv内に設置できるようにする際に使う。
;; この手続きを通す事によって、true-procの実行を、supervise-procに
;; フィルタリングさせる事ができる。
;; 尚、enfoldの対象がGauche組み込みの手続きだった場合は、
;; 無限ループ対策として、予め対策されているものについては、
;; 自動的に代替手続きに差し換えられる。
(define (enfold-proc symbol true-proc)
  (guard (e (else
              (generate-enfolded-proc symbol true-proc)))
    (hash-table-get *table:proc->replacement* true-proc)))


;; Gauche標準のmacro及びspecial formは、基本的には
;; *table:syntax-entity->replacement*に登録されている。
;; このテーブルに登録されている場合、それをenfold後の実体として返す。
;; 登録されていない場合は以下の挙動とする事。
;; - マクロならそのまま返す。
;; - special formなら、specialform->sv-specialformにかけてマクロ化する。

(define (enfold-macro name-symbol entity)
  (hash-table-get *table:syntax-entity->replacement* entity entity))

(define (enfold-syntax name-symbol entity)
  (guard (e (else
              (specialform->sv-specialform name-symbol entity)))
    (hash-table-get *table:syntax-entity->replacement* entity)))


(provide "eval-sv/enfold")

