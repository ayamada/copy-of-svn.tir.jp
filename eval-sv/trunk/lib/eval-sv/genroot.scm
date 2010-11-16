;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; "generate root" of "eval with supervise" module
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


;;; このモジュールは、以下の三機能を持つ。
;;; - syntax関連の束縛と手続きを提供する。
;;; - テンプレートモジュールを生成する為のユーティリティマクロを提供する。
;;; - [atmn 'root]モジュールを生成する。
;;; -- binds.scmが生成するテンプレートモジュールや、enfold.scmは、
;;;    この[atmn 'root]モジュールが正しく生成されていないと、機能しない。
;;; --- binds.scmのテンプレートモジュール生成はuse段階なので、
;;;     どうにかして先に生成しておく必要があり、
;;;     このテンプレートモジュール生成にenfold.scmも必要とされる為、
;;;     結局、[atmn 'root]生成を最優先で行っておく必要がある。

;;; TODO: テーブル部分だけ別モジュールに追い出すべきかも？


(define-module eval-sv.genroot
  (use srfi-1)
  (use util.list)
  (use eval-sv.common)

  (export
    *table:specialform-entity->symbols*
    syntax-table-register
    *alist:syntax:symbol.entity*
    define-template-module
    ))
(select-module eval-sv.genroot)



;;; ----


;; 隠し束縛の情報を保持するhash-table
;; keyはentity、valは'(original-symbol gensym-symbol)
;; (keyがentityなのは、symbolからentityへの変換は容易だが、逆は難しい為)
(define *table:specialform-entity->symbols*
  (make-hash-table 'eq?))

;; *table:specialform-entity->symbols*へのテーブル登録手続き
;; (この手続きは隠し束縛シンボルを返す)
;; NB: モジュールへの登録は自前で行う事
(define (syntax-table-register symbol entity)
  (let1 internal-symbol (gensym (symbol->string symbol))
    ;; テーブルに登録する
    (hash-table-put! *table:specialform-entity->symbols*
                     entity
                     (list symbol internal-symbol))
    ;; 生成されたシンボルを返す
    internal-symbol))


;; syntaxのシンボル一覧をlistにして取る
(define *alist:syntax:symbol.entity*
  (let ()
    (define (get-symbol->entity-alist module-symbol)
      (let* ((module (find-module module-symbol))
             (symbol-list (map car (hash-table->alist (module-table module))))
             )
        (filter-map
          (lambda (symbol)
            (let1 entity (eval symbol module)
              (and
                (is-a? entity <syntax>)
                (cons symbol entity))))
          symbol-list)))
    (append
      (get-symbol->entity-alist 'null)
      (get-symbol->entity-alist 'gauche))))




;; テンプレートモジュールのベースを定義する為のマクロ
(define-macro (define-template-module module-symbol . extends)
  (let ((name (unwrap-syntax (macroexpand module-symbol)))
        (args (map
                (lambda (x)
                  (unwrap-syntax (macroexpand x)))
                extends))
        )
    `(define-module ,name
       (extend ,@args)
       (,provide ,(module-name->path name)))))



;;; ----


;; rootモジュールを用意する
(define-template-module [atmn root])

;; 基礎syntaxをrootに隠しimportする
(let1 root (find-module [atmn 'root])
  (for-each
    (lambda (symbol+entity)
      (let ((symbol (car symbol+entity))
            (entity (cdr symbol+entity)))
        (bind-in-module-entity
          root
          (syntax-table-register symbol entity)
          entity)))
    *alist:syntax:symbol.entity*)
  #t)


(provide "eval-sv/genroot")

