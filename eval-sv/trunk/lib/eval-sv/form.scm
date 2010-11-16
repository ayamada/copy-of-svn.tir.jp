;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; "eval with supervise" form expand module
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

;;; このモジュールでは、eval内の、Gaucheでのpass1に相当する
;;; フォーム展開部分を受け持つ。
;;; 要は、マクロとspecial form(以下、この二つを統合してformと呼ぶ)を展開して、
;;; 式からformをなくす事を目的とする。
;;; 但し、通常のpass1とは違い、以下の挙動を行わなくてはならない。
;;; - special form展開後の、本来の処理を行う部分をlambdaで囲み、
;;;   そのlambdaをinvoke-supervisorに渡す必要がある
;;; - マクロ展開一回毎にinvoke-supervisorを実行する必要がある
;;;   (これもspecial form同様、マクロ展開一回の処理を行う部分をlambdaで囲み、
;;;    そのlambdaをinvoke-supervisorに渡す事)
;;;
;;; 尚、このeval-svでの実装では、special formは基本的にマクロ化されているので
;;; 例外を除いて、マクロ展開のみ行えば問題無い。
;;; 例外になるのは以下の二つ。
;;; - quote系
;;; -- quasiquote内のunquote系を自前で実装するか、どうにかする必要がある。
;;;    マクロで実装可能なら、マクロで実装してしまえばいい。
;;; - define系
;;; -- マクロによるラッピングでは、define系に以下の問題が発生する。
;;; --- lambdaで囲むとtop levelではなくなる。
;;; --- define系は基本的にletrec動作なので、定義順がおかしくなる。
;;; とりあえず現状では、quote系は直に提供してしまい、
;;; define系は未対応となっている。
;;; internal defineかtop level defineかについては、
;;; この段階で判定を行う必要がある。
;;; 判定結果はparam:is-toplevel?に反映すれば良いものとする。


(define-module eval-sv.form
  (use srfi-1)
  (use gauche.parameter)
  (use eval-sv.common)
  (use eval-sv.genroot)
  (use eval-sv.gensyntax)
  (export
    expand-form
    ))
(select-module eval-sv.form)


;;; ----

(define *not-found* (gensym))

;; マクロとspecial formの展開を行う
;; 現在のところはインチキ実装。
;;
;; - internal defineは、let等がシンボルdefineを見て展開しているので、
;;   そのまま放置で良い。
;;   (そのしわ寄せは、let等のマクロ化部分に行く)
;; - top level defineは、defineのみここでマクロ展開を行って解決する。
;; -- define系は事前に、マクロ展開時に(param:is-toplevel?)を見て
;;    挙動を変える実装にしておく
;; -- ここで、トップレベル部分、つまり、exprが(begin ...)になっている部分を
;;    再帰的に検索し、そこにdefine系が存在しているなら、
;;    (parameterize ((param:is-toplevel? #t))
;;      (unwrap-syntax (macroexpand child-expr)))
;;    を行って、手動でマクロを展開しておく。
;;    これは、define系かつトップレベルに限定されるので、unquoteのような
;;    問題は発生せずに展開する事が出来る。

(define (expand-form expr env)
  ;; eval/svは、(loadではなく)eval型のインターフェースなので、
  ;; exprは単一のS式または即値類になる。
  ;; よって、通常のload的に、複数のS式が並んで来る場合は、
  ;; それらは(begin ...)によって囲まれて単一のS式になっている、と仮定して良い。

  ;; とりあえず仮に、トップレベルdefineの対応だけ行う。
  ;; (本当なら、監視しながら全マクロを一つずつ展開する必要がある)
  (expand-top-level-define expr env))


(define (expand-top-level-define expr env)
  ;; 要素が正規のlistである事を保証する手続き
  ;; (listでない場合はその値を返し、不正規なlistの場合はエラーを投げる)
  (define (assure-proper-list l proc)
    (cond
      ((not (pair? l)) l) ; listでないなら、そのまま返す
      ;; 不完全なlistならエラーにする
      ((not (list? l)) (error "not proper list" l))
      (else
        (proc l))))
  ;; トップレベルに属する部分だけを検索するmap風手続き
  (define (top-level-map proc expr)
    (assure-proper-list
      expr
      (lambda (expr)
        (let next ((l expr))
          (if (null? l)
            '() ; 終端に達した
            (let ((target (car l))
                  (rest (cdr l)))
              (let1 result (proc target)
                (cons result (next rest)))))))))

  (define (convert-node node)
    (assure-proper-list
      node
      (lambda (node)
        ;; carの実体を取り出す
        (let1 target (if (symbol? (car node))
                       (global-variable-ref env (car node) *not-found*)
                       *not-found*)
          (cond
            ;; 未定義の束縛値。ローカル変数等なのでそのままにする
            ((eq? *not-found* target) node)
            ;; beginなら、再帰的に処理を行う
            ((eq? begin target)
             (cons
               (car node) ; beginのsymbol
               (top-level-map convert-node (cdr node)))) ; 再帰検索
            ;; define*系なら、マクロを展開する
            ;; TODO: unwrap-syntaxはモジュールを考慮しないので、
            ;;       場合によっては、identifierからモジュールも取り出し、
            ;;       with-moduleか何かを行う必要があるかも知れない
            ((hash-table-exists? *top-level-define-entity-table* target)
             (eval `(,unwrap-syntax (,macroexpand ,node)) env))
            ;; それ以外なら、そのままにしておく
            (else node))))))

  (if (car-is-begin? env expr)
    (parameterize ((param:is-toplevel? #t))
      (top-level-map convert-node expr))
    expr))

(define *begin-entity*
  (hash-table-get *table:syntax-entity->replacement* begin))

(define (car-is-begin? env expr)
  (and
    (pair? expr)
    (list? expr)
    (symbol? (car expr))
    (eq?
      *begin-entity*
      (global-variable-ref env (car expr) *not-found*))))


(define *top-level-define-entity-table*
  (let1 table:entity->exists (make-hash-table 'eq?)
    (for-each
      (lambda (entity)
        (hash-table-put!
          table:entity->exists
          (hash-table-get *table:syntax-entity->replacement* entity)
          #t))
      (list
        define
        define-syntax
        define-generic
        define-constant
        define-method
        define-inline
        define-class
        define-macro
        ))
    table:entity->exists))


;;; ※マクロの自前展開について
;;; - '(macro 123)
;;;   のようなS式をマクロ展開する事を考えると、
;;;   (unwrap-syntax (macroexpand '(macro 123)))
;;;   で終わりでいい。
;;; - '(list (macro 123) (macro2 456))
;;;   のようなS式をマクロ展開する事を考えると、
;;;   全体を一つのlistとして見て、再帰的に
;;;   (lambda (elem) (unwrap-syntax (macroexpand elem)))
;;;   をかけていけばよい。
;;; - では、
;;;   '(quasiquote ((macro1 123) (unquote (macro2 456))))
;;;   のようなS式ではどうすれば良い？
;;;   おそらく、quasiquoteが子要素を見て行き、unquoteを発見したら、
;;;   unquoteの子要素を再帰的に展開する必要があるとは思うが……。
;;;
;;; ……という問題があるので、マクロの自前展開は実は困難だった。
;;; これを真面目に解決する場合、要するに、マクロの展開と
;;; special formの内部形式への変換を同時にこなしていく必要がある。
;;; つまり、マクロ展開だけでなく、special formも自前で実装する必要がある！
;;;
;;; ……それは大変なので、今回は、マクロ展開監視については諦める。
;;; gauche.night終了後に実装する。


;;; ----

(provide "eval-sv/form")

