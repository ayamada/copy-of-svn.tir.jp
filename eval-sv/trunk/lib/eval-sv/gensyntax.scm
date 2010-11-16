;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; "eval with supervise" syntaxes define module
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

;;; このモジュールでは、R5RS及びGauche標準のspecial form及びマクロの
;;; 再定義を行う。
;;; 再定義した結果は、*table:syntax-entity->replacement*内に、
;;; マクロ(または元のsyntaxのまま)の実体として保存される。

;;; 憶え書き:
;;; ループ(再帰含む)に関する停止問題を回避するには、
;;; - 全ての手続き類は、漏れなく監視実行されるものとする
;;; - ループを生成する手続き類は、ループ内に監視実行をこっそり含めるようにする
;;; のどちらかが必要になるが、現実的に、前者は無理。
;;; - 無監視な手続き類が存在する可能性がある
;;; - ループ内を空/即値のみにできる可能性がある
;;; よって、ループを生成する手続きには監視実行を含めるようにする事。

(define-module eval-sv.gensyntax
  (use srfi-1)
  (use util.list)
  ;(use gauche.parameter)
  (use eval-sv.common)
  (use eval-sv.genroot)
  (use eval-sv.cf-arity)

  (export
    *table:syntax-entity->replacement*
    specialform->sv-specialform)
  )
(select-module eval-sv.gensyntax)


;;; ----


;; syntax(Gauche提供のmacroまたはspecial form)一覧を保持するhash-table。
;; keyはentity、valは代替マクロ(一部は元のentityそのまま)。
(define *table:syntax-entity->replacement*
  (make-hash-table 'eq?))

(define (register-syntax key-entity . opt-val)
  (let1 val (get-optional opt-val key-entity)
    (hash-table-put! *table:syntax-entity->replacement* key-entity val)))

(define-macro (register-syntax/sv symbol)
  (let* ((key-entity (eval symbol (current-module)))
         (val-entity (specialform->sv-specialform symbol key-entity)))
    `(register-syntax ,key-entity ,val-entity)))


;;; ----
;;; ユーティリティ手続きを用意する

(define (entity->hidden-symbol entity)
  (guard (e (else (error "syntax-entity not found" entity e)))
    (cadr (hash-table-get *table:specialform-entity->symbols* entity))))
(define e2hs entity->hidden-symbol)

;; syntax-entityを隠し束縛のシンボルに変換してから、マクロでくるむ
;; (name-symbolはマクロ生成時に与える(表示上の)名前としてのみ使われる)
;; 大抵のspecial formはこれで機能するが、一部、これでは機能しないものもある
;; そのようなspecial formは自前でマクロ化するか、
;; syntaxをそのまま提供する必要がある
(define (specialform->sv-specialform name-symbol syntax-entity)
  (let (
        ;; マクロ生成にはdefine-macroを使うが、これはdefineなので、
        ;; 何らかのシンボルに束縛する必要がある。
        ;; しかし、安易に束縛を増やしたくないので、
        ;; 一時的に使うモジュール空間を用意し、その中に束縛する。
        (tmp-module (spawn-module [atmn 'root]))
        ;; マクロ展開後に出現する各special form類の隠し束縛を得ておく
        (h:quote [e2hs quote])
        (h:quasiquote [e2hs quasiquote])
        ;; NB: Gaucheのquasiquoteは、unquoteとunquote-splicingを
        ;;     シンボルだけで判定しており、内容物は見ていないので、
        ;;     隠し束縛を使う必要はない(使ってはいけない)
        (h:syntax-symbol [e2hs syntax-entity])
        (h:define-macro [e2hs define-macro])
        (h:lambda [e2hs lambda])
        )
    ;; tmp-module内でevalにかける、次のようなS式を生成する
    ;; (define-macro (シンボル名 . args)
    ;;   `(invoke-supervisor
    ;;      'syntax
    ;;      'シンボル名
    ;;      (lambda ()
    ;;        (実行するsyntaxのシンボル ,@args))
    ;;      '()))
    ;; NB: 将来は、argsをそれらしく渡せるようにしたい
    ;;     (現在は空にして、完全にlambdaに埋め込んでしまっている)
    (define (generate-sexpr args)
      `(,invoke-supervisor
         (,h:quote syntax)
         (,h:quote ,name-symbol)
         (,h:lambda ()
           (,h:syntax-symbol ,@args))
         (,h:quote ())))
      
    (let1 s `(,h:define-macro (,name-symbol . args)
               (,generate-sexpr args))
      ;; define-macroを実行し、マクロを生成する
      (eval s tmp-module))
    ;; 生成したマクロをモジュールから取り出し、返り値として返す
    ;; (これは束縛内容として使われる為、実体返しで問題無い)
    (global-variable-ref tmp-module name-symbol)))

;;; ----
;;; 実際に定義していく
;;; 方針:
;;; - 基本的には、syntaxは、伝統的マクロとlambdaに分解できる(筈)
;;;   分解後は、lambdaになる。
;;; - 基本的には、register-syntax/svで囲む
;;; - 内部にloop部分を含む可能性のあるものは、loop部分も
;;;   lambda化して、invoke-supervisorにかかるようにする

;; ----
;; R5RS syntaxes

;; quote系は今のところ、実体をそのまま使わせる
(register-syntax quote)
(register-syntax quasiquote)
(register-syntax unquote)
(register-syntax unquote-splicing)

;; TODO: define系はinternal define / toplevel define両対応にする必要がある
(define-macro (sv:define target . args)
  ;; (define (hoge) ...)形式なら、(define hoge (lambda ...))形式に変形する
  (define (transform target args)
    (if (symbol? target)
      (values target (car args)) ; (define a #f)形式なら、argsは1個固定
      (transform (car target)
                 (list ; argsは、listの必要がある
                   `(lambda ,(cdr target)
                      ,@args)))))

  ;; これで、(define symbol (lambda ...))である事が保証される
  (receive (symbol entity) (transform target args)
    ;; TODO: あとで、defineの外側をsv化する事
    `(,[e2hs define] ,symbol ,entity)))
(register-syntax define sv:define) ; 仮
(register-syntax define-syntax define-syntax) ; 仮

(register-syntax/sv set!)

(register-syntax/sv let-syntax)
(register-syntax/sv letrec-syntax)

;; NB: internal define対策は、lambda直後に,@bodiesが来てるので、
;;     今のところは大丈夫
;; TODO: 'syntax監視の追加
(define-macro (sv:let . args)
  (if (not (symbol? (car args)))
    ;; 通常let
    `(,[e2hs let] ,@args)
    ;; named let
    (let ((name (car args))
          (binds (cadr args))
          (bodies (cddr args)))
      `(,[e2hs let] ,name ,binds
         (,invoke-supervisor 'loop
                             ',name
                             (,[e2hs lambda] ()
                               ,@bodies)
                             '())))))
(register-syntax let sv:let)
(register-syntax/sv let*)
;; TODO: letrecも'loop監視が必要になる……
(register-syntax/sv letrec)

(register-syntax/sv if)
(register-syntax/sv cond)
(register-syntax/sv case)
(register-syntax/sv and)
(register-syntax/sv or)

;; beginは特殊(toplevelを維持する)なので、監視は付けない(仮)
(register-syntax begin)

;; TODO: 'syntax監視の追加
(define-macro (sv:do vars tests . bodies)
  `(,[e2hs do]
     ,vars
     ,tests
     (,invoke-supervisor 'loop
                         'do
                         (,[e2hs lambda] ()
                           ,@bodies)
                         '())))
(register-syntax do sv:do)

;; TODO: 'syntax監視の追加
(define-macro (sv:lambda args . bodies)
  ;; 展開すると、以下のような式になるようにすればよい。
  ;; (camouflage-arity
  ;;   arg-num
  ;;   has-optional?
  ;;   (lambda all-args
  ;;     (invoke-supervisor
  ;;       'proc
  ;;       #f
  ;;       (lambda ,args ,@bodies)
  ;;       all-args)))
  (receive (arg-num has-optional?) (let next ((l args)
                                              (num 0))
                                     (cond
                                       ((symbol? l) (values num #t))
                                       ((null? l) (values num #f))
                                       (else (next (cdr l) (+ 1 num)))))
    `(,camouflage-arity
       ,arg-num
       ,has-optional?
       (,[e2hs lambda] all-args
         (,invoke-supervisor
           'proc
           #f ; シンボル無し
           (,[e2hs lambda] ,args ,@bodies)
           all-args)))))
(register-syntax lambda sv:lambda) ; 仮

(register-syntax/sv delay)


;; ----
;; Gauche syntaxes

(register-syntax define-macro define-macro) ; 仮
(register-syntax define-generic define-generic) ; 仮
(register-syntax define-constant define-constant) ; 仮
(register-syntax define-method define-method) ; 仮
(register-syntax define-inline define-inline) ; 仮
(register-syntax define-class define-class) ; 仮

(register-syntax/sv receive)
(register-syntax/sv and-let*)
(register-syntax/sv when)
(register-syntax/sv unless)
(register-syntax/sv %macroexpand-1)
(register-syntax/sv lazy)

;; TODO: 展開ループ対策が必要(自前で%macroexpand-1を使って実装する)
(register-syntax %macroexpand %macroexpand) ; 仮

;; モジュール関係は使わない筈なので、そのまま登録しておく(仮)
(register-syntax define-module)
(register-syntax define-in-module)
(register-syntax import)
(register-syntax export)
(register-syntax export-all)
(register-syntax extend)
(register-syntax current-module)
(register-syntax select-module)
(register-syntax with-module)
(register-syntax require)

;; 内部用なので通常は使われない
(register-syntax eval-when)


;; ----
;; Gauche macros
;; マクロは基本的にそのまま使って問題ない。
;; (例によって無限ループが発生するものには対処が必要。
;;  ……と思ったが、マクロは全てspecial formか手続きに展開されるので、
;;  special formや手続きで無限ループ対策が完璧なら、
;;  特に対策を行う必要は無かった。)

;; まず、安全なものをまとめて登録しておく
(for-each
  (lambda (symbol)
    (and-let* ((entity (eval symbol (current-module))))
      (register-syntax entity)))
  '(
    use
    with-signal-handlers
    let1
    export-if-defined
    guard
    cond-expand
    use-version
    unwind-protect
    add-load-path
    rxmatch-let
    cute
    syntax-error
    rxmatch-if
    syntax-errorf
    rxmatch-cond
    push!
    rxmatch-case
    require-extension
    pop!
    inc!
    dec!
    define-condition-type
    update!
    $
    condition
    check-arg
    $*
    get-optional
    let-optionals*
    get-keyword*
    let-keywords*
    case-lambda
    let-keywords
    debug-print
    time
    program
    begin0
    fluid-let
    define-values
    values-ref
    autoload
    cut
    set!-values

    rec ; letrecに展開されるので、そっちに対応を入れておけばok
    let/cc ; call/ccに展開される
    ;%guard-rec ; 内部用っぽい
    while ; doに展開される
    until ; doに展開される
    dotimes ; doに展開される
    dolist ; for-eachに展開される
    ))


(provide "eval-sv/gensyntax")

