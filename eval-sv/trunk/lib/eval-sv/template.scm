;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; "eval with supervise" template-module define module
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

;;; このモジュールは、eval-svが使う為の以下のものを提供する。
;;; - テンプレートモジュールそのもの

;;; テンプレートに束縛する為の基礎ルール 
;;; - special formは、基本的にマクロとして実装する。
;;; -- マクロを展開すると、隠しsyntax、監視proc、eval、apply等が出現する。
;;; - マクロは、そのまま束縛してよい。
;;; -- マクロに対する監視は、展開時のみの為。
;;; - 手続きは、規定の監視を付けて束縛する。
;;; -- 内部でそのまま返ってこなくなる可能性のある手続きは、
;;;    それぞれに対してカスタマイズされた監視をつける。
;;; - 即値やlist等も、そのまま束縛してよい。
;;; -- 標準では、list等の中に手続き等が入っている事は無いっぽい。

;;; テンプレートモジュールの継承図
;;
;; [atmn root](隠しsyntaxとprimitiveなsyntaxが入ったモジュール)
;; ↓ ↓
;; ↓ [atmn syntax.*]類と[lazy等、追加のsyntax入りモジュール]
;; ↓                               ↓
;; [その他のテンプレートモジュール] ↓
;; ↓(syntax無し、macro入り)        必要に応じて、マクロを加える
;; ↓([atmn r5rs.*]等)              ↓
;; ↓                               ↓
;; ([atmn r5rs]等、テンプレート集合R5RSモジュール各種)
;; (この辺を、直近の親モジュールとして提供する)
;; (しかし、自分で生成するのも一応許す(インターフェースは無いけど))
;; ↓
;; [spawnされた無名モジュール]

;; TODO: 特定手続きのカスタマイズは、別ファイルに分けてもよい

(define-module eval-sv.template
  (use srfi-1)
  (use util.list)
  (use gauche.parameter)
  (use eval-sv.common)
  (use eval-sv.genroot)
  (use eval-sv.gensyntax)
  (use eval-sv.genproc)
  (use eval-sv.enfold)

  (export
    ;; 今のところ、無し
    ))
(select-module eval-sv.template)



;;; ----

;; import/svは引数としてモジュール実体を要求するので、
;; モジュール名で束縛できるようなユーティリティ手続きを用意する
(define (%bind/sv through-sv module-symbol symbol . opt-entity)
  (let-optionals* opt-entity ((entity (eval symbol (current-module))))
    (import/sv
      (find-module module-symbol)
      symbol
      entity
      through-sv)))
(define (bind/sv module-symbol symbol . opt-entity)
  (apply %bind/sv #f module-symbol symbol opt-entity))
(define (bind module-symbol symbol . opt-entity)
  (apply %bind/sv #t module-symbol symbol opt-entity))

;;; ----
;;; テンプレートモジュールの定義

;;; ----
;;; syntax用テンプレートモジュール

;; [atmn root]を継承した、[atmn syntax.*]モジュールを生成する

(define-template-module [atmn syntax.r5rs.quote] [atmn root])
(bind/sv [atmn 'syntax.r5rs.quote] 'quote)
(bind/sv [atmn 'syntax.r5rs.quote] 'quasiquote)
(bind/sv [atmn 'syntax.r5rs.quote] 'unquote)
(bind/sv [atmn 'syntax.r5rs.quote] 'unquote-splicing)

(define-template-module [atmn syntax.r5rs.macro] [atmn root])
(bind/sv [atmn 'syntax.r5rs.macro] 'define-syntax)
(bind/sv [atmn 'syntax.r5rs.macro] 'let-syntax)
(bind/sv [atmn 'syntax.r5rs.macro] 'letrec-syntax)

(define-template-module [atmn syntax.r5rs.macro-no-sideeffect] [atmn root])
(bind/sv [atmn 'syntax.r5rs.macro-no-sideeffect] 'let-syntax)
(bind/sv [atmn 'syntax.r5rs.macro-no-sideeffect] 'letrec-syntax)

;; NB: defineはinternal defineがある為、特殊な扱いになる事に注意
(define-template-module [atmn syntax.r5rs.sideeffect] [atmn root])
(bind/sv [atmn 'syntax.r5rs.sideeffect] 'define)
(bind/sv [atmn 'syntax.r5rs.sideeffect] 'set!)

(define-template-module [atmn syntax.r5rs.let] [atmn root])
(bind/sv [atmn 'syntax.r5rs.let] 'let)
(bind/sv [atmn 'syntax.r5rs.let] 'let*)
(bind/sv [atmn 'syntax.r5rs.let] 'letrec)

(define-template-module [atmn syntax.r5rs.cond] [atmn root])
(bind/sv [atmn 'syntax.r5rs.cond] 'if)
(bind/sv [atmn 'syntax.r5rs.cond] 'cond)
(bind/sv [atmn 'syntax.r5rs.cond] 'case)
(bind/sv [atmn 'syntax.r5rs.cond] 'and)
(bind/sv [atmn 'syntax.r5rs.cond] 'or)

(define-template-module [atmn syntax.r5rs.lambda] [atmn root])
(bind/sv [atmn 'syntax.r5rs.lambda] 'lambda)
(bind/sv [atmn 'syntax.r5rs.lambda] 'begin)
(bind/sv [atmn 'syntax.r5rs.lambda] 'delay)

(define-template-module [atmn syntax.r5rs.do] [atmn root])
(bind/sv [atmn 'syntax.r5rs.do] 'do)


;; まとめたモジュールを何個か用意する
(define-template-module [atmn syntax.r5rs]
  [atmn syntax.r5rs.quote]
  [atmn syntax.r5rs.macro]
  [atmn syntax.r5rs.sideeffect]
  [atmn syntax.r5rs.let]
  [atmn syntax.r5rs.cond]
  [atmn syntax.r5rs.lambda]
  [atmn syntax.r5rs.do]
  [atmn root])
(define-template-module [atmn syntax.r5rs-safe]
  [atmn syntax.r5rs.quote]
  [atmn syntax.r5rs.macro-no-sideeffect]
  [atmn syntax.r5rs.let]
  [atmn syntax.r5rs.cond]
  [atmn syntax.r5rs.lambda]
  [atmn syntax.r5rs.do]
  [atmn root])

;; syntax.gauche.*を定義する
(define-template-module [atmn syntax.gauche.macro]
  [atmn syntax.r5rs.macro] [atmn root])
(bind/sv [atmn 'syntax.gauche.macro] 'define-macro)
(bind/sv [atmn 'syntax.gauche.macro] '%macroexpand)
(bind/sv [atmn 'syntax.gauche.macro] '%macroexpand-1)

(define-template-module [atmn syntax.gauche.macro-no-sideeffect]
  [atmn syntax.r5rs.macro-no-sideeffect] [atmn root])
(bind/sv [atmn 'syntax.gauche.macro-no-sideeffect] '%macroexpand)
(bind/sv [atmn 'syntax.gauche.macro-no-sideeffect] '%macroexpand-1)

(define-template-module [atmn syntax.gauche.sideeffect]
  [atmn syntax.r5rs.sideeffect] [atmn root])
(bind/sv [atmn 'syntax.gauche.sideeffect] 'define-generic)
(bind/sv [atmn 'syntax.gauche.sideeffect] 'define-constant)
(bind/sv [atmn 'syntax.gauche.sideeffect] 'define-method)
(bind/sv [atmn 'syntax.gauche.sideeffect] 'define-inline)
(bind/sv [atmn 'syntax.gauche.sideeffect] 'define-class)

(define-template-module [atmn syntax.gauche.let]
  [atmn syntax.r5rs.let] [atmn root])
(bind/sv [atmn 'syntax.gauche.let] 'receive)

(define-template-module [atmn syntax.gauche.cond]
  [atmn syntax.r5rs.cond] [atmn root])
(bind/sv [atmn 'syntax.gauche.cond] 'and-let*)
(bind/sv [atmn 'syntax.gauche.cond] 'when)
(bind/sv [atmn 'syntax.gauche.cond] 'unless)

(define-template-module [atmn syntax.gauche.lambda]
  [atmn syntax.r5rs.lambda] [atmn root])
(bind/sv [atmn 'syntax.gauche.lambda] 'lazy)

(define-template-module [atmn syntax.gauche.module] [atmn root])
;; ユーザに提供する事は無い筈
(bind/sv [atmn 'syntax.gauche.module] 'define-module)
(bind/sv [atmn 'syntax.gauche.module] 'current-module)
(bind/sv [atmn 'syntax.gauche.module] 'select-module)
(bind/sv [atmn 'syntax.gauche.module] 'with-module)
(bind/sv [atmn 'syntax.gauche.module] 'import)
(bind/sv [atmn 'syntax.gauche.module] 'export)
(bind/sv [atmn 'syntax.gauche.module] 'export-all)
(bind/sv [atmn 'syntax.gauche.module] 'extend)
(bind/sv [atmn 'syntax.gauche.module] 'require)
(bind/sv [atmn 'syntax.gauche.module] 'define-in-module)

(define-template-module [atmn syntax.gauche.internal]
  [atmn root])
;; ユーザに提供する事は無い筈
(bind/sv [atmn 'syntax.gauche.internal] 'eval-when)

;; まとめたモジュールを何個か用意する
(define-template-module [atmn syntax.gauche]
  [atmn syntax.gauche.macro]
  [atmn syntax.gauche.sideeffect]
  [atmn syntax.gauche.let]
  [atmn syntax.gauche.cond]
  [atmn syntax.gauche.lambda]
  [atmn syntax.r5rs.quote]
  [atmn syntax.r5rs.do]
  [atmn root])
(define-template-module [atmn syntax.gauche-safe]
  [atmn syntax.gauche.macro-no-sideeffect]
  [atmn syntax.gauche.let]
  [atmn syntax.gauche.cond]
  [atmn syntax.gauche.lambda]
  [atmn syntax.r5rs.quote]
  [atmn syntax.r5rs.do]
  [atmn root])
(define-template-module [atmn syntax.gauche-all-unsafe]
  [atmn syntax.gauche.macro]
  [atmn syntax.gauche.sideeffect]
  [atmn syntax.gauche.let]
  [atmn syntax.gauche.cond]
  [atmn syntax.gauche.lambda]
  [atmn syntax.gauche.module]
  [atmn syntax.gauche.internal]
  [atmn syntax.r5rs.quote]
  [atmn syntax.r5rs.do]
  [atmn root])

;;; TODO: 現状では、↑にはマクロが含まれてない。
;;;       マクロもsyntaxと同じ扱いになるべきなので、
;;;       ここで含めておく事。
;;; とりあえず、以下を分類に分けてから含めればok。
;;; '(use with-signal-handlers while let1 until export-if-defined guard cond-expand use-version unwind-protect add-load-path rxmatch-let cute syntax-error rxmatch-if syntax-errorf rxmatch-cond push! rxmatch-case require-extension pop! inc! dec! rec define-condition-type update! %guard-rec $ condition check-arg $* get-optional let-optionals* get-keyword* let-keywords* case-lambda let-keywords debug-print let/cc time program begin0 fluid-let define-values values-ref autoload cut set!-values dotimes dolist)

;;; ----
;;; 手続き用テンプレートモジュール

;; TODO: 今のところ結局、内部でimport/svを使ってしまっているので、
;;       この手続きの名前はnosyntaxesじゃなくてよい
;;       (syntaxを渡しても良い為)
;;       あとで名前を考え直す
;; bind/sv系では、syntaxを実体で渡していたので
;; source-module-symbolは不要だったが、
;; ここでは、symbolによって渡されるので、symbolを参照するモジュールも
;; 一緒に指定する必要がある。
(define (bind-nosyntaxes module-symbol bind-symbols source-module-symbol)
  (let ((source-module (find-module source-module-symbol))
        (target-module (find-module module-symbol))
        )
    (for-each
      (lambda (symbol)
        (import/sv target-module
                   symbol
                   (global-variable-ref source-module symbol)))
      bind-symbols)))

;; Gaucheのschemeモジュールの内、ファイル関連の束縛を集めたモジュール
;; (import非提供)
(define *binds:r5rs:file*
  '(
    *dynamic-load-path*
    *load-path*
    *load-suffixes*
    call-with-input-file
    call-with-output-file
    load
    load-from-port
    open-input-file
    open-output-file
    with-input-from-file
    with-output-to-file
    ))
(define-template-module [atmn proc.r5rs.file] [atmn root])
(bind-nosyntaxes [atmn 'proc.r5rs.file] *binds:r5rs:file* 'scheme)


;; Gaucheのschemeモジュールの内、数値関連の束縛を集めたモジュール
;; note: %で始まる束縛は最初、提供しない方針だったが、
;;       Gaucheの拡張モジュールに含まれるマクロが
;;       これらを必要とするような気がしてきたので、
;;       結局含める事にした。
(define *binds:r5rs:number*
  '(
    %acos
    %asin
    %atan
    %complex->real/imag
    %cos
    %cosh
    %exp
    %expt
    %gcd
    %log
    %sin
    %sinh
    %sqrt
    %tan
    %tanh
    *
    +
    -
    /
    <
    <=
    =
    >
    >=
    abs
    acos
    angle
    asin
    atan
    ceiling
    complex?
    cos
    denominator
    even?
    exact->inexact
    exact?
    exp
    expt
    floor
    gcd
    imag-part
    inexact->exact
    inexact?
    integer->char
    integer?
    lcm
    log
    magnitude
    make-polar
    make-rectangular
    max
    min
    modulo
    nearly=?
    negative?
    number->string
    number?
    numerator
    odd?
    positive?
    quotient
    rational?
    real-part
    real?
    remainder
    round
    sin
    sqrt
    tan
    truncate
    zero?
    ))
(define-template-module [atmn proc.r5rs.number] [atmn root])
(bind-nosyntaxes [atmn 'proc.r5rs.number] *binds:r5rs:number* 'scheme)


;; リスト
(define *binds:r5rs:list*
  '(
    append
    assoc
    assq
    assv
    caaaar
    caaadr
    caaar
    caadar
    caaddr
    caadr
    caar
    cadaar
    cadadr
    cadar
    caddar
    cadddr
    caddr
    cadr
    car
    cdaaar
    cdaadr
    cdaar
    cdadar
    cdaddr
    cdadr
    cdar
    cddaar
    cddadr
    cddar
    cdddar
    cddddr
    cdddr
    cddr
    cdr
    cons
    length
    length+
    list
    list->string
    list->vector
    list-ref
    list-tail
    list?
    map
    member
    memq
    memv
    null?
    pair?
    reverse
    set-car!
    set-cdr!
    ))
(define-template-module [atmn proc.r5rs.list] [atmn root])
(bind-nosyntaxes [atmn 'proc.r5rs.list] *binds:r5rs:list* 'scheme)
;;;; TODO:
;; assoc / assq / assv / length / list->string / list->vector
;; / map / member / memq / memv / reverse を、置き換える必要がある
;; (引数が無限listでない事を保証する必要がある)
;; TODO: 実は無限listを渡しても安全かも知れないので、確認する
;; - 事前に引数が無限listかどうかをチェックし、
;;   無限listであればerror例外を投げるようにすればok
;; TODO: あとで実装する
;; TODO: set-car! set-cdr! は別扱いにすべきかも知れない


;; 手続き/promise/eval
(define *binds:r5rs:proc*
  '(
    apply
    call-with-values
    for-each
    procedure?
    values
    force
    eval
    ))
(define-template-module [atmn proc.r5rs.proc] [atmn root])
(bind-nosyntaxes [atmn 'proc.r5rs.proc] *binds:r5rs:proc* 'scheme)
;; note: lazyはr5rs外なので、別に提供する
;; apply / for-each が、無限listを渡しても大丈夫な事を確認する必要がある
;; (もし駄目なら、対策を入れる必要がある)
;; evalの扱いを決める必要がある
;; - evalを呼ぶ前に監視proc実行、及び、evalを実行するモジュール空間を
;;   自分自身に限定する、という条件でなら、特別扱いする必要は無い筈。
;; また、再帰的に畳み込まれるeval/svを提供するのが好ましい
;; (しかし、それは、*binds:r5rs:proc*外で提供する事)
;; TODO: あとで実装する


;; 真偽値/同値比較/シンボル
(define *binds:r5rs:prime*
  '(
    boolean?
    eq?
    equal?
    eqv?
    not
    symbol->string
    symbol?
    ))
(define-template-module [atmn proc.r5rs.prime] [atmn root])
(bind-nosyntaxes [atmn 'proc.r5rs.prime] *binds:r5rs:prime* 'scheme)
;; TODO: もう少しモジュールの分け方を考え直した方がいいかも


;; 継続
(define *binds:r5rs:cont*
  '(
    call-with-current-continuation
    call/cc
    dynamic-wind
    ))
(define-template-module [atmn proc.r5rs.cont] [atmn root])
(bind-nosyntaxes [atmn 'proc.r5rs.cont] *binds:r5rs:cont* 'scheme)
;; call-with-current-continuation / call/cc は、
;; 継続を呼ぶ毎に監視procを呼ぶようにした方が良い
;; (無限ループ対策)
;; TODO: あとで実装する


;; 文字/文字列
(define *binds:r5rs:str*
  '(
    char->integer
    char-alphabetic?
    char-ci<=?
    char-ci<?
    char-ci=?
    char-ci>=?
    char-ci>?
    char-downcase
    char-lower-case?
    char-numeric?
    char-upcase
    char-upper-case?
    char-whitespace?
    char<=?
    char<?
    char=?
    char>=?
    char>?
    char?
    make-string
    string
    string->list
    string->number
    string->symbol
    string-append
    string-ci<=?
    string-ci<?
    string-ci=?
    string-ci>=?
    string-ci>?
    string-copy
    string-length
    string-ref
    string-set!
    string<=?
    string<?
    string=?
    string>=?
    string>?
    string?
    substring
    ))
(define-template-module [atmn proc.r5rs.str] [atmn root])
(bind-nosyntaxes [atmn 'proc.r5rs.str] *binds:r5rs:str* 'scheme)
;; TODO: string-set!は別枠にすべきかも


;; port
(define *binds:r5rs:port*
  '(
    char-ready?
    close-input-port
    close-output-port
    current-input-port
    current-output-port
    display
    eof-object?
    input-port?
    newline
    output-port?
    peek-char
    port?
    read
    read-char
    write
    write-char
    ))
(define-template-module [atmn proc.r5rs.port] [atmn root])
(bind-nosyntaxes [atmn 'proc.r5rs.port] *binds:r5rs:port* 'scheme)
;; display / write は、再帰構造が無い事を保証する必要がある。
;; (或いは、write/ssベースにするか)
;; close-input-port / close-output-port / display / newline / peek-char
;; read / read-char / write / write-char は、portがブロックする可能性がある。
;; しかし、それを判定/回避する事は出来ないので、
;; ブロックするようなportは持ち込まない事、という約束で回避する。
;; TODO: あとで実装する


;; eval用env(ダミー(自分自身のenvを返す)に差し換えられる)
(define *binds:r5rs:env*
  '(
    interaction-environment
    null-environment
    scheme-report-environment
    ))
(define-template-module [atmn proc.r5rs.env] [atmn root])
(bind-nosyntaxes [atmn 'proc.r5rs.env] *binds:r5rs:env* 'scheme)
;; TODO: あとで自分自身のenvを返すものにさしかえる事

;; vector
(define *binds:r5rs:vector*
  '(
    make-vector
    vector
    vector->list
    vector-fill!
    vector-length
    vector-ref
    vector-set!
    vector?
    ))
(define-template-module [atmn proc.r5rs.vector] [atmn root])
(bind-nosyntaxes [atmn 'proc.r5rs.vector] *binds:r5rs:vector* 'scheme)
;; TODO: vector-fill! vector-set! は、別枠にすべきかも

;; まとめたモジュールを何個か用意する

;; r5rs('schemeモジュール)が提供する束縛のデバッグ要モジュール
;; これはデバッグ用。危険なproc.r5rs.fileを含んでいるので、
;; 通常は使うべきではない。
(define-template-module [atmn r5rs-all-debug]
  [atmn syntax.r5rs]
  [atmn proc.r5rs.file]
  [atmn proc.r5rs.number]
  [atmn proc.r5rs.list]
  [atmn proc.r5rs.proc]
  [atmn proc.r5rs.prime]
  [atmn proc.r5rs.cont]
  [atmn proc.r5rs.str]
  [atmn proc.r5rs.port]
  [atmn proc.r5rs.env]
  [atmn proc.r5rs.vector]
  [atmn root])

;; 通常使用用
(define-template-module [atmn r5rs]
  [atmn syntax.r5rs]
  [atmn proc.r5rs.number]
  [atmn proc.r5rs.list]
  [atmn proc.r5rs.proc]
  [atmn proc.r5rs.prime]
  [atmn proc.r5rs.cont]
  [atmn proc.r5rs.str]
  [atmn proc.r5rs.port]
  [atmn proc.r5rs.env]
  [atmn proc.r5rs.vector]
  [atmn root])

;; define系抜き
;; TODO: set-car!等、一部副作用有りのものが残っている
;;       また、継続も残っている
;;       (set!が無いので、set-car!等が正常に機能しない可能性が若干ある)
(define-template-module [atmn r5rs-safe]
  [atmn syntax.r5rs-safe]
  [atmn proc.r5rs.number]
  [atmn proc.r5rs.list]
  [atmn proc.r5rs.proc]
  [atmn proc.r5rs.prime]
  [atmn proc.r5rs.cont]
  [atmn proc.r5rs.str]
  [atmn proc.r5rs.port]
  [atmn proc.r5rs.env]
  [atmn proc.r5rs.vector]
  [atmn root])



;; 'gauche環境が提供する束縛各種
;; これらはGaucheの'gaucheモジュールから取得しているので、
;; 将来にGauche本体がバージョンアップした際に増える可能性がある。
;; なので、これで全てかどうかを簡単に確認する為の手続きを用意しておくと
;; メンテナンスが楽になる。

(define *binds:r5rs+:lazy*
  '(
    lazy
    ))
(define-template-module [atmn proc.r5rs+.lazy] [atmn root])
(bind-nosyntaxes [atmn 'proc.r5rs+.lazy] *binds:r5rs+:lazy* 'gauche)
;; TODO: force時に無限ループになるパターンが無い事を確認する事

(define *binds:r5rs+:flush*
  '(
    flush
    ))
(define-template-module [atmn proc.r5rs+.flush] [atmn root])
(bind-nosyntaxes [atmn 'proc.r5rs+.flush] *binds:r5rs+:flush* 'gauche)

;;; TODO: R5RS外だが、Gaucheの提供している、重要な機能をr5rs+として定義していく

;;;; TODO: これ以上の定義は、もっと完成してからにする

;; とりあえず、ダミーのモジュールを用意しておく
(define-template-module [atmn r5rs+] [atmn r5rs])




;;; ----


;; syntaxの一覧を取得する方法
#|
(use util.list)
(use srfi-1)
(define (syntax-list module-symbol)
  (let1 m (find-module module-symbol)
    (filter
      (lambda (symbol)
        (is-a? (eval symbol m) <syntax>))
      (map car (hash-table->alist (module-table m))))))
(syntax-list 'null) ; => r5rsのsyntax一覧
(syntax-list 'gauche) ; => Gaucheのsyntax一覧
|#
;; Gaucheでは、現在はマクロとして実装されているspecial formが
;; 将来にsyntaxとして実装される可能性がある。
;; (過去では、and-let*がマクロからsyntaxに昇格した)
;; バージョンアップ時に、マクロからsyntaxに昇格したspecial formがあると、
;; そのsyntaxが安全でなくなる可能性がある為、
;; test caseで、eval-sv側を書いた時に存在を確認されているsyntax一覧と、
;; インストールされているGaucheでのsyntax一覧を比較して、
;; もし違いがある場合は、エラーを出した方がいいかも知れない。
;; Gauche-0.8.13では、以下の結果が得られた。
#|
;; 'scheme
'(cond letrec-syntax letrec set! unquote-splicing quote case do and let begin define-syntax lambda define quasiquote delay or let* let-syntax if unquote)

;; 'gauche
'(define-in-module export define-generic define-constant import current-module select-module define-method with-module define-module define-inline receive define-class and-let* eval-when unless export-all %macroexpand-1 when %macroexpand extend require lazy define-macro)
|#


;; 尚、require等、Gauche標準ではsyntaxだが(use slib)した際に
;; 別束縛によって隠されてしまうものが存在する事にも注意。

;; note: マクロのlist
#|
;; 'scheme
'() ; 無し
;; 'gauche
'(use with-signal-handlers while let1 until export-if-defined guard cond-expand use-version unwind-protect add-load-path rxmatch-let cute syntax-error rxmatch-if syntax-errorf rxmatch-cond push! rxmatch-case require-extension pop! inc! dec! rec define-condition-type update! %guard-rec $ condition check-arg $* get-optional let-optionals* get-keyword* let-keywords* case-lambda let-keywords debug-print let/cc time program begin0 fluid-let define-values values-ref autoload cut set!-values dotimes dolist)
|#


;;; syntaxを扱う方針
;;;
;;; * 前提
;;; - syntaxは、以下のモジュールにしか含まれているもので全てとする
;;;   (但し、現在はこれが成り立っているのを確認したが、将来も成り立つかは不定)
;;; -- (find-module 'null) R5RS内のsyntax
;;; -- (find-module 'gauche) Gauche内のsyntax
;;;
;;; * 実際の取り扱いについて
;;; - まず、前述のnullモジュールに含まれる束縛と、
;;;   gaucheモジュールに含まれる束縛の内(eq? <syntax> entity)が成り立つものを
;;;   抽出しておく。
;;;   これが、扱うべきsyntaxの全てとなる。
;;; - この全てのsyntaxを、テンプレートモジュールのrootに、
;;;   隠し束縛として束縛しておく。
;;; - 実際に提供するsyntaxは、隠し束縛に展開されるマクロとして提供する。
;;;   (このマクロは、展開すると:sv-proc呼び出しと隠しsyntax実行になる)
;;; - do等、更にカスタマイズする必要のあるもののカスタマイズも可能なように
;;;   しておく必要もある！
;;;
;;; * 危険なsyntax一覧
;;; 信用できないユーザに何も考えずに提供するのは危険なものをリストアップする。
;;; - define define-syntax set!
;;;   define-in-module define-generic define-constant define-method
;;;   define-module define-inline define-class define-macro
;;; -- メモリ過使用DoSの可能性有り
;;; -- メモリ過使用DoSを防ぐ為には、以下の方法がある。
;;; --- これらのsyntaxを提供しない。
;;; --- envを再利用しなければ、モジュールごとGCされるので、
;;;     毎回make-eval/svして新しい環境を生成する(重い)。
;;; --- 信用できるユーザにのみ使わせる。
;;; - do %macroexpand
;;; -- 無限loopの可能性有り
;;; -- doはmacroでくるんで、各部分の処理一回毎に
;;;    :sv-procを呼ぶようにすればよい。
;;; -- %macroexpandは、%macroexpand-1を使って、
;;;    :sv-procを呼ぶ代用物を用意すればよい。
;;; - eval-when require
;;; -- eval-whenはevalと同じく、
;;;    eval時にマクロ展開が発生し、無限loopになる可能性有り
;;; -- requireは外部ファイルを実行できてしまう
;;; -- どちらもユーザに提供すべきではない


;;; note: Gaucheのevalの挙動について
;;; - (eval '(if 1 2 3) (interaction-environment)) => special formは正常に機能
;;; - (eval '(use util.list) (interaction-environment)) => macroは正常に機能
;;; - (eval '(+ 1 2) (interaction-environment)) => 手続きは正常に機能
;;; - (eval '"aaa" (interaction-environment)) => quoteされた即値は正常に機能
;;; - (eval `(,if 1 2 3) (interaction-environment))
;;;   => 実体を渡されたspecial formは評価できない
;;; - (eval `(,use util.list) (interaction-environment)) =>
;;;   => 実体を渡されたmacroは評価できない
;;; - (eval `(,+ 1 2) (interaction-environment))
;;;   => 実体を渡された手続きは正常に機能
;;; - (eval "aaa" (interaction-environment)) => 即値は正常に機能
;;;
;;; 結論:
;;; - special formとmacroは実体渡しができない。
;;; - 手続きと即値は実体渡しをしてよい。


;; * 無名モジュールに対して、手続き/マクロ/special form/即値、
;;   を束縛する際の処理について
;; 前提条件として、束縛時には
;; bind-in-module-entity(define-in-moduleの手続き版)を使う。
;; - 但し、bind-in-module-entity自体はユーザには提供しない事。
;;   (sv手続きが付与されず、そのままパスして束縛してしまう為)
;; - 手続き
;; -- 監視procによるフィルタをかけ、その結果の手続きを
;;    モジュール内のシンボルに対して束縛する必要がある。
;; -- しかし望むなら、監視proc無しで、直接束縛してもよい。
;;    その際に発生する諸問題については自分で対処する事。
;; - マクロ
;; -- 無加工で束縛してしまってok。
;;    但し、マクロ展開後に出現するシンボル類は、
;;    既にモジュール内に束縛されている必要がある事に注意。
;;    (要は、マクロでは、import前の環境の束縛を参照する事は出来ないという事。)
;;    但し、define-macroによる伝統的なマクロの、展開手続き部分については
;;    import前の環境の束縛を参照できるようだ。
;;    (展開後のS式には含まれないようにする事。)
;; -- マクロに対する監視procは、マクロの展開時(macroexpand系手続き)に
;;    実行される。展開後には監視は行われない。
;;    (展開後のコードに含まれる手続きについては、通常通り監視が行われる)
;; - special form
;; -- すごいややっこしい。
;; -- TODO: あとで書く
;; - 即値(及びlist、シンボル、オブジェクト等)
;; -- 普通に束縛してしまってok。


;; r5rs('schemeモジュール)内で、無限ループが発生しうるものと、その回避策:
;; - マクロ展開が止まらない場合
;; -- define-syntax / let-syntax / letrec-syntax
;; -- マクロ展開中にカウントアップを入れる事が出来れば……
;;    しかし、可能かどうかは謎
;; - ループを生成するもの(ループのbodyにカウンタが進む手続き等が含まれればok)
;; -- do / for-each / eval / call-with-current-continuation / call/cc
;;    letrec / letrec-syntax / define(手続きを束縛する場合)
;; -- これらは、bodyにカウントアップを入れるようにすればok
;; - 無限listに対する操作
;; -- assoc / assq / assv / length / list->string / list->vector
;;    map / member / memq / memv / reverse / apply / display / write
;;    equal?
;;    (lengthはlength+で置き換え可能、applyは特殊)
;;    (手続きによっては、内部で引数が無限listかチェックしているものがあるかも)
;;    (displayとwriteは、port系でもある事に注意)
;; -- lengthに対するlength+のように、事前に無限listかどうかをチェックし、
;;    無限listであればerror例外を投げるようにすればok
;; -- equal?は、比較対象がlistだったり、内部にlistを含む場合に
;;    発散する可能性があるので、util.isomorphのisomorphic?で
;;    代用する。
;; - port系(read/writeがブロックした場合)
;; -- close-input-port / close-output-port / display / newline / peek-char
;;    read / read-char / write / write-char
;;    (displayとwriteは、無限list系でもある事に注意)
;; -- ブロックするようなportを持ち込まなければok。
;;    そうなる可能性があるのは、標準ではcurrent-*-portのみ。

(provide "eval-sv/template")

