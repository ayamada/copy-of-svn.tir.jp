;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; "eval with supervise" generate procedure module
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
;;; - テンプレートモジュールにimportされるべき、
;;;   R5RS及びGauche標準の手続き類

;;; 憶え書き:
;;; ループ(再帰含む)に関する停止問題を回避するには、
;;; - 全ての手続き類は、漏れなく監視実行されるものとする
;;; - ループを生成する手続き類は、ループ内に監視実行をこっそり含めるようにする
;;; のどちらかが必要になるが、現実的に、前者は無理。
;;; - 無監視な手続き類が存在する可能性がある
;;; - ループ内を空/即値のみにできる可能性がある
;;; よって、ループを生成する手続きには監視実行を含めるようにする事。

;;; TODO: 手続きの登録とテンプレートへの定義は
;;;       重複している部分が多いので、可能なら、統合した方が良い

(define-module eval-sv.genproc
  (use srfi-1)
  (use util.list)
  (use util.isomorph)
  (use eval-sv.cf-arity)
  (use eval-sv.common)
  (use eval-sv.genroot)
  ;(use eval-sv.gensyntax)

  (export
    *table:proc->replacement*
    generate-enfolded-proc
    ))
(select-module eval-sv.genproc)

;;; ----

(define (generate-enfolded-proc symbol true-proc)
  (receive (arg-num has-optional?) (solve-arity true-proc)
    ;; sv-procを生成する
    (let1 proc (lambda args
                 (invoke-supervisor 'proc symbol true-proc args))
      ;; arityを偽装する
      (camouflage-arity arg-num has-optional? proc))))

;;; ----

;; syntax(Gauche提供のmacroまたはspecial form)一覧を保持するhash-table。
;; keyはentity、valは代替マクロ(一部は元のentityそのまま)。
(define *table:proc->replacement*
  (make-hash-table 'eq?))

(define (register-proc key-entity . opt-val)
  (let1 val (get-optional opt-val key-entity)
    (hash-table-put! *table:proc->replacement* key-entity val)))

(define (register-proc/sv symbol . opt-entity)
  (let* ((key-entity (eval symbol (current-module)))
         (val-entity (generate-enfolded-proc
                       symbol
                       (get-optional opt-entity key-entity))))
    (register-proc key-entity val-entity)))


;;; ----

;;; R5RS

(define (symbols->register-proc/sv symbols)
  (for-each
    (lambda (symbol)
      (and-let* ((entity (eval symbol (current-module))))
        (when (or
                (is-a? entity <procedure>)
                (is-a? entity <generic>))
          (register-proc/sv symbol entity))))
    symbols))

(define *genproc:r5rs:file*
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
(symbols->register-proc/sv *genproc:r5rs:file*)

(define *genproc:r5rs:number*
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
(symbols->register-proc/sv *genproc:r5rs:number*)

(define *genproc:r5rs:list*
  '(
    append
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
    length+
    list
    list-ref
    list-tail
    list?
    null?
    pair?
    set-car!
    set-cdr!
    ;assoc
    ;assq
    ;assv
    ;length
    ;list->string
    ;list->vector
    ;map
    ;member
    ;memq
    ;memv
    ;reverse
    ))
(symbols->register-proc/sv *genproc:r5rs:list*)
;; 循環リストを評価すると止まらない手続きは対策する必要がある
(define (check-list l)
  (when (circular-list? l)
    (error "circular list detected" l)))
(define (check-tree l)
  (when (pair? l)
    ;; ↓このアルゴリズムでは、別々の枝に同じlistが出現するのを検出できないが、
    ;; ↓その場合は循環はしないので、別に問題はない
    (let search-line ((current-line l) ; 現在調査中のlist
                      (found `(,l)) ; 発見済のlist
                      ;; TODO: foundはhash-tableの方が良い？
                      ;;       あとでwrite/ssの実装を見て決める事
                      )
      (define (check-one l)
        (when (or
                (circular-list? l) ; 循環listそのものを発見
                (and
                  (pair? l) ; list以外はok
                  ;; lがlistなら、既に出現していないか確認を行う
                  (find (cut eq? l <>) found)))
          (error "circular reference detected"))) ; 同じlistを発見した

      (if (null? current-line)
        #t ; 検索完了
        (let ((c-car (car current-line))
              (c-cdr (cdr current-line)))
          ;; carとcdrをそれぞれ単体として、循環していない事を保証する
          (check-one c-car)
          (check-one c-cdr)
          ;; carとcdrが更に子要素を持つなら、再帰検査を行う
          (when (pair? c-car) ; pairなら、再帰調査をする必要がある
            (search-line c-car (cons c-car found)))
          (when (pair? c-cdr)
            (search-line c-cdr (cons c-cdr found))))))))

(register-proc/sv assoc (lambda (a l . opt)
                          (check-list l) (apply assoc a l opt)))
(register-proc/sv assq (lambda (a l) (check-list l) (assq a l)))
(register-proc/sv assv (lambda (a l) (check-list l) (assv a l)))
(register-proc/sv length (lambda (l) (check-list l) (length l)))
(register-proc/sv list->string (lambda (l) (check-list l) (list->string l)))
(register-proc/sv list->vector (lambda (l . opt)
                                 (check-list l) (apply list->vector l opt)))
(register-proc/sv map (lambda (a l . opt)
                        (check-list l) (apply map a l opt)))
(register-proc/sv member (lambda (a l . opt)
                           (check-list l) (apply member a l opt)))
(register-proc/sv memq (lambda (a l) (check-list l) (memq a l)))
(register-proc/sv memv (lambda (a l) (check-list l) (memv a l)))
(register-proc/sv reverse (lambda (l) (check-list l) (reverse l)))

(define *genproc:r5rs:proc*
  '(
    apply
    call-with-values
    ;for-each
    procedure?
    values
    force
    ;eval
    ))
(symbols->register-proc/sv *genproc:r5rs:proc*)
(register-proc/sv for-each (lambda (a l . opt)
                             (check-list l) (apply for-each a l opt)))
(register-proc/sv eval (lambda (expr env)
                         (let1 eval/sv (or (param:eval/sv) fallback-eval/sv)
                           ;; ここでは、eval/svとしての機能は求められないので、
                           ;; envにprocが渡された時はエラーとする
                           (when (or
                                   (is-a? env <procedure>)
                                   (is-a? env <generic>))
                             (error "invalid environment" env))
                           (eval/sv expr env))))
;; evalは、special formやマクロの展開を行うので、そのままevalを実行すると
;; マクロ無限展開等が発生する可能性があるので、eval/svに差し替える必要がある。
;; eval/svは引数無しで呼び出す事で、evalそのものとして機能する。
(define (fallback-eval/sv expr env)
  ;; これが呼ばれるという事は、eval/sv外での呼び出しなので、
  ;; envを取り出す方法が無い。
  (eval expr env))

(define *genproc:r5rs:prime*
  '(
    boolean?
    eq?
    eqv?
    not
    symbol->string
    symbol?
    ;equal?
    ))
(symbols->register-proc/sv *genproc:r5rs:prime*)
(register-proc/sv equal? (lambda (a b)
                           ;; 発散しない事を保証する必要があるが……。
                           ;; isomorphic?では不完全なパターンがある
                           ;; (isomorphic?的には等しくないが、
                           ;;  equal?的には等しく、しかも循環しているパターン)
                           ;; 結局、以下の仕様になった
                           (define (is-circular? x)
                             (guard (e (else #t))
                               (begin (check-tree x) #f)))
                           (or
                             (eqv? a b) ; short cut
                             (if (and (is-circular? a) (is-circular? b))
                               ;; これでは判定が不完全なパターンがあるが、
                               ;; 今はこれで誤魔化す
                               ;; TODO: あとで考え直す事
                               (isomorphic? a b)
                               (equal? a b)))))


;; 継続
(define *genproc:r5rs:cont*
  '(
    ;call-with-current-continuation
    ;call/cc
    dynamic-wind
    ))
(symbols->register-proc/sv *genproc:r5rs:cont*)
;; call-with-current-continuation / call/cc が生成した継続も、
;; lambda同様に、実行時に監視procを呼ぶ
;; (継続は、戻ってこない点以外は手続きと全く同じ)
(define sv:call/cc
  (generate-enfolded-proc
    'call/cc
    (lambda (proc)
      (let/cc cont-true
        (define (wrapped-cont . args)
          (invoke-supervisor
            'proc ; or 'loop
            'continuation
            cont-true
            args))
        (proc wrapped-cont)))))
(register-proc call-with-current-continuation sv:call/cc)
(register-proc call/cc sv:call/cc)

;; 文字/文字列
(define *genproc:r5rs:str*
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
(symbols->register-proc/sv *genproc:r5rs:str*)


;; port
(define *genproc:r5rs:port*
  '(
    char-ready?
    close-input-port
    close-output-port
    current-input-port
    current-output-port
    eof-object?
    input-port?
    newline
    output-port?
    peek-char
    port?
    read
    read-char
    write-char
    ;display
    ;write
    ))
(symbols->register-proc/sv *genproc:r5rs:port*)
(register-proc/sv display (lambda (l . opt)
                            (check-tree l) (apply display l opt)))
(register-proc/sv write write/ss)
;; display / write は、再帰構造が無い事を保証する必要がある。
;; close-input-port / close-output-port / display / newline / peek-char
;; read / read-char / write / write-char は、portがブロックする可能性がある。
;; しかし、それを判定/回避する事は出来ないので、
;; ブロックするようなportは持ち込まない事、という約束で回避する。


;; eval用env(ダミー(自分自身のenvを返す)に差し換えられる)
(define *genproc:r5rs:env*
  '(
    ;interaction-environment
    ;null-environment
    ;scheme-report-environment
    ))
(symbols->register-proc/sv *genproc:r5rs:env*)
(register-proc/sv interaction-environment (lambda ()
                                            (or
                                              (param:env)
                                              (error "out of eval/sv"))))
(register-proc/sv null-environment (lambda (a)
                                     (or
                                       (param:env)
                                       (error "out of eval/sv"))))
(register-proc/sv scheme-report-environment (lambda (a)
                                              (or
                                                (param:env)
                                                (error "out of eval/sv"))))
;; (param:env)が#fの時はeval/sv外なので、通常通りの挙動にしようかとも考えたが、
;; そうするとeval/svの中に素の(interaction-environment)が漏れる場合が
;; ありそうなので、エラー例外を投げる事にする
;; TODO: この仕様は微妙なので、何か良い方法があれば考え直したい

;; vector
(define *genproc:r5rs:vector*
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
(symbols->register-proc/sv *genproc:r5rs:vector*)


;;; Gaucheのproc
;;; 普通に定義していては数が多すぎるので、以下の方針で定義を行う。
;;; - (find-module 'gauche)に含まれる全procを抽出し、全登録する。
;;; -- 但し、既にtableに登録されているものは登録しない(R5RS登録済など)。
;;; - その後、特別な対応が必要なもののみ、追加対応を行う。

;; まず、全登録を行う。
(let1 module (find-module 'gauche)
  (hash-table-for-each
    (module-table module)
    (lambda (symbol val)
      ;; 実体を取り出す
      (let1 entity (eval symbol module)
        (when (and
                ;; proc以外は除外
                (or
                  (is-a? entity <procedure>)
                  (is-a? entity <generic>))
                ;; 既にtableに登録されていたら除外
                (not
                  (hash-table-get *table:proc->replacement* entity #f)))
          ;; 登録する
          (register-proc/sv symbol entity))))))

;;; その後、危険なもののみ上書きする。
;;; しかし、量が多すぎるので、今のところは、重要なもののみ厳選する。
;;; (これは、templateの方でも厳選されている筈なので、上の全束縛については
;;;  今の段階でも残す事にする)

;;; 危険なもの一覧
;;; - とりあえず、srfi-1系は危険
;;; - listを他形式に変換するものも危険
;;; - sort系も危険
;;; - printやformat系も危険

(provide "eval-sv/genproc")

