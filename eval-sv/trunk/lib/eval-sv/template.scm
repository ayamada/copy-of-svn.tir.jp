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

;;; ���Υ⥸�塼��ϡ�eval-sv���Ȥ��٤ΰʲ��Τ�Τ��󶡤��롣
;;; - �ƥ�ץ졼�ȥ⥸�塼�뤽�Τ��

;;; �ƥ�ץ졼�Ȥ�«������٤δ��å롼�� 
;;; - special form�ϡ�����Ū�˥ޥ���Ȥ��Ƽ������롣
;;; -- �ޥ����Ÿ������ȡ�����syntax���ƻ�proc��eval��apply�����и����롣
;;; - �ޥ���ϡ����Τޤ�«�����Ƥ褤��
;;; -- �ޥ�����Ф���ƻ�ϡ�Ÿ�����Τߤΰ١�
;;; - ��³���ϡ�����δƻ���դ���«�����롣
;;; -- �����Ǥ��Τޤ��֤äƤ��ʤ��ʤ��ǽ���Τ����³���ϡ�
;;;    ���줾����Ф��ƥ������ޥ������줿�ƻ��Ĥ��롣
;;; - ¨�ͤ�list���⡢���Τޤ�«�����Ƥ褤��
;;; -- ɸ��Ǥϡ�list������˼�³���������äƤ������̵���äݤ���

;;; �ƥ�ץ졼�ȥ⥸�塼��ηѾ���
;;
;; [atmn root](����syntax��primitive��syntax�����ä��⥸�塼��)
;; �� ��
;; �� [atmn syntax.*]���[lazy�����ɲä�syntax����⥸�塼��]
;; ��                               ��
;; [����¾�Υƥ�ץ졼�ȥ⥸�塼��] ��
;; ��(syntax̵����macro����)        ɬ�פ˱����ơ��ޥ����ä���
;; ��([atmn r5rs.*]��)              ��
;; ��                               ��
;; ([atmn r5rs]�����ƥ�ץ졼�Ƚ���R5RS�⥸�塼��Ƽ�)
;; (�����դ�ľ��οƥ⥸�塼��Ȥ����󶡤���)
;; (����������ʬ����������Τ�������(���󥿡��ե�������̵������))
;; ��
;; [spawn���줿̵̾�⥸�塼��]

;; TODO: �����³���Υ������ޥ����ϡ��̥ե������ʬ���Ƥ�褤

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
    ;; ���ΤȤ���̵��
    ))
(select-module eval-sv.template)



;;; ----

;; import/sv�ϰ����Ȥ��ƥ⥸�塼����Τ��׵᤹��Τǡ�
;; �⥸�塼��̾��«���Ǥ���褦�ʥ桼�ƥ���ƥ���³�����Ѱդ���
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
;;; �ƥ�ץ졼�ȥ⥸�塼������

;;; ----
;;; syntax�ѥƥ�ץ졼�ȥ⥸�塼��

;; [atmn root]��Ѿ�������[atmn syntax.*]�⥸�塼�����������

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

;; NB: define��internal define������١��ü�ʰ����ˤʤ�������
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


;; �ޤȤ᤿�⥸�塼��򲿸Ĥ��Ѱդ���
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

;; syntax.gauche.*���������
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
;; �桼�����󶡤������̵��Ȧ
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
;; �桼�����󶡤������̵��Ȧ
(bind/sv [atmn 'syntax.gauche.internal] 'eval-when)

;; �ޤȤ᤿�⥸�塼��򲿸Ĥ��Ѱդ���
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

;;; TODO: �����Ǥϡ����ˤϥޥ����ޤޤ�Ƥʤ���
;;;       �ޥ����syntax��Ʊ�������ˤʤ�٤��ʤΤǡ�
;;;       �����Ǵޤ�Ƥ�������
;;; �Ȥꤢ�������ʲ���ʬ���ʬ���Ƥ���ޤ���ok��
;;; '(use with-signal-handlers while let1 until export-if-defined guard cond-expand use-version unwind-protect add-load-path rxmatch-let cute syntax-error rxmatch-if syntax-errorf rxmatch-cond push! rxmatch-case require-extension pop! inc! dec! rec define-condition-type update! %guard-rec $ condition check-arg $* get-optional let-optionals* get-keyword* let-keywords* case-lambda let-keywords debug-print let/cc time program begin0 fluid-let define-values values-ref autoload cut set!-values dotimes dolist)

;;; ----
;;; ��³���ѥƥ�ץ졼�ȥ⥸�塼��

;; TODO: ���ΤȤ����ɡ�������import/sv��ȤäƤ��ޤäƤ���Τǡ�
;;       ���μ�³����̾����nosyntaxes����ʤ��Ƥ褤
;;       (syntax���Ϥ��Ƥ��ɤ���)
;;       ���Ȥ�̾����ͤ�ľ��
;; bind/sv�ϤǤϡ�syntax����Τ��Ϥ��Ƥ����Τ�
;; source-module-symbol�����פ��ä�����
;; �����Ǥϡ�symbol�ˤ�ä��Ϥ����Τǡ�symbol�򻲾Ȥ���⥸�塼���
;; ���˻��ꤹ��ɬ�פ����롣
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

;; Gauche��scheme�⥸�塼����⡢�ե������Ϣ��«���򽸤᤿�⥸�塼��
;; (import����)
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


;; Gauche��scheme�⥸�塼����⡢���ʹ�Ϣ��«���򽸤᤿�⥸�塼��
;; note: %�ǻϤޤ�«���Ϻǽ顢�󶡤��ʤ����ˤ��ä�����
;;       Gauche�γ�ĥ�⥸�塼��˴ޤޤ��ޥ���
;;       ������ɬ�פȤ���褦�ʵ������Ƥ����Τǡ�
;;       ��ɴޤ����ˤ�����
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


;; �ꥹ��
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
;; / map / member / memq / memv / reverse ���֤�������ɬ�פ�����
;; (������̵��list�Ǥʤ������ݾڤ���ɬ�פ�����)
;; TODO: �¤�̵��list���Ϥ��Ƥ���������Τ�ʤ��Τǡ���ǧ����
;; - �����˰�����̵��list���ɤ���������å�����
;;   ̵��list�Ǥ����error�㳰���ꤲ��褦�ˤ����ok
;; TODO: ���ȤǼ�������
;; TODO: set-car! set-cdr! ���̰����ˤ��٤������Τ�ʤ�


;; ��³��/promise/eval
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
;; note: lazy��r5rs���ʤΤǡ��̤��󶡤���
;; apply / for-each ����̵��list���Ϥ��Ƥ�����פʻ����ǧ����ɬ�פ�����
;; (�⤷���ܤʤ顢�к��������ɬ�פ�����)
;; eval�ΰ��������ɬ�פ�����
;; - eval��Ƥ����˴ƻ�proc�¹ԡ��ڤӡ�eval��¹Ԥ���⥸�塼����֤�
;;   ��ʬ���Ȥ˸��ꤹ�롢�Ȥ������Ǥʤ顢���̰�������ɬ�פ�̵��Ȧ��
;; �ޤ����Ƶ�Ū�˾��߹��ޤ��eval/sv���󶡤���Τ����ޤ���
;; (������������ϡ�*binds:r5rs:proc*�����󶡤����)
;; TODO: ���ȤǼ�������


;; ������/Ʊ�����/����ܥ�
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
;; TODO: �⤦�����⥸�塼���ʬ������ͤ�ľ����������������


;; ��³
(define *binds:r5rs:cont*
  '(
    call-with-current-continuation
    call/cc
    dynamic-wind
    ))
(define-template-module [atmn proc.r5rs.cont] [atmn root])
(bind-nosyntaxes [atmn 'proc.r5rs.cont] *binds:r5rs:cont* 'scheme)
;; call-with-current-continuation / call/cc �ϡ�
;; ��³��Ƥ���˴ƻ�proc��Ƥ֤褦�ˤ��������ɤ�
;; (̵�¥롼���к�)
;; TODO: ���ȤǼ�������


;; ʸ��/ʸ����
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
;; TODO: string-set!�����Ȥˤ��٤�����


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
;; display / write �ϡ��Ƶ���¤��̵�������ݾڤ���ɬ�פ����롣
;; (�����ϡ�write/ss�١����ˤ��뤫)
;; close-input-port / close-output-port / display / newline / peek-char
;; read / read-char / write / write-char �ϡ�port���֥�å������ǽ�������롣
;; �������������Ƚ��/���򤹤���Ͻ���ʤ��Τǡ�
;; �֥�å�����褦��port�ϻ������ޤʤ������Ȥ�����«�ǲ��򤹤롣
;; TODO: ���ȤǼ�������


;; eval��env(���ߡ�(��ʬ���Ȥ�env���֤�)�˺�����������)
(define *binds:r5rs:env*
  '(
    interaction-environment
    null-environment
    scheme-report-environment
    ))
(define-template-module [atmn proc.r5rs.env] [atmn root])
(bind-nosyntaxes [atmn 'proc.r5rs.env] *binds:r5rs:env* 'scheme)
;; TODO: ���ȤǼ�ʬ���Ȥ�env���֤���Τˤ����������

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
;; TODO: vector-fill! vector-set! �ϡ����Ȥˤ��٤�����

;; �ޤȤ᤿�⥸�塼��򲿸Ĥ��Ѱդ���

;; r5rs('scheme�⥸�塼��)���󶡤���«���ΥǥХå��ץ⥸�塼��
;; ����ϥǥХå��ѡ�����proc.r5rs.file��ޤ�Ǥ���Τǡ�
;; �̾�ϻȤ��٤��ǤϤʤ���
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

;; �̾������
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

;; define��ȴ��
;; TODO: set-car!��������������ͭ��Τ�Τ��ĤäƤ���
;;       �ޤ�����³��ĤäƤ���
;;       (set!��̵���Τǡ�set-car!��������˵�ǽ���ʤ���ǽ�����㴳����)
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



;; 'gauche�Ķ����󶡤���«���Ƽ�
;; ������Gauche��'gauche�⥸�塼�뤫��������Ƥ���Τǡ�
;; �����Gauche���Τ��С�����󥢥åפ����ݤ��������ǽ�������롣
;; �ʤΤǡ���������Ƥ��ɤ������ñ�˳�ǧ����٤μ�³�����Ѱդ��Ƥ�����
;; ���ƥʥ󥹤��ڤˤʤ롣

(define *binds:r5rs+:lazy*
  '(
    lazy
    ))
(define-template-module [atmn proc.r5rs+.lazy] [atmn root])
(bind-nosyntaxes [atmn 'proc.r5rs+.lazy] *binds:r5rs+:lazy* 'gauche)
;; TODO: force����̵�¥롼�פˤʤ�ѥ�����̵�������ǧ�����

(define *binds:r5rs+:flush*
  '(
    flush
    ))
(define-template-module [atmn proc.r5rs+.flush] [atmn root])
(bind-nosyntaxes [atmn 'proc.r5rs+.flush] *binds:r5rs+:flush* 'gauche)

;;; TODO: R5RS��������Gauche���󶡤��Ƥ��롢���פʵ�ǽ��r5rs+�Ȥ���������Ƥ���

;;;; TODO: ����ʾ������ϡ���äȴ������Ƥ���ˤ���

;; �Ȥꤢ���������ߡ��Υ⥸�塼����Ѱդ��Ƥ���
(define-template-module [atmn r5rs+] [atmn r5rs])




;;; ----


;; syntax�ΰ��������������ˡ
#|
(use util.list)
(use srfi-1)
(define (syntax-list module-symbol)
  (let1 m (find-module module-symbol)
    (filter
      (lambda (symbol)
        (is-a? (eval symbol m) <syntax>))
      (map car (hash-table->alist (module-table m))))))
(syntax-list 'null) ; => r5rs��syntax����
(syntax-list 'gauche) ; => Gauche��syntax����
|#
;; Gauche�Ǥϡ����ߤϥޥ���Ȥ��Ƽ�������Ƥ���special form��
;; �����syntax�Ȥ��Ƽ���������ǽ�������롣
;; (���Ǥϡ�and-let*���ޥ�����syntax�˾��ʤ���)
;; �С�����󥢥å׻��ˡ��ޥ�����syntax�˾��ʤ���special form������ȡ�
;; ����syntax�������Ǥʤ��ʤ��ǽ��������١�
;; test case�ǡ�eval-sv¦��񤤤�����¸�ߤ��ǧ����Ƥ���syntax�����ȡ�
;; ���󥹥ȡ��뤵��Ƥ���Gauche�Ǥ�syntax��������Ӥ��ơ�
;; �⤷�㤤��������ϡ����顼��Ф����������������Τ�ʤ���
;; Gauche-0.8.13�Ǥϡ��ʲ��η�̤�����줿��
#|
;; 'scheme
'(cond letrec-syntax letrec set! unquote-splicing quote case do and let begin define-syntax lambda define quasiquote delay or let* let-syntax if unquote)

;; 'gauche
'(define-in-module export define-generic define-constant import current-module select-module define-method with-module define-module define-inline receive define-class and-let* eval-when unless export-all %macroexpand-1 when %macroexpand extend require lazy define-macro)
|#


;; ����require����Gaucheɸ��Ǥ�syntax����(use slib)�����ݤ�
;; ��«���ˤ�äƱ�����Ƥ��ޤ���Τ�¸�ߤ�����ˤ���ա�

;; note: �ޥ����list
#|
;; 'scheme
'() ; ̵��
;; 'gauche
'(use with-signal-handlers while let1 until export-if-defined guard cond-expand use-version unwind-protect add-load-path rxmatch-let cute syntax-error rxmatch-if syntax-errorf rxmatch-cond push! rxmatch-case require-extension pop! inc! dec! rec define-condition-type update! %guard-rec $ condition check-arg $* get-optional let-optionals* get-keyword* let-keywords* case-lambda let-keywords debug-print let/cc time program begin0 fluid-let define-values values-ref autoload cut set!-values dotimes dolist)
|#


;;; syntax�򰷤�����
;;;
;;; * ����
;;; - syntax�ϡ��ʲ��Υ⥸�塼��ˤ����ޤޤ�Ƥ����Τ����ƤȤ���
;;;   (â�������ߤϤ��줬����Ω�äƤ���Τ��ǧ�����������������Ω�Ĥ�������)
;;; -- (find-module 'null) R5RS���syntax
;;; -- (find-module 'gauche) Gauche���syntax
;;;
;;; * �ºݤμ�갷���ˤĤ���
;;; - �ޤ������Ҥ�null�⥸�塼��˴ޤޤ��«���ȡ�
;;;   gauche�⥸�塼��˴ޤޤ��«������(eq? <syntax> entity)������Ω�Ĥ�Τ�
;;;   ��Ф��Ƥ�����
;;;   ���줬�������٤�syntax�����ƤȤʤ롣
;;; - �������Ƥ�syntax�򡢥ƥ�ץ졼�ȥ⥸�塼���root�ˡ�
;;;   ����«���Ȥ���«�����Ƥ�����
;;; - �ºݤ��󶡤���syntax�ϡ�����«����Ÿ�������ޥ���Ȥ����󶡤��롣
;;;   (���Υޥ���ϡ�Ÿ�������:sv-proc�ƤӽФ��ȱ���syntax�¹Ԥˤʤ�)
;;; - do�������˥������ޥ�������ɬ�פΤ����ΤΥ������ޥ������ǽ�ʤ褦��
;;;   ���Ƥ���ɬ�פ⤢�롪
;;;
;;; * ����syntax����
;;; ���ѤǤ��ʤ��桼���˲���ͤ������󶡤���Τϴ��ʤ�Τ�ꥹ�ȥ��åפ��롣
;;; - define define-syntax set!
;;;   define-in-module define-generic define-constant define-method
;;;   define-module define-inline define-class define-macro
;;; -- ��������DoS�β�ǽ��ͭ��
;;; -- ��������DoS���ɤ��٤ˤϡ��ʲ�����ˡ�����롣
;;; --- ������syntax���󶡤��ʤ���
;;; --- env������Ѥ��ʤ���С��⥸�塼�뤴��GC�����Τǡ�
;;;     ���make-eval/sv���ƿ������Ķ�����������(�Ť�)��
;;; --- ���ѤǤ���桼���ˤΤ߻Ȥ碌�롣
;;; - do %macroexpand
;;; -- ̵��loop�β�ǽ��ͭ��
;;; -- do��macro�Ǥ����ǡ�����ʬ�ν���������
;;;    :sv-proc��Ƥ֤褦�ˤ���Ф褤��
;;; -- %macroexpand�ϡ�%macroexpand-1��Ȥäơ�
;;;    :sv-proc��Ƥ�����ʪ���Ѱդ���Ф褤��
;;; - eval-when require
;;; -- eval-when��eval��Ʊ������
;;;    eval���˥ޥ���Ÿ����ȯ������̵��loop�ˤʤ��ǽ��ͭ��
;;; -- require�ϳ����ե������¹ԤǤ��Ƥ��ޤ�
;;; -- �ɤ����桼�����󶡤��٤��ǤϤʤ�


;;; note: Gauche��eval�ε�ư�ˤĤ���
;;; - (eval '(if 1 2 3) (interaction-environment)) => special form������˵�ǽ
;;; - (eval '(use util.list) (interaction-environment)) => macro������˵�ǽ
;;; - (eval '(+ 1 2) (interaction-environment)) => ��³��������˵�ǽ
;;; - (eval '"aaa" (interaction-environment)) => quote���줿¨�ͤ�����˵�ǽ
;;; - (eval `(,if 1 2 3) (interaction-environment))
;;;   => ���Τ��Ϥ��줿special form��ɾ���Ǥ��ʤ�
;;; - (eval `(,use util.list) (interaction-environment)) =>
;;;   => ���Τ��Ϥ��줿macro��ɾ���Ǥ��ʤ�
;;; - (eval `(,+ 1 2) (interaction-environment))
;;;   => ���Τ��Ϥ��줿��³��������˵�ǽ
;;; - (eval "aaa" (interaction-environment)) => ¨�ͤ�����˵�ǽ
;;;
;;; ����:
;;; - special form��macro�ϼ����Ϥ����Ǥ��ʤ���
;;; - ��³����¨�ͤϼ����Ϥ��򤷤Ƥ褤��


;; * ̵̾�⥸�塼����Ф��ơ���³��/�ޥ���/special form/¨�͡�
;;   ��«������ݤν����ˤĤ���
;; ������Ȥ��ơ�«�����ˤ�
;; bind-in-module-entity(define-in-module�μ�³����)��Ȥ���
;; - â����bind-in-module-entity���Τϥ桼���ˤ��󶡤��ʤ�����
;;   (sv��³������Ϳ���줺�����Τޤޥѥ�����«�����Ƥ��ޤ���)
;; - ��³��
;; -- �ƻ�proc�ˤ��ե��륿�򤫤������η�̤μ�³����
;;    �⥸�塼����Υ���ܥ���Ф���«������ɬ�פ����롣
;; -- ������˾��ʤ顢�ƻ�proc̵���ǡ�ľ��«�����Ƥ�褤��
;;    ���κݤ�ȯ�����������ˤĤ��Ƥϼ�ʬ���н褹�����
;; - �ޥ���
;; -- ̵�ù���«�����Ƥ��ޤä�ok��
;;    â�����ޥ���Ÿ����˽и����륷��ܥ���ϡ�
;;    ���˥⥸�塼�����«������Ƥ���ɬ�פ����������ա�
;;    (�פϡ��ޥ���Ǥϡ�import���δĶ���«���򻲾Ȥ�����Ͻ���ʤ��Ȥ�������)
;;    â����define-macro�ˤ������Ū�ʥޥ���Ρ�Ÿ����³����ʬ�ˤĤ��Ƥ�
;;    import���δĶ���«���򻲾ȤǤ���褦����
;;    (Ÿ�����S���ˤϴޤޤ�ʤ��褦�ˤ������)
;; -- �ޥ�����Ф���ƻ�proc�ϡ��ޥ����Ÿ����(macroexpand�ϼ�³��)��
;;    �¹Ԥ���롣Ÿ����ˤϴƻ�ϹԤ��ʤ���
;;    (Ÿ����Υ����ɤ˴ޤޤ���³���ˤĤ��Ƥϡ��̾��̤�ƻ뤬�Ԥ���)
;; - special form
;; -- ���������ä�������
;; -- TODO: ���Ȥǽ�
;; - ¨��(�ڤ�list������ܥ롢���֥���������)
;; -- ���̤�«�����Ƥ��ޤä�ok��


;; r5rs('scheme�⥸�塼��)��ǡ�̵�¥롼�פ�ȯ���������Τȡ����β����:
;; - �ޥ���Ÿ�����ߤޤ�ʤ����
;; -- define-syntax / let-syntax / letrec-syntax
;; -- �ޥ���Ÿ����˥�����ȥ��åפ��������������Сġ�
;;    ����������ǽ���ɤ�������
;; - �롼�פ�����������(�롼�פ�body�˥����󥿤��ʤ��³�������ޤޤ���ok)
;; -- do / for-each / eval / call-with-current-continuation / call/cc
;;    letrec / letrec-syntax / define(��³����«��������)
;; -- �����ϡ�body�˥�����ȥ��åפ������褦�ˤ����ok
;; - ̵��list���Ф������
;; -- assoc / assq / assv / length / list->string / list->vector
;;    map / member / memq / memv / reverse / apply / display / write
;;    equal?
;;    (length��length+���֤�������ǽ��apply���ü�)
;;    (��³���ˤ�äƤϡ������ǰ�����̵��list�������å����Ƥ����Τ����뤫��)
;;    (display��write�ϡ�port�ϤǤ⤢��������)
;; -- length���Ф���length+�Τ褦�ˡ�������̵��list���ɤ���������å�����
;;    ̵��list�Ǥ����error�㳰���ꤲ��褦�ˤ����ok
;; -- equal?�ϡ�����оݤ�list���ä��ꡢ������list��ޤ����
;;    ȯ�������ǽ��������Τǡ�util.isomorph��isomorphic?��
;;    ���Ѥ��롣
;; - port��(read/write���֥�å��������)
;; -- close-input-port / close-output-port / display / newline / peek-char
;;    read / read-char / write / write-char
;;    (display��write�ϡ�̵��list�ϤǤ⤢��������)
;; -- �֥�å�����褦��port��������ޤʤ����ok��
;;    �����ʤ��ǽ��������Τϡ�ɸ��Ǥ�current-*-port�Τߡ�

(provide "eval-sv/template")

