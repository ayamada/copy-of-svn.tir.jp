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

;;; ���Υ⥸�塼��ϡ�eval-sv���Ȥ��٤ΰʲ��Τ�Τ��󶡤��롣
;;; - �ƥ�ץ졼�ȥ⥸�塼���import�����٤���
;;;   R5RS�ڤ�Gaucheɸ��μ�³����

;;; ������:
;;; �롼��(�Ƶ��ޤ�)�˴ؤ�������������򤹤�ˤϡ�
;;; - ���Ƥμ�³����ϡ�ϳ��ʤ��ƻ�¹Ԥ�����ΤȤ���
;;; - �롼�פ����������³����ϡ��롼����˴ƻ�¹Ԥ򤳤ä���ޤ��褦�ˤ���
;;; �Τɤ��餫��ɬ�פˤʤ뤬������Ū�ˡ����Ԥ�̵����
;;; - ̵�ƻ�ʼ�³���ब¸�ߤ����ǽ��������
;;; - �롼������/¨�ͤΤߤˤǤ����ǽ��������
;;; ��äơ��롼�פ����������³���ˤϴƻ�¹Ԥ�ޤ��褦�ˤ������

;;; TODO: ��³������Ͽ�ȥƥ�ץ졼�Ȥؤ������
;;;       ��ʣ���Ƥ�����ʬ��¿���Τǡ���ǽ�ʤ顢���礷�������ɤ�

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
    ;; sv-proc����������
    (let1 proc (lambda args
                 (invoke-supervisor 'proc symbol true-proc args))
      ;; arity��������
      (camouflage-arity arg-num has-optional? proc))))

;;; ----

;; syntax(Gauche�󶡤�macro�ޤ���special form)�������ݻ�����hash-table��
;; key��entity��val�����إޥ���(�����ϸ���entity���Τޤ�)��
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
;; �۴ĥꥹ�Ȥ�ɾ������Ȼߤޤ�ʤ���³�����к�����ɬ�פ�����
(define (check-list l)
  (when (circular-list? l)
    (error "circular list detected" l)))
(define (check-tree l)
  (when (pair? l)
    ;; �����Υ��르�ꥺ��Ǥϡ��̡��λޤ�Ʊ��list���и�����Τ򸡽ФǤ��ʤ�����
    ;; �����ξ��Ͻ۴ĤϤ��ʤ��Τǡ��̤�����Ϥʤ�
    (let search-line ((current-line l) ; ����Ĵ�����list
                      (found `(,l)) ; ȯ���Ѥ�list
                      ;; TODO: found��hash-table�������ɤ���
                      ;;       ���Ȥ�write/ss�μ����򸫤Ʒ����
                      )
      (define (check-one l)
        (when (or
                (circular-list? l) ; �۴�list���Τ�Τ�ȯ��
                (and
                  (pair? l) ; list�ʳ���ok
                  ;; l��list�ʤ顢���˽и����Ƥ��ʤ�����ǧ��Ԥ�
                  (find (cut eq? l <>) found)))
          (error "circular reference detected"))) ; Ʊ��list��ȯ������

      (if (null? current-line)
        #t ; ������λ
        (let ((c-car (car current-line))
              (c-cdr (cdr current-line)))
          ;; car��cdr�򤽤줾��ñ�ΤȤ��ơ��۴Ĥ��Ƥ��ʤ������ݾڤ���
          (check-one c-car)
          (check-one c-cdr)
          ;; car��cdr�����˻����Ǥ���Ĥʤ顢�Ƶ�������Ԥ�
          (when (pair? c-car) ; pair�ʤ顢�Ƶ�Ĵ���򤹤�ɬ�פ�����
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
                           ;; �����Ǥϡ�eval/sv�Ȥ��Ƥε�ǽ�ϵ����ʤ��Τǡ�
                           ;; env��proc���Ϥ��줿���ϥ��顼�Ȥ���
                           (when (or
                                   (is-a? env <procedure>)
                                   (is-a? env <generic>))
                             (error "invalid environment" env))
                           (eval/sv expr env))))
;; eval�ϡ�special form��ޥ����Ÿ����Ԥ��Τǡ����Τޤ�eval��¹Ԥ����
;; �ޥ���̵��Ÿ������ȯ�������ǽ��������Τǡ�eval/sv�˺����ؤ���ɬ�פ����롣
;; eval/sv�ϰ���̵���ǸƤӽФ����ǡ�eval���Τ�ΤȤ��Ƶ�ǽ���롣
(define (fallback-eval/sv expr env)
  ;; ���줬�ƤФ��Ȥ������ϡ�eval/sv���ǤθƤӽФ��ʤΤǡ�
  ;; env����Ф���ˡ��̵����
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
                           ;; ȯ�����ʤ������ݾڤ���ɬ�פ����뤬�ġġ�
                           ;; isomorphic?�Ǥ��Դ����ʥѥ����󤬤���
                           ;; (isomorphic?Ū�ˤ��������ʤ�����
                           ;;  equal?Ū�ˤ���������������۴Ĥ��Ƥ���ѥ�����)
                           ;; ��ɡ��ʲ��λ��ͤˤʤä�
                           (define (is-circular? x)
                             (guard (e (else #t))
                               (begin (check-tree x) #f)))
                           (or
                             (eqv? a b) ; short cut
                             (if (and (is-circular? a) (is-circular? b))
                               ;; ����Ǥ�Ƚ�꤬�Դ����ʥѥ����󤬤��뤬��
                               ;; ���Ϥ���Ǹ��ⲽ��
                               ;; TODO: ���Ȥǹͤ�ľ����
                               (isomorphic? a b)
                               (equal? a b)))))


;; ��³
(define *genproc:r5rs:cont*
  '(
    ;call-with-current-continuation
    ;call/cc
    dynamic-wind
    ))
(symbols->register-proc/sv *genproc:r5rs:cont*)
;; call-with-current-continuation / call/cc ������������³�⡢
;; lambdaƱ�ͤˡ��¹Ի��˴ƻ�proc��Ƥ�
;; (��³�ϡ���äƤ��ʤ����ʳ��ϼ�³��������Ʊ��)
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

;; ʸ��/ʸ����
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
;; display / write �ϡ��Ƶ���¤��̵�������ݾڤ���ɬ�פ����롣
;; close-input-port / close-output-port / display / newline / peek-char
;; read / read-char / write / write-char �ϡ�port���֥�å������ǽ�������롣
;; �������������Ƚ��/���򤹤���Ͻ���ʤ��Τǡ�
;; �֥�å�����褦��port�ϻ������ޤʤ������Ȥ�����«�ǲ��򤹤롣


;; eval��env(���ߡ�(��ʬ���Ȥ�env���֤�)�˺�����������)
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
;; (param:env)��#f�λ���eval/sv���ʤΤǡ��̾��̤�ε�ư�ˤ��褦���Ȥ�ͤ�������
;; ���������eval/sv������Ǥ�(interaction-environment)��ϳ����礬
;; ���ꤽ���ʤΤǡ����顼�㳰���ꤲ����ˤ���
;; TODO: ���λ��ͤ���̯�ʤΤǡ������ɤ���ˡ������йͤ�ľ������

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


;;; Gauche��proc
;;; ���̤�������Ƥ��ƤϿ���¿������Τǡ��ʲ������ˤ������Ԥ���
;;; - (find-module 'gauche)�˴ޤޤ����proc����Ф�������Ͽ���롣
;;; -- â��������table����Ͽ����Ƥ����Τ���Ͽ���ʤ�(R5RS��Ͽ�Ѥʤ�)��
;;; - ���θ塢���̤��б���ɬ�פʤ�ΤΤߡ��ɲ��б���Ԥ���

;; �ޤ�������Ͽ��Ԥ���
(let1 module (find-module 'gauche)
  (hash-table-for-each
    (module-table module)
    (lambda (symbol val)
      ;; ���Τ���Ф�
      (let1 entity (eval symbol module)
        (when (and
                ;; proc�ʳ��Ͻ���
                (or
                  (is-a? entity <procedure>)
                  (is-a? entity <generic>))
                ;; ����table����Ͽ����Ƥ��������
                (not
                  (hash-table-get *table:proc->replacement* entity #f)))
          ;; ��Ͽ����
          (register-proc/sv symbol entity))))))

;;; ���θ塢���ʤ�ΤΤ߾�񤭤��롣
;;; ���������̤�¿������Τǡ����ΤȤ���ϡ����פʤ�ΤΤ߸������롣
;;; (����ϡ�template�����Ǥ⸷������Ƥ���Ȧ�ʤΤǡ������«���ˤĤ��Ƥ�
;;;  �����ʳ��Ǥ�Ĥ����ˤ���)

;;; ���ʤ�ΰ���
;;; - �Ȥꤢ������srfi-1�Ϥϴ�
;;; - list��¾�������Ѵ������Τ��
;;; - sort�Ϥ��
;;; - print��format�Ϥ��

(provide "eval-sv/genproc")

