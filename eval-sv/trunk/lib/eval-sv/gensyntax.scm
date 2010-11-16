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

;;; ���Υ⥸�塼��Ǥϡ�R5RS�ڤ�Gaucheɸ���special form�ڤӥޥ����
;;; �������Ԥ���
;;; �����������̤ϡ�*table:syntax-entity->replacement*��ˡ�
;;; �ޥ���(�ޤ��ϸ���syntax�Τޤ�)�μ��ΤȤ�����¸����롣

;;; ������:
;;; �롼��(�Ƶ��ޤ�)�˴ؤ�������������򤹤�ˤϡ�
;;; - ���Ƥμ�³����ϡ�ϳ��ʤ��ƻ�¹Ԥ�����ΤȤ���
;;; - �롼�פ����������³����ϡ��롼����˴ƻ�¹Ԥ򤳤ä���ޤ��褦�ˤ���
;;; �Τɤ��餫��ɬ�פˤʤ뤬������Ū�ˡ����Ԥ�̵����
;;; - ̵�ƻ�ʼ�³���ब¸�ߤ����ǽ��������
;;; - �롼������/¨�ͤΤߤˤǤ����ǽ��������
;;; ��äơ��롼�פ����������³���ˤϴƻ�¹Ԥ�ޤ��褦�ˤ������

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


;; syntax(Gauche�󶡤�macro�ޤ���special form)�������ݻ�����hash-table��
;; key��entity��val�����إޥ���(�����ϸ���entity���Τޤ�)��
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
;;; �桼�ƥ���ƥ���³�����Ѱդ���

(define (entity->hidden-symbol entity)
  (guard (e (else (error "syntax-entity not found" entity e)))
    (cadr (hash-table-get *table:specialform-entity->symbols* entity))))
(define e2hs entity->hidden-symbol)

;; syntax-entity�򱣤�«���Υ���ܥ���Ѵ����Ƥ��顢�ޥ���Ǥ����
;; (name-symbol�ϥޥ�����������Ϳ����(ɽ�����)̾���Ȥ��ƤΤ߻Ȥ���)
;; �����special form�Ϥ���ǵ�ǽ���뤬������������Ǥϵ�ǽ���ʤ���Τ⤢��
;; ���Τ褦��special form�ϼ����ǥޥ������뤫��
;; syntax�򤽤Τޤ��󶡤���ɬ�פ�����
(define (specialform->sv-specialform name-symbol syntax-entity)
  (let (
        ;; �ޥ��������ˤ�define-macro��Ȥ����������define�ʤΤǡ�
        ;; ���餫�Υ���ܥ��«������ɬ�פ����롣
        ;; ���������°פ�«�������䤷�����ʤ��Τǡ�
        ;; ���Ū�˻Ȥ��⥸�塼����֤��Ѱդ����������«�����롣
        (tmp-module (spawn-module [atmn 'root]))
        ;; �ޥ���Ÿ����˽и������special form��α���«�������Ƥ���
        (h:quote [e2hs quote])
        (h:quasiquote [e2hs quasiquote])
        ;; NB: Gauche��quasiquote�ϡ�unquote��unquote-splicing��
        ;;     ����ܥ������Ƚ�ꤷ�Ƥ��ꡢ����ʪ�ϸ��Ƥ��ʤ��Τǡ�
        ;;     ����«����Ȥ�ɬ�פϤʤ�(�ȤäƤϤ����ʤ�)
        (h:syntax-symbol [e2hs syntax-entity])
        (h:define-macro [e2hs define-macro])
        (h:lambda [e2hs lambda])
        )
    ;; tmp-module���eval�ˤ����롢���Τ褦��S������������
    ;; (define-macro (����ܥ�̾ . args)
    ;;   `(invoke-supervisor
    ;;      'syntax
    ;;      '����ܥ�̾
    ;;      (lambda ()
    ;;        (�¹Ԥ���syntax�Υ���ܥ� ,@args))
    ;;      '()))
    ;; NB: ����ϡ�args�򤽤�餷���Ϥ���褦�ˤ�����
    ;;     (���ߤ϶��ˤ��ơ�������lambda��������Ǥ��ޤäƤ���)
    (define (generate-sexpr args)
      `(,invoke-supervisor
         (,h:quote syntax)
         (,h:quote ,name-symbol)
         (,h:lambda ()
           (,h:syntax-symbol ,@args))
         (,h:quote ())))
      
    (let1 s `(,h:define-macro (,name-symbol . args)
               (,generate-sexpr args))
      ;; define-macro��¹Ԥ����ޥ������������
      (eval s tmp-module))
    ;; ���������ޥ����⥸�塼�뤫����Ф����֤��ͤȤ����֤�
    ;; (�����«�����ƤȤ��ƻȤ���١������֤�������̵��)
    (global-variable-ref tmp-module name-symbol)))

;;; ----
;;; �ºݤ�������Ƥ���
;;; ����:
;;; - ����Ū�ˤϡ�syntax�ϡ�����Ū�ޥ����lambda��ʬ��Ǥ���(Ȧ)
;;;   ʬ���ϡ�lambda�ˤʤ롣
;;; - ����Ū�ˤϡ�register-syntax/sv�ǰϤ�
;;; - ������loop��ʬ��ޤ��ǽ���Τ����Τϡ�loop��ʬ��
;;;   lambda�����ơ�invoke-supervisor�ˤ�����褦�ˤ���

;; ----
;; R5RS syntaxes

;; quote�ϤϺ��ΤȤ������Τ򤽤Τޤ޻Ȥ碌��
(register-syntax quote)
(register-syntax quasiquote)
(register-syntax unquote)
(register-syntax unquote-splicing)

;; TODO: define�Ϥ�internal define / toplevel defineξ�б��ˤ���ɬ�פ�����
(define-macro (sv:define target . args)
  ;; (define (hoge) ...)�����ʤ顢(define hoge (lambda ...))�������ѷ�����
  (define (transform target args)
    (if (symbol? target)
      (values target (car args)) ; (define a #f)�����ʤ顢args��1�ĸ���
      (transform (car target)
                 (list ; args�ϡ�list��ɬ�פ�����
                   `(lambda ,(cdr target)
                      ,@args)))))

  ;; ����ǡ�(define symbol (lambda ...))�Ǥ�������ݾڤ����
  (receive (symbol entity) (transform target args)
    ;; TODO: ���Ȥǡ�define�γ�¦��sv�������
    `(,[e2hs define] ,symbol ,entity)))
(register-syntax define sv:define) ; ��
(register-syntax define-syntax define-syntax) ; ��

(register-syntax/sv set!)

(register-syntax/sv let-syntax)
(register-syntax/sv letrec-syntax)

;; NB: internal define�к��ϡ�lambdaľ���,@bodies����Ƥ�Τǡ�
;;     ���ΤȤ���������
;; TODO: 'syntax�ƻ���ɲ�
(define-macro (sv:let . args)
  (if (not (symbol? (car args)))
    ;; �̾�let
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
;; TODO: letrec��'loop�ƻ뤬ɬ�פˤʤ�ġ�
(register-syntax/sv letrec)

(register-syntax/sv if)
(register-syntax/sv cond)
(register-syntax/sv case)
(register-syntax/sv and)
(register-syntax/sv or)

;; begin���ü�(toplevel��ݻ�����)�ʤΤǡ��ƻ���դ��ʤ�(��)
(register-syntax begin)

;; TODO: 'syntax�ƻ���ɲ�
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

;; TODO: 'syntax�ƻ���ɲ�
(define-macro (sv:lambda args . bodies)
  ;; Ÿ������ȡ��ʲ��Τ褦�ʼ��ˤʤ�褦�ˤ���Ф褤��
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
           #f ; ����ܥ�̵��
           (,[e2hs lambda] ,args ,@bodies)
           all-args)))))
(register-syntax lambda sv:lambda) ; ��

(register-syntax/sv delay)


;; ----
;; Gauche syntaxes

(register-syntax define-macro define-macro) ; ��
(register-syntax define-generic define-generic) ; ��
(register-syntax define-constant define-constant) ; ��
(register-syntax define-method define-method) ; ��
(register-syntax define-inline define-inline) ; ��
(register-syntax define-class define-class) ; ��

(register-syntax/sv receive)
(register-syntax/sv and-let*)
(register-syntax/sv when)
(register-syntax/sv unless)
(register-syntax/sv %macroexpand-1)
(register-syntax/sv lazy)

;; TODO: Ÿ���롼���к���ɬ��(������%macroexpand-1��ȤäƼ�������)
(register-syntax %macroexpand %macroexpand) ; ��

;; �⥸�塼��ط��ϻȤ�ʤ�Ȧ�ʤΤǡ����Τޤ���Ͽ���Ƥ���(��)
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

;; �����ѤʤΤ��̾�ϻȤ��ʤ�
(register-syntax eval-when)


;; ----
;; Gauche macros
;; �ޥ���ϴ���Ū�ˤ��Τޤ޻Ȥä�����ʤ���
;; (��ˤ�ä�̵�¥롼�פ�ȯ�������Τˤ��н褬ɬ�ס�
;;  �ġĤȻפä������ޥ��������special form����³����Ÿ�������Τǡ�
;;  special form���³����̵�¥롼���к��������ʤ顢
;;  �ä��к���Ԥ�ɬ�פ�̵���ä���)

;; �ޤ��������ʤ�Τ�ޤȤ����Ͽ���Ƥ���
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

    rec ; letrec��Ÿ�������Τǡ����ä����б�������Ƥ�����ok
    let/cc ; call/cc��Ÿ�������
    ;%guard-rec ; �����Ѥäݤ�
    while ; do��Ÿ�������
    until ; do��Ÿ�������
    dotimes ; do��Ÿ�������
    dolist ; for-each��Ÿ�������
    ))


(provide "eval-sv/gensyntax")

