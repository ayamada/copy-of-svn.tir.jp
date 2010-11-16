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

;;; ���Υ⥸�塼��Ǥϡ�eval��Ρ�Gauche�Ǥ�pass1����������
;;; �ե�����Ÿ����ʬ��������ġ�
;;; �פϡ��ޥ����special form(�ʲ���������Ĥ����礷��form�ȸƤ�)��Ÿ�����ơ�
;;; ������form��ʤ���������Ū�Ȥ��롣
;;; â�����̾��pass1�Ȥϰ㤤���ʲ��ε�ư��Ԥ�ʤ��ƤϤʤ�ʤ���
;;; - special formŸ����Ρ�����ν�����Ԥ���ʬ��lambda�ǰϤߡ�
;;;   ����lambda��invoke-supervisor���Ϥ�ɬ�פ�����
;;; - �ޥ���Ÿ��������invoke-supervisor��¹Ԥ���ɬ�פ�����
;;;   (�����special formƱ�͡��ޥ���Ÿ�����ν�����Ԥ���ʬ��lambda�ǰϤߡ�
;;;    ����lambda��invoke-supervisor���Ϥ���)
;;;
;;; ��������eval-sv�Ǥμ����Ǥϡ�special form�ϴ���Ū�˥ޥ�������Ƥ���Τ�
;;; �㳰������ơ��ޥ���Ÿ���Τ߹Ԥ�������̵����
;;; �㳰�ˤʤ�Τϰʲ�����ġ�
;;; - quote��
;;; -- quasiquote���unquote�Ϥ����Ǽ������뤫���ɤ��ˤ�����ɬ�פ����롣
;;;    �ޥ���Ǽ�����ǽ�ʤ顢�ޥ���Ǽ������Ƥ��ޤ��Ф�����
;;; - define��
;;; -- �ޥ���ˤ���åԥ󥰤Ǥϡ�define�Ϥ˰ʲ������꤬ȯ�����롣
;;; --- lambda�ǰϤ��top level�ǤϤʤ��ʤ롣
;;; --- define�Ϥϴ���Ū��letrecư��ʤΤǡ�����礬���������ʤ롣
;;; �Ȥꤢ���������Ǥϡ�quote�Ϥ�ľ���󶡤��Ƥ��ޤ���
;;; define�Ϥ�̤�б��ȤʤäƤ��롣
;;; internal define��top level define���ˤĤ��Ƥϡ�
;;; �����ʳ���Ƚ���Ԥ�ɬ�פ����롣
;;; Ƚ���̤�param:is-toplevel?��ȿ�Ǥ�����ɤ���ΤȤ��롣


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

;; �ޥ����special form��Ÿ����Ԥ�
;; ���ߤΤȤ���ϥ������������
;;
;; - internal define�ϡ�let��������ܥ�define�򸫤�Ÿ�����Ƥ���Τǡ�
;;   ���Τޤ����֤��ɤ���
;;   (���Τ���󤻤ϡ�let���Υޥ�����ʬ�˹Ԥ�)
;; - top level define�ϡ�define�Τߤ����ǥޥ���Ÿ����ԤäƲ�褹�롣
;; -- define�Ϥϻ����ˡ��ޥ���Ÿ������(param:is-toplevel?)�򸫤�
;;    ��ư���Ѥ�������ˤ��Ƥ���
;; -- �����ǡ��ȥåץ�٥���ʬ���Ĥޤꡢexpr��(begin ...)�ˤʤäƤ�����ʬ��
;;    �Ƶ�Ū�˸�������������define�Ϥ�¸�ߤ��Ƥ���ʤ顢
;;    (parameterize ((param:is-toplevel? #t))
;;      (unwrap-syntax (macroexpand child-expr)))
;;    ��Ԥäơ���ư�ǥޥ����Ÿ�����Ƥ�����
;;    ����ϡ�define�Ϥ��ĥȥåץ�٥�˸��ꤵ���Τǡ�unquote�Τ褦��
;;    �����ȯ��������Ÿ�������������롣

(define (expand-form expr env)
  ;; eval/sv�ϡ�(load�ǤϤʤ�)eval���Υ��󥿡��ե������ʤΤǡ�
  ;; expr��ñ���S���ޤ���¨����ˤʤ롣
  ;; ��äơ��̾��loadŪ�ˡ�ʣ����S�����¤�������ϡ�
  ;; ������(begin ...)�ˤ�äưϤޤ��ñ���S���ˤʤäƤ��롢�Ȳ��ꤷ���ɤ���

  ;; �Ȥꤢ�������ˡ��ȥåץ�٥�define���б������Ԥ���
  ;; (�����ʤ顢�ƻ뤷�ʤ������ޥ�����Ĥ���Ÿ������ɬ�פ�����)
  (expand-top-level-define expr env))


(define (expand-top-level-define expr env)
  ;; ���Ǥ�������list�Ǥ�������ݾڤ����³��
  ;; (list�Ǥʤ����Ϥ����ͤ��֤�����������list�ξ��ϥ��顼���ꤲ��)
  (define (assure-proper-list l proc)
    (cond
      ((not (pair? l)) l) ; list�Ǥʤ��ʤ顢���Τޤ��֤�
      ;; �Դ�����list�ʤ饨�顼�ˤ���
      ((not (list? l)) (error "not proper list" l))
      (else
        (proc l))))
  ;; �ȥåץ�٥��°������ʬ�����򸡺�����map����³��
  (define (top-level-map proc expr)
    (assure-proper-list
      expr
      (lambda (expr)
        (let next ((l expr))
          (if (null? l)
            '() ; ��ü��ã����
            (let ((target (car l))
                  (rest (cdr l)))
              (let1 result (proc target)
                (cons result (next rest)))))))))

  (define (convert-node node)
    (assure-proper-list
      node
      (lambda (node)
        ;; car�μ��Τ���Ф�
        (let1 target (if (symbol? (car node))
                       (global-variable-ref env (car node) *not-found*)
                       *not-found*)
          (cond
            ;; ̤�����«���͡��������ѿ����ʤΤǤ��Τޤޤˤ���
            ((eq? *not-found* target) node)
            ;; begin�ʤ顢�Ƶ�Ū�˽�����Ԥ�
            ((eq? begin target)
             (cons
               (car node) ; begin��symbol
               (top-level-map convert-node (cdr node)))) ; �Ƶ�����
            ;; define*�Ϥʤ顢�ޥ����Ÿ������
            ;; TODO: unwrap-syntax�ϥ⥸�塼����θ���ʤ��Τǡ�
            ;;       ���ˤ�äƤϡ�identifier����⥸�塼�����Ф���
            ;;       with-module��������Ԥ�ɬ�פ����뤫���Τ�ʤ�
            ((hash-table-exists? *top-level-define-entity-table* target)
             (eval `(,unwrap-syntax (,macroexpand ,node)) env))
            ;; ����ʳ��ʤ顢���Τޤޤˤ��Ƥ���
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


;;; ���ޥ���μ���Ÿ���ˤĤ���
;;; - '(macro 123)
;;;   �Τ褦��S����ޥ���Ÿ���������ͤ���ȡ�
;;;   (unwrap-syntax (macroexpand '(macro 123)))
;;;   �ǽ����Ǥ�����
;;; - '(list (macro 123) (macro2 456))
;;;   �Τ褦��S����ޥ���Ÿ���������ͤ���ȡ�
;;;   ���Τ��Ĥ�list�Ȥ��Ƹ��ơ��Ƶ�Ū��
;;;   (lambda (elem) (unwrap-syntax (macroexpand elem)))
;;;   �򤫤��Ƥ����Ф褤��
;;; - �Ǥϡ�
;;;   '(quasiquote ((macro1 123) (unquote (macro2 456))))
;;;   �Τ褦��S���ǤϤɤ�������ɤ���
;;;   �����餯��quasiquote�������Ǥ򸫤ƹԤ���unquote��ȯ�������顢
;;;   unquote�λ����Ǥ�Ƶ�Ū��Ÿ������ɬ�פ�����Ȥϻפ����ġġ�
;;;
;;; �ġĤȤ������꤬����Τǡ��ޥ���μ���Ÿ���ϼ¤Ϻ�����ä���
;;; ��������ܤ˲�褹���硢�פ���ˡ��ޥ����Ÿ����
;;; special form�����������ؤ��Ѵ���Ʊ���ˤ��ʤ��Ƥ���ɬ�פ����롣
;;; �Ĥޤꡢ�ޥ���Ÿ�������Ǥʤ���special form�⼫���Ǽ�������ɬ�פ����롪
;;;
;;; �ġĤ�������ѤʤΤǡ�����ϡ��ޥ���Ÿ���ƻ�ˤĤ��Ƥ�����롣
;;; gauche.night��λ��˼������롣


;;; ----

(provide "eval-sv/form")

