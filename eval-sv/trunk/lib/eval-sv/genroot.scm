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


;;; ���Υ⥸�塼��ϡ��ʲ��λ���ǽ����ġ�
;;; - syntax��Ϣ��«���ȼ�³�����󶡤��롣
;;; - �ƥ�ץ졼�ȥ⥸�塼�����������٤Υ桼�ƥ���ƥ��ޥ�����󶡤��롣
;;; - [atmn 'root]�⥸�塼����������롣
;;; -- binds.scm����������ƥ�ץ졼�ȥ⥸�塼��䡢enfold.scm�ϡ�
;;;    ����[atmn 'root]�⥸�塼�뤬��������������Ƥ��ʤ��ȡ���ǽ���ʤ���
;;; --- binds.scm�Υƥ�ץ졼�ȥ⥸�塼��������use�ʳ��ʤΤǡ�
;;;     �ɤ��ˤ���������������Ƥ���ɬ�פ����ꡢ
;;;     ���Υƥ�ץ졼�ȥ⥸�塼��������enfold.scm��ɬ�פȤ����١�
;;;     ��ɡ�[atmn 'root]�������ͥ��ǹԤäƤ���ɬ�פ����롣

;;; TODO: �ơ��֥���ʬ�����̥⥸�塼����ɤ��Ф��٤����⡩


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


;; ����«���ξ�����ݻ�����hash-table
;; key��entity��val��'(original-symbol gensym-symbol)
;; (key��entity�ʤΤϡ�symbol����entity�ؤ��Ѵ����ưפ������դ��񤷤���)
(define *table:specialform-entity->symbols*
  (make-hash-table 'eq?))

;; *table:specialform-entity->symbols*�ؤΥơ��֥���Ͽ��³��
;; (���μ�³���ϱ���«������ܥ���֤�)
;; NB: �⥸�塼��ؤ���Ͽ�ϼ����ǹԤ���
(define (syntax-table-register symbol entity)
  (let1 internal-symbol (gensym (symbol->string symbol))
    ;; �ơ��֥����Ͽ����
    (hash-table-put! *table:specialform-entity->symbols*
                     entity
                     (list symbol internal-symbol))
    ;; �������줿����ܥ���֤�
    internal-symbol))


;; syntax�Υ���ܥ������list�ˤ��Ƽ��
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




;; �ƥ�ץ졼�ȥ⥸�塼��Υ١������������٤Υޥ���
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


;; root�⥸�塼����Ѱդ���
(define-template-module [atmn root])

;; ����syntax��root�˱���import����
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

