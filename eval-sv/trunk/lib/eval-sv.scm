;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; "eval with supervise" module
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


;;; ���Υ⥸�塼��ϡ����ƥå״ƻ��դ���ɾ����(�Υ����ͥ졼��)���󶡤��롣

;;; TODO: ����Ū�ʥɥ�����Ȥν���

;;; TODO: import/sv��enfold-entity���ˡ�
;;;       ����enfold�Ѥ��ɤ����Υ����å��򤷤��������������Τ�ʤ�
;;;       (���enfold�к�)
;;;       ���������ɤ���äƼ¸����뤫����̯

;;; ���С������ν��������:
;;; - eval/sv���define-syntax��define-macro��Ȥäƥޥ���������Ǥ��ޤ���
;;; -- ���������������ޥ�������������ϲ�ǽ�Ǥ������ƻ뤬��ǽ���Ƥ��ʤ��١�
;;;    �ޥ���Ÿ������ߤ�������ݾڤ��Ƥ���������


;;; ������Ū:
;;; - eval�μ¹Ի���(̵�¥롼�פ�֥�å��������)������֥ϥޤ������
;;;   �����Ǥ���褦�ˡ��֥�å�������ε������������Ǥ����������ǡ�
;;;   �ϥޤ����꤬�����������ǽ���Τ�����̤ǳ����߼�³����¹Ԥ���
;;;   Ǥ�դ��ʳ��ǡ�eval��³�Ԥ��٤����ɤ�����Ƚ�Ǥ��ǽ�Ȥ��롣
;;; - �ޤ��������߻��ν����򲣼�ꤹ�������ǽ�ʤΤǡ�
;;;   ¾�����Ӥˤ�ž�ѤǤ��뤫���Τ�ʤ���
;;; - ���⥸�塼��Ǥϡ��꥽�����Ȥ��᤮�ˤ��DoS���ɤ����Ͻ���ʤ��Τǡ�
;;;   �⤷ɬ�פʾ������ӡ��꥽�������̤��к���Ԥ�ɬ�פ����롣

;;; ����:
;;; (define eval/sv (make-eval/sv ...))
;;; �ޤ���
;;; (define-values (eval/sv env) (make-eval/sv ...))
;;; �Τ褦�ˡ�eval/sv��³�����������롣
;;; eval/sv�ϡ�
;;; (eval/sv '(+ 1 2) sv-proc)
;;; �Τ褦�ˡ�sv-proc����ꤷ�Ƽ¹Ԥ�����ˤʤ롣
;;; (���λ���eval���δĶ��ϡ�Ʊ�����������줿env����ưŪ�˺��Ѥ���롣)
;;; �ޤ����Τʤ���δĶ�����̵��eval�Τ褦�ˡ�
;;; (eval/sv '(+ 1 2))
;;; �Τ褦�ˤ��Ƥ⼰��ɾ���Ǥ��롣
;;; (���λ���make-eval/sv���Υ�����ɰ���:default-sv-proc������ȡ�
;;;  ���줬���ꤵ�줿��ΤȤ���ɾ������롣)
;;; eval/sv�ˤ��ɾ���Ǥϡ��ʲ��Υ����ߥ󥰤�sv-proc�������߼¹Ԥ���롣
;;; - (����Ū�˳����߽������ꤷ�Ƥ����ΰʳ���)��³���¹Ի�
;;; - (����Ū�˳����߽������ꤷ�Ƥ����ΰʳ���)�ޥ���Ÿ�������
;;; - (����Ū�˳����߽������ꤷ�Ƥ����ΰʳ���)syntax�¹Ի�
;;; - (̵�¥롼�פ����ǽ���Τ���)����syntax/��³���Υ롼����¹԰����
;;; ����sv-proc�ˤϡ�(��³��/macro/syntax)�¹��Ѥξ����Ϥ����Τǡ�
;;; ���̤˼¹Ԥ��Ƥ褤��Ƚ�Ǥ����ʤ顢��ʬ�Ǽ¹Ԥ�Ԥ�ɬ�פ����롣
;;; �⤷���������ѹ����Ƥ���¹Ԥ�������Ƚ�Ǥ����ꡢ
;;; ����ϼ¹Ԥ������ʤ���Ƚ�Ǥ����ʤ顢���Τ褦�ˤ��Ƥ�褤��
;;; �ޤ���sv-proc�ƤӽФ����ˤϡ�eval/sv�ƤӽФ����ذ쵤�����٤�
;;; ��³���Ϥ����Τǡ������Ȥä�eval��λ���Ƥ⤤������
;;; proc��Ƿ�³���������ơ���������å�ư��Τ褦�ʻ����ǽ�Ȥʤ롣
;;;
;;; ����ɾ�����δĶ����֤ϡ�make-eval/sv��������Ǥ��롣
;;; �ǥե���ȤǤϡ�R5RS�١����˾������Ѥ�ä����⥸�塼���Ѿ�������
;;; make-eval/sv�����Ω����̵̾�⥸�塼��Ȥʤ롣
;;; ���ηѾ����⥸�塼��ϥ������ޥ������������ǽ������
;;; �����߼¹Ԥ�Ԥ���褦�ˤ���٤ˡ�����Υ롼��˽���ɬ�פ����롣
;;; �ޤ���make-eval/sv������������env���Ф���import/sv��Ȥ����ǡ�
;;; ���դ��Ǽ�³��/�ޥ���/syntax/�ѿ���«������������롣

;;; ���פ�������:
;;; ���Υ⥸�塼��μ����ˤϡ�Gauche��ͭ��(R5RSɸ��ǤϤʤ�)��
;;; ��eval���Ϥ�S���ˡ�����ܥ�ǤϤʤ�(���˥���ѥ���Ѥ�)���Τ��Ϥ���
;;; ��ǽ��ȤäƤ��롣
;;; �Ȥꤢ����portable�ǤϤʤ��Ȥ���������ա�

;;; �����:
;;; - �ƥ⥸�塼����������դ������
;;; - �¹�®�٤�˾��ʤ���
;;; - method��arity���������ʤ��ʤ�(method�ʳ��μ�³����arity��������Ȧ)
;;; - eval/sv�⤫���³��������Ф��Ƽ¹Ԥ�����硢
;;;   �ƻ�ϹԤ��ʤ���
;;;   �����Ф�����³����ƻ��դ��Ǽ¹Ԥ��������ϡ�
;;;   �ޤ�eval/sv�������Ƽ¹Ԥ���Ф褤��
;;;   (let1 proc (eval/sv '(lambda (a) (+ 1 a)) sv-proc)
;;;     (proc 2) ; ����Ǥ�sv-proc�ϼ¹Ԥ���ʤ�
;;;     (eval/sv `(,proc 2) sv-proc) ; ����ʤ�sv-proc�ϼ¹Ԥ����
;;; - ���μ�³��/�ޥ���/special form��eval/sv��˻�������ݤˤϡ�
;;;   �ʲ���������դ������
;;; -- ��(��³���ξ��)��³����������̵�¥롼������ȯ�����ʤ������ݾڤ������
;;; --- ɸ��Υƥ�ץ졼�ȥ⥸�塼����󶡤���Ƥ����³���ϡ�
;;;     ̵�¥롼������ȯ�����ʤ��褦�ˡ������μ�³���������������Ƥ��롣
;;; -- ��(�ޥ���ξ��)�ޥ���Ÿ����˽и�����symbol��syntax�ξ�硢
;;;      ����syntax��«��������eval/sv�Ķ��ˤ���ɬ�פ����롣
;;;      (syntax�ʳ��Ǥ�eval/sv�Ķ���ˤ���ɬ�פ�̵��)
;;; --- �äˡ��̾���󶡤���Ƥ��롢�����special form/�ޥ���/��³����
;;;     (�����ΰ٤�)�󶡤��ʤ��ä��ݤˡ�Gauche�Ȥ߹��ߤΥޥ���Ǥ��äƤ�
;;;     ���ξ��֤ˤʤ��ǽ�������������ա�
;;; ---- let���󶡤��Ƥ��ʤ��Τ�let1�ޥ����import�������ʤɡ�
;;; ---- �㤨�С�use�ޥ���(use hoge)�ϡ�
;;;      (begin (with-module gauche (require "hoge")) (import hoge))
;;;      ��Ÿ�������Τǡ�begin with-module import����
;;;      �ɤ줫��ĤǤ�礱�Ƥ���Ķ��Ǥ�����˵�ǽ���ʤ���
;;; -- ������������ʳ��ǡ���enfold���줿�׾��֤ˤʤ�褦�ˤ������
;;; --- ��enfold���줿�פȤϡ��������⤦�Ȥ��Ƥ���syntax���³���ˡ�
;;;     �ƻ��³�����Ȥ߹��ޤ�����ؤ��Ƥ��롣
;;; --- enfold����ˤϡ����Τɤ줫��Ԥ��Ф褤��
;;; ---- make-eval/sv����:bind-alist��ͳ�ǥ����Х��«������
;;; ---- importer��³����ͳ�ǥ����Х��«������
;;; ---- ��ʬ���Ȥ�enfold-entity��ƤӽФ���enfold����
;;;      (�����Х��«���������ʤ�����)
;;; --- �⤷��enfold����Ƥ��ʤ���³��/syntax��¸�ߤ����硢
;;;     ���μ�³����ߤ�̵�¥롼������ȯ������
;;;     ̵�¥롼����˴ƻ�ݥ���Ȥ��ޤޤ�Ƥ��ʤ���С�
;;;     ���ϤǤϻߤ���ʤ��ʤ롣
;;;     (�������饷���ʥ���������ǻߤ������̾��̤��ǽ�����ġ�)
;;; -- ����³��/�ޥ���/special form���������ݤˤϡ�ɬ����Ĥ��Ļ����������
;;;    list/vector/hash-table�����������ƻ����������硢
;;;    ���Ȥ�:bind-alist��importer��enfold-entity��ͳ�Ǥ��äƤ⡢
;;;    �ƻ��³�����Ȥ߹��ޤ�ʤ��Τǡ��ȥ�֥�θ����ˤʤ롣
;;; - apply��ǽ��object�����������ϡ�object-apply��import����ɬ�פ����롣
;;; -- ����gauche.parameter��object-apply�ˤ������ˤʤäƤ���

;;; �������ƥ����������:
;;; - ���⥸�塼��Ǥϡ��꥽�������Ѳ�¿�ˤ��DoS����Ū���ɤ����Ͻ���ʤ���
;;; -- �����define���������ѷϽ�����ػߤ����ꡢ
;;;    ���make-eval/sv���ʤ�����clean up������ǡ�
;;;    �꥽�������Ѿ��������Ѥ��ɤ����ϲ�ǽ�����ġġ�


;; * make-eval/sv�ˤĤ���
;; (define-values (eval/sv env)
;;   (make-eval/sv :default-sv-proc #f
;;                 ;; ���ǥե���Ȥδƻ��³�����ǥե�����ͤ�#f��
;;                 ;; �����������ꤻ���ˡ�eval/sv�¹Ի��˻��ꤷ�Ƥ�褤
;;                 ;; ��(��������ʬ����䤹��)
;;                 :isolate-port? bool
;;                 ;; ��current-*-port����ߡ��˺����ؤ���ե饰��
;;                 ;; �ǥե�����ͤ�#t��
;;                 ;; (current-*-port���֥�å��ǥХ������ä���硢
;;                 ;;  �֥�å�����ߤ�ȯ�������ǽ��������١�
;;                 ;;  ����������ݤ���٤ˡ�#t�ˤ��Ƥ����������ɤ���
;;                 ;;  ������current-*-port�ǥ֥�å�����ߤ�
;;                 ;;  ȯ�����ʤ��ݾڤ򤷤Ƥ���ʤ顢#f�ˤ��Ƥ�褤��)
;;                 :parent-module 'module-name
;;                 ;; ��make-eval/sv��extend���롢
;;                 ;; ̵̾�⥸�塼��οƥ⥸�塼��̾��
;;                 ;; ����ܥ�ǻ��ꤹ���(̵̾�⥸�塼���Բ�)��
;;                 ;; �ƥ⥸�塼��ϡ��������ˡ�ǽ��������Ƥ������
;;                 :bind-alist `((symbol ,expr) ...)
;;                 ;; ���ɲ�«����alist��
;;                 ;; ��ưŪ��(import/sv env symbol expr)����롣
;;                 ))
;; (eval/sv expr sv-proc)
;;
;; make-eval/sv�Υ�����ɰ�����:default-sv-proc���ڤӡ�
;; eval/sv�ΰ�����sv-proc�ϡ��ʲ��Τ褦�ʼ�³���Ȥʤ롣
;; ���μ�³���ϡ�eval/sv�¹���ˡ���ҤΥ����ߥ󥰤ǳ����߼¹Ԥ���롣
;; (define (sv-proc type symbol expr args return except) ...)
;; - type�ϡ������߼��̤򼨤�����ܥ�Ȥʤ롣���μ���ˤĤ��Ƥϸ�Ҥ��롣
;; - symbol�ϡ�«���оݤΥ���ܥ�(����ܥ뤬̵������#f)��
;; - expr�ϡ�symbol��«�����줿���Ρ�
;;   ������ư�Ǽ¹Ԥ�����ǡ�ɾ���Υ��ƥåפ��ʹԤ��롣
;;   �ޥ���Ÿ����syntax�ξ��⡢lambda�ˤ���ޤ���Ϥ����Τǡ�
;;   �ä����꤬�ʤ���С�����ͤ�����(apply expr args)���Ƥ��ޤäƤ褤��
;; - args�ϡ�expr��¹Ԥ���ݤΰ�����
;; -- �ޥ���Ÿ������syntax�ξ�硢�����ͤϰ�̣������ʤ���ǽ�����⤤��
;; - return�ϡ�eval/sv��쵤��ȴ����٤η�³���Ϥ���롣
;; -- error�㳰�ˤ��æ�Ф�guard������ǽ��������Τǡ�������Ѱդ�����
;; -- return���Ϥ��������ϡ����Τޤ�eval/sv���֤��ͤˤʤ롣
;; - except�ϡ����顼�㳰��ȯ��������eval/sv��쵤��ȴ����٤η�³���Ϥ���롣
;; -- error�㳰�ˤ��æ�Ф�guard������ǽ��������Τǡ�������Ѱդ�����
;; -- except���Ϥ��������ϡ�(apply error args)����롣
;; -- �פ�(error "hoge" ...)���������ˡ������(except "hoge" ...)������ɤ���
;;
;; type�ξܺ٤ϡ��ʲ����̤�Ȥʤ롣
;; - 'proc�ϡ������ߤ����ꤵ�줿��³����method, promise, cont����
;;   ��³�����ह���Τμ¹Ի��ˤ��γ����ߤ�ȯ���������򼨤���
;; - 'macro�ϡ��ޥ����Ÿ�����ˤ��γ����ߤ�ȯ���������򼨤���
;;   Ÿ�����Ƶ�Ū�˹Ԥ�����ϡ����ΰ������Ф��Ƴ����ߤ�ȯ�����롣
;; - 'syntax�ϡ�special form�μ¹Ի��ˤ��γ����ߤ�ȯ���������򼨤���
;; - 'loop�ϡ�����proc�ڤ�syntax�ǡ��롼�פޤ��ϺƵ����ƤӽФ�������
;;   �����ߤ�ȯ���������򼨤���
;;   (����ϡ��롼��/�Ƶ���ǡ������ߤ�ȯ���������Τ�����̵���ä�����
;;    �롼��/�Ƶ�����ߤ�������ʤ�̵���ʤ�Τ��ɻߤ���٤��Ѱդ��줿��)


(define-module eval-sv
  (use srfi-1)
  (use util.list)
  (use gauche.parameter)
  (use eval-sv.nullport)
  (use eval-sv.common)
  (use eval-sv.enfold)
  (use eval-sv.template)
  (use eval-sv.form) ; expand-form

  (export
    make-eval/sv

    ;; came from eval-sv.common
    atmn ; �ƥ�ץ졼�Ȼ�������ޥ���
    ;; came from eval-sv.enfold
    import/sv ; «���ɲü�³��
    enfold-entity ; �ƻ��ɲü�³��
    ;; enfold-entity�ϡ�eval/sv���ɾ���������˴ƻ뤬�Ԥ���褦�ˡ�
    ;; �оݤ�lambda��ޥ���Ǥ���ߡ���������̤��֤��ͤȤ����֤���
    ;; (â����entity��list��¨�ͤξ��ϲ��⤷�ʤ���)
    ;; import/sv�ϡ�enfold-entity�����Ʊ���ˡ�����⥸�塼����Ф���
    ;; �����Х��«�����������롣
    ))
(select-module eval-sv)


;; import���Ƥ����³����򤽤Τޤ޳�����export����٤����
(define atmn atmn)
(define import/sv import/sv)
(define enfold-entity enfold-entity)


;;; ----
;;; �桼�ƥ���ƥ���³����ʤ�


;; �ǥե���ȤΥƥ�ץ졼�ȿƥ⥸�塼��
;; (�ܺ٤�binds.scm�򻲾�)
(define *default-parent-module-symbol* [atmn 'r5rs])
;; TODO: binds.scm���������롢�����फ�Υƥ�ץ졼�ȥ⥸�塼��(�Υ���ܥ�)��
;;       �桼�������ꤷ�䤹���褦�ˡ�����餷���󶡤���ɬ�פ�����

;; fallback��default-sv-proc
;; (�ƻ�̵����eval�¹Ԥ򥨥ߥ�졼�Ȥ�������)
(define (fallback-sv-proc type symbol expr args return except)
  (apply expr args))


;;; ----
;;; ����

(define (make-eval/sv . keywords)
  (let-keywords keywords ((default-sv-proc #f) ; ���Ҥ���������
                          (isolate-port? #t)
                          ;; current-*-port����ߡ��Τ�Τ˺����ؤ���ե饰��
                          ;; (current-*-port���֥�å��ǥХ������ä���硢
                          ;;  �֥�å�����ߤ�ȯ�����Ƥ��ޤ���ǽ��������١�
                          ;;  ����������ݤ���٤ˡ�#t�ˤ��Ƥ����������ɤ���
                          ;;  ������current-*-port�ǥ֥�å�����ߤ�
                          ;;  ȯ�����ʤ��ݾڤ򤷤Ƥ���ʤ顢#f�ˤ��Ƥ�褤��)
                          (parent-module *default-parent-module-symbol*)
                          ;; ����ܥ�ǻ��ꤹ���(̵̾�⥸�塼���Բ�)��
                          ;; parent-module�˴ޤޤ��«���ϡ�
                          ;; ����importer���̤������ˤʤäƤ����ΤȤ���
                          ;; (importer���̤��Ƥ��ʤ����ϡ�
                          ;;  sv-proc�ˤ��ƻ�¹Ԥ��Ԥ��ʤ��������)
                          (bind-alist '()) ; (symbol expr)�������ɲ�«������
                          ;; bind-alist��parent-module�Ȥϰ㤤��
                          ;; ɬ�פ�«���ˤ�����Ū��importer���̤���
                          )
    ;; parent-module�δʰץ����å�
    (unless (find-module parent-module)
      (errorf "cannot found module ~a" parent-module))
    ;; parent-module��extend����env(̵̾�⥸�塼��)����������
    (let1 env (spawn-module parent-module)
      ;; env��bind-alist��«������
      (for-each
        (lambda (symbol+expr)
          (apply import/sv env symbol+expr))
        bind-alist)
      ;; eval/sv����������
      (let1 eval/sv (%make-eval/sv default-sv-proc env isolate-port?)
        ;; �ͤ��֤�
        (values eval/sv env)))))


;; eval/sv�����������֤���³����
;; - eval/sv�ϡ�����Ҽ¹Բ�ǽ�Ǥ���ɬ�פ����롣
;; -- import/sv��Ȥäơ�eval/sv���eval/sv����������Ȥ�������
;; - eval/sv�ϡ���������ˡ�������sv-proc�����Ǥ��롣
;; -- â���������R5RS��eval�ΰ����Ⱥ��𤷤䤹���Τǡ�
;;    ����������⥸�塼����ä����Τߡ����⤷�ʤ����ͤȤ��롣
;;    (���ꤵ�줿�⥸�塼���ñ��̵�뤵��롣)
;; - �⤷��������sv-proc�����ꤵ�줿��硢����eval/sv��Ǥϡ�
;;   �Ť�sv-proc�ȿ�����sv-proc�Ρ�ξ�����¹Ԥ������ˤʤ롣
;;   (�⤷��eval/sv�Υͥ��Ȥ����ʤʤ顢���Ĥ�sv-proc���¹Ԥ���롣)
;;   (�¹Խ���ϡ��Ƶ����Ф��Թ�塢�Ť���Τ�����˼¹Ԥ���롣)
;; - sv-proc��cont��Ƥ����硢
;;   ����sv-proc�����ꤵ�줿����eval/sv��ȴ������ˤʤ롣
;; -- eval/sv�����ʤˤʤäƤ��ơ��̡���sv-proc�����ꤵ��Ƥ����硢
;;    ��¦��sv-proc���Ϥ����cont��Ƥ֤ȳ�¦��eval/sv�¹Ի�������ꡢ
;;    ��¦��sv-proc���Ϥ����cont��Ƥ֤���¦��eval/sv�¹Ի�������롣
;; note: sv-proc�ڤ�new-sv-proc�ϰ���ϻ�ġ�
;;       ������isv-proc��new-isv-proc�ϰ����ͤĤȤ��롣
;;       isv-proc��ϡ�����ñ��lambda��cont�����񤷤������Τ�Ρ�
(define (%make-eval/sv default-sv-proc env isolate-port?)
  (let1 port-guarder (if isolate-port?
                       isolate-current-port
                       (lambda (thunk) (thunk)))
    ;; ���μ�³����eval/sv�Ȥ����֤�
    (rec (eval/sv expr . opt-sv-proc)
      ;; return��³��except��³�������ꡢ�ºݤ�eval/sv������Ԥ�
      (define (solve-eval/sv return except)
        (let ((nested? (param:isv-proc))
              (new-sv-proc (let1 tmp-sv-proc (get-optional opt-sv-proc #f)
                             (and
                               (not (module? tmp-sv-proc))
                               tmp-sv-proc)))
              )
          (define (sv-proc->isv-proc proc)
            (lambda (type symbol expr args)
              (proc type symbol expr args return except)))
          (define (doit isv-proc)
            ;; ������isv-proc�����ꤹ��
            (parameterize ((param:isv-proc isv-proc)
                           (param:eval/sv eval/sv) ; ��ʬ����
                           (param:env env)
                           )
              ;; macro��syntax��Ÿ����Ԥ�(�����Ǥ�isv-proc�����Ȥ����)
              (let1 true-expr (expand-form expr env)
                ;; �¹Ԥ���
                (eval true-expr env))))
          (if (not nested?)
            (port-guarder
              (lambda ()
                (doit
                  (sv-proc->isv-proc
                    (or
                      new-sv-proc
                      default-sv-proc
                      fallback-sv-proc)))))
            (let1 old-isv-proc (param:isv-proc)
              (doit
                ;; �Ť�sv-proc�ȿ�����sv-proc��merge���Ƽ¹Ԥ���
                (lambda (type symbol expr args)
                  ;; �ޤ�old-isv-proc��old-cont�ȶ��˼¹Ԥ��롣
                  ;; old-isv-proc��(apply expr args)��¹Ԥ���ȡ�
                  ;; ���ˡ�new-sv-proc�����ߤ�return�ȶ��˼¹Ԥ���롣
                  ;; ���θ塢new-sv-proc��(apply expr in-args)��¹Ԥ���
                  ;; ��ä�����Ρ������߸��μ�³�����¹Ԥ���롣
                  (old-isv-proc
                    type
                    symbol
                    (lambda in-args
                      (let1 sv-proc (or
                                      new-sv-proc
                                      default-sv-proc
                                      fallback-sv-proc)
                        (sv-proc type symbol expr in-args return except)))
                    args)))))))

      ;; ����eval/sv�ƤӽФ���ȴ����٤η�³����������solve-eval/sv��¹Ԥ���
      ;; (return ...)��¹Ԥ���ȡ�eval/sv��ȴ�����֤��ͤȤ���...���֤���
      ;; (except ...)��¹Ԥ���ȡ�eval/sv��ȴ����(apply error ...)��¹Ԥ��롣
      (let/cc return
        (receive r (let/cc except
                     (receive rr (solve-eval/sv return except)
                       (apply return rr)))
          (apply error r))))))

;;; ----

(provide "eval-sv")

