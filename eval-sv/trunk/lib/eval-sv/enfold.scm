;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; "eval with supervise" enfold with superviser module
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


(define-module eval-sv.enfold
  (use srfi-1)
  (use util.list)
  (use gauche.parameter)
  (use eval-sv.common)
  (use eval-sv.genroot)
  (use eval-sv.gensyntax)
  (use eval-sv.genproc)

  (export
    import/sv ; ���Τ�enfold-entity���Ƥ���⥸�塼�����global«������
    enfold-entity ; ���Τ�proc/syntax�ʤ顢�¹Ի��ƻ���ɲä�����Τ��֤�
    ))
(select-module eval-sv.enfold)


;;; ----

;; ����⥸�塼����ǡ�symbol���Ф���entity��«�����롣
;; â����entity����³��/special form�����ä����ϡ�
;; Ŭ�ڤ˴ƻ��³�����Ȥ߹��ࡣ
;; ��entity��list��vector��hash-table���ǡ�������proc����ޤ�Ǥ�����硢
;;   �����ˤϴƻ��³�����Ȥ߹��ޤ�ʤ���
;;   �ƻ��³�����Ȥ߹��ߤ������ϰ�Ĥ��Ļ����������
(define (import/sv module-entity symbol entity . opt-through-sv)
  (let-optionals* opt-through-sv ((through-sv #f))
    (bind-in-module-entity
      module-entity
      symbol
      (if through-sv
        entity
        (enfold-entity symbol entity)))))


;; proc��syntax�ˡ��¹Ի��ƻ���ղä����³����
;; import/sv�Ǥϥ⥸�塼����˥����Х�«�����äƤ��ޤ�����
;; ���줬���ʻ��Ϥ������ľ�˻Ȥ��С��ƻ���ղäǤ��롣
(define (enfold-entity symbol entity)
  (cond
    ((or
       (is-a? entity <procedure>)
       (is-a? entity <generic>)
       )
     (enfold-proc symbol entity))
    ((is-a? entity <macro>)
     (enfold-macro symbol entity))
    ((is-a? entity <syntax>)
     (enfold-syntax symbol entity))
    (else
      entity)))


;; ���μ�³���ϡ����̤�proc��eval/sv������֤Ǥ���褦�ˤ���ݤ˻Ȥ���
;; ���μ�³�����̤����ˤ�äơ�true-proc�μ¹Ԥ�supervise-proc��
;; �ե��륿��󥰤���������Ǥ��롣
;; ����enfold���оݤ�Gauche�Ȥ߹��ߤμ�³�����ä����ϡ�
;; ̵�¥롼���к��Ȥ��ơ�ͽ���к�����Ƥ����ΤˤĤ��Ƥϡ�
;; ��ưŪ�����ؼ�³���˺����������롣
(define (enfold-proc symbol true-proc)
  (guard (e (else
              (generate-enfolded-proc symbol true-proc)))
    (hash-table-get *table:proc->replacement* true-proc)))


;; Gaucheɸ���macro�ڤ�special form�ϡ�����Ū�ˤ�
;; *table:syntax-entity->replacement*����Ͽ����Ƥ��롣
;; ���Υơ��֥����Ͽ����Ƥ����硢�����enfold��μ��ΤȤ����֤���
;; ��Ͽ����Ƥ��ʤ����ϰʲ��ε�ư�Ȥ������
;; - �ޥ���ʤ餽�Τޤ��֤���
;; - special form�ʤ顢specialform->sv-specialform�ˤ����ƥޥ������롣

(define (enfold-macro name-symbol entity)
  (hash-table-get *table:syntax-entity->replacement* entity entity))

(define (enfold-syntax name-symbol entity)
  (guard (e (else
              (specialform->sv-specialform name-symbol entity)))
    (hash-table-get *table:syntax-entity->replacement* entity)))


(provide "eval-sv/enfold")

