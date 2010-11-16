;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; "eval with counting up (lite)" module
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


;;; ���Υ⥸�塼��ϡ��¹ԥ�������դ�ɾ����(�Υ����ͥ졼��)���󶡤��롣

;;; �Ȥ���:
;;; - (make-eval/cu ...)�ˤ�äơ�eval/cu�ڤӡ�eval/cu���������ݻ����Ƥ���
;;;   �Ķ��������롣
;;; - (eval/cu expr threshold)�ǡ�expr��ɾ�����롣
;;;   threshold����������expr����λ���ʤ����ϡ�������㳰���ꤲ���롣
;;;   (����Ȥ��̤ˡ��ޥ���Ÿ���䡢do���Υ롼�פ���������ã�������⡢
;;;    ������㳰���ꤲ���롣)
;;; -- �����㳰�ȶ���³�Ԥ���٤η�³���ꤲ����Τǡ�
;;;    ���η�³��¹Ԥ����ɾ����Ƴ������������ǽ��
;;; - (eval-cu-last-count)�ǡ��Ǹ�����ｪλ�����ݤΡ�
;;;   �Ǹ�μ¹��ͤ������롣
;;;   (�㳰��λ���������������¹��ͤ������ʤ���ɬ�פʤ顢
;;;    �㳰���֥������Ȥ����������)
;;; - ���Ȥϡ����Ρ�eval/sv��Ʊ����eval/cu���Ȥ�import/sv��������ǽ��


;;; usage
#|
(define-values (eval/cu env)
  (make-eval/cu :isolate-port? bool
                :parent-module [atmn 'r5rs+]
                :bind-alist '()
                :default-threshold 200
                :macro-threshold 200
                :loop-threshold 200
                ))
(guard (e ((<eval-cu-exceeded> e)
           (let ((type (ece->type e)) ; 'proc or 'macro or 'loop
                 (count (ece->count e)) ; ���λ����ǤΥ�������
                 (continue (ece->continue e))) ; ³�Ԥ���٤η�³
                 ;; ��continue��Ȥä�³�Ԥ������⡢�������ͤ�
                 ;;   ³�Ԥ���³����������
                 ;;   (������㳰���ƤӽФ����Τ�threshold�μ����ܿ�)
             ;; �ºݤΥ����С�������Ƚ����򤳤��˽�
             ...))
          (else
            ;; ����¾�Υ��顼�����򤳤��˽�
            ...))
  ;; expr�ˤ�eval����������threshold�ˤϥ�����Ȥ����ͤ����ꤹ��
  (eval/cu expr threshold))

(eval-cu-last-count)
;; �����¹Ԥ�����ǡ��Ǹ�˼¹Ԥ���eval/cu�Υ�������ͤ������롣
;; (����ϡ�eval/cu������˽�λ��������
;;  ������Ȥ��ɤ��ޤǿʤ���Τ��Τꤿ�����˻Ȥ���)
;; �����parameter�ʤΤǡ�����Ҥ�threadư�������԰¤�������ϡ�
;; parameterize���Ƥ����а����˻��Ȥ����������롣
|#


(define-module eval-cu-lite
  (use gauche.parameter)
  (use eval-sv)

  (export
    make-eval/cu
    import/sv
    import/cu
    enfold-entity
    eval-cu-last-count
    atmn
    <eval-cu-exceeded>
    ece->type
    ece->count
    ece->continue
    ))
(select-module eval-cu-lite)

;;; ----

(define import/sv import/sv)
(define import/cu import/sv)
(define enfold-entity enfold-entity)
(define atmn atmn)

;; �ǥե���Ȥ�����
(define *default-threshold* 200)
(define *macro-threshold* 200)
(define *loop-threshold* 200)

(define *unspecified* (gensym))

;; parameter
(define p:nested? (make-parameter #f))
(define eval-cu-last-count (make-parameter #f))

;;; ----

;; ����ǥ��������������
(define-condition-type <eval-cu-exceeded> <error>
  eval-cu-exceeded?
  (type ece->type)
  (count ece->count)
  (continue ece->continue))


(define-macro (ke symbol) ; keyword-expand
  `(if (eq? ,symbol *unspecified*)
     '()
     (list ,(make-keyword symbol) ,symbol)))

(define (make-eval/cu . keywords)
  (let-keywords keywords ((isolate-port? *unspecified*)
                          (parent-module *unspecified*)
                          (bind-alist *unspecified*)
                          (default-threshold *default-threshold*)
                          (macro-threshold *macro-threshold*)
                          (loop-threshold *loop-threshold*)
                          )
    ;; �������ݻ�����eval/sv��env���Ѱդ���
    (receive (eval/sv env) (apply
                             make-eval/sv
                             `(
                               ,@(ke isolate-port?)
                               ,@(ke parent-module)
                               ,@(ke bind-alist)
                               ))
      ;; �֤��ͤȤ����֤�eval/cu���Ѱդ���
      (define (eval/cu expr . opt-threshold)
        (let ((threshold (get-optional opt-threshold default-threshold))
              ;; �������ݻ�������ͤ��Ѱդ���
              (proc-count 0) ; ���줬threshold��ã������proc�¹ԡ�0�����
              (proc-total 0) ; proc��������ȿ�(proc�ϸ���Τǡ������Ѱ�)
              (macro-count 0)
              (macro-total 0)
              (loop-count 0)
              (loop-total 0)
              )
          ;; eval/sv���Ϥ���³������������
          (define (supervise-proc type symbol expr args return except)
            ;; ̵�ºƵ��¹Ԥ��ɤ��٤ˡ��¹Ԥ�����˥�����Ȥ�Ԥ�ɬ�פ�����
            (let/cc continue
              ;; TODO: ����ν�����¿���Τ�let-syntax�Ȥ��ǥޥ���������
              (cond
                ((or (eq? type 'proc) (eq? type 'syntax))
                 (inc! proc-count)
                 (inc! proc-total)
                 (when (<= threshold proc-count)
                   (set! proc-count (- proc-count threshold))
                   (except <eval-cu-exceeded>
                           :type 'proc
                           :count (- proc-total 1)
                           :continue continue
                           "proc limit exceeded")))
                ((eq? type 'macro)
                 (inc! macro-count)
                 (inc! macro-total)
                 (when (<= macro-threshold macro-count)
                   (set! macro-count (- macro-count macro-threshold))
                   (except <eval-cu-exceeded>
                           :type 'macro
                           :count (- macro-total 1)
                           :continue continue
                           "macro limit exceeded")))
                ((eq? type 'loop)
                 (inc! loop-count)
                 (inc! loop-total)
                 (when (<= loop-threshold loop-count)
                   (set! loop-count (- loop-count loop-threshold))
                   (except <eval-cu-exceeded>
                           :type 'loop
                           :count (- loop-total)
                           :continue continue
                           "loop limit exceeded")))
                (else
                  (error "assertion"))))
            ;; ������Ƚ����塢�ץ�����¹Ԥ���
            (apply expr args))

          ;; �¹����ˡ��ͥ��ȥե饰��Ω�Ƥ�
          (parameterize ((p:nested? #t))
            (receive r (eval/sv expr supervise-proc)
              (eval-cu-last-count proc-total)
              (apply values r)))))

      ;; ��������eval/cu��env���֤�
      (values eval/cu env))))


;;; ----

(provide "eval-cu-lite")


