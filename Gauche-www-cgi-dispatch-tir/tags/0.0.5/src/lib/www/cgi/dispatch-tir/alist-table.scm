;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; alist table module

;;;   Copyright (c) 2005 atsuo yamada, All rights reserved.
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;;; ����:
;;; alist��key�˻Ȥ��롢hash-table�Τ褦�ʥ��饹��
;;; �Ť���
;;; alist���Ф���ޥå��󥰤äݤ���Τ���ǽ�ʻ��ʳ��ˤϥ��åȤ�̵����
;;; alist��value�Ȥ��ơ�:*����ꤹ����ǡ�wildcard���꤬���Ѳ�ǽ��

;;; note: alist��value���Ф���wildcard�ϡ��٤������ꤹ�����Ǥ���(¿ʬ)��
;;;       '(("key" . ("val1" "val2" . :*))) �Ȥ���

;;; note:
;;; �פ���ˡ�Ϳ����alist�������Υơ��֥��key�˻��ꤷ��alist(template)��
;;; ���񤷤Ƥ���ȡ��֥ޥå����Ƥ���פȤߤʤ��ޤ���
;;; �����ޥå�����template��ʣ��������ϡ��ʲ��ξ���ͥ���̤����ꤵ��롣
;;; - ͥ������: ͥ��key(�Ǥ�褯���Ѥ���Ƥ���key)�����template�Ǥ������
;;; - ͥ������: template�ξ�郎������(¿�������Ǥ����alist�Ǥ���)����
;;; - ͥ���پ�: wildcard��ȤäƤ��ʤ�template�Ǥ������

;;; note: ���Υ��饹��(gensym)��ޤ�١����ꥢ�饤������ȡ���̤����������ʤ�
;;;       ��ǽ�����⤤��â����wildcard��Ȥ鷺�����ꥢ�饤����������
;;;       cached-tree-delete!����С����ꥢ�饤����ǽ��

;;; �Ѹ�:
;;; - template = require-template = template-alist
;;;   <alist-table>�ǤΡ�hash-table�ˤ�����key�����̤�����Ρ�
;;;   table������ݤˡ�����template�����Ƥ�key��value�˥ޥå��������Ǥ�
;;;   ���ꤵ�줿alist���ޤ�Ǥ���ʤ顢�ޥå������Ȥߤʤ��졢entity�������롣
;;;   (�פ���ˡ�table������ݤ˻��ꤹ��alist�����񤵤��褦��template��
;;;    ���Ф졢����value���֤롢�Ȥ�����)
;;; - entity = value
;;;   <alist-table>�ǤΡ�hash-table�ˤ�����value�����̤�����Ρ�
;;;   ���̤�key��value�ȸ����ȡ�template-alist��key��value�Ⱥ�Ʊ���Ƥ��ޤä�
;;;   �Τǡ�ʬ����䤹������٤ˡ��虜�虜̾�����̤ˤ�����ˤ�����



(define-module www.cgi.dispatch-tir.alist-table
  (use gauche.parameter)

  (use srfi-1)
  (use srfi-2) ; and-let*

  (use util.list)

  (export
    <alist-table>

    alist-table-put!
    alist-table-get

    alist-table-fold
    alist-table-fold-right
    alist-table-map
    alist-table-for-each
    alist-table-any

    ;; for internal manipulation
    cached-tree-get
    cached-tree-delete!
    cached-tree-update!
    ))
(select-module www.cgi.dispatch-tir.alist-table)


;;; --------


(define *fallback-is-empty* (gensym)) ; fallback��̵�����򼨤�����ܥ�


;;; --------
;;; wildcardȽ���Ϣ

;; note:
;; �����δؿ�������Ѥ������ˤϡ�ɬ����
;; (parameterize ((is-wildcard?-parameter (is-wildcard?-pred-of self))) ... )
;; �������
;; ����Ū�ˤϡ�cached-tree-update!����alist-table-get����ɬ�פˤʤ롣
;; ¾�ˤ⡢�ؿ����ɲä����ݤ�ɬ�פˤʤ뤫�⤷��ʤ���

;; �ǥե���Ȥ�wildcardȽ��ؿ�
(define (is-wildcard?-default x)
  (eq? :* x))

;; wildcardȽ��ؿ���Ȥ��䤹������٤�gauche.parameter�ȥ�åѡ�
(define is-wildcard?-parameter (make-parameter is-wildcard?-default))

(define (is-wildcard? x)
  ((is-wildcard?-parameter) x))


;; WARN: �̾��equal?�Ȥϰ㤤��template¦�ΰ��֤�����ʻ������
(define (wildcard-equal? template value)
  (or
    (equal? template value) ; ���̤˰��פ���ʤ顢���Τޤ��̤�
    (is-wildcard? template) ; template��wildcard�ʤ顢�ʤ�Ǥ�#t
    (and
      (pair? template)
      (pair? value)
      ;; ξ���Ȥ�pair�ʤ顢�Ƶ�Ū�˥����å�����
      (wildcard-equal? (car template) (car value))
      (wildcard-equal? (cdr template) (cdr value)))))

;; WARN: template-vals¦��wildcard��ޤࡣtemplate��key��wildcard�Ϲ�θ���ʤ���
(define (wildcard-assoc-ref template-alist key . opt-default)
  (or
    (any
      (lambda (template-key+val)
        (and
          (wildcard-equal? (car template-key+val) key)
          (cdr template-key+val)))
      template-alist)
    (get-optional opt-default #f)))



;;; --------


(define-class <alist-table> ()
  (
   ;; wildcard���ѹ����������ϡ������˿�����wildcardȽ��ؿ��������
   ;; (�ǥե���Ȥ�wildcard�ϡ�:*�������������:*�פ�template�˽и������ǽ����
   ;;  ����ʤ顢Ŭ���ˡ�template�˽и����ʤ�wildcard��(gensym)����
   ;;  �Ѱդ��ʤ��ƤϤʤ�ʤ���)
   ;; ����ñ��wildcard��ǽ�򻦤��������ϡ�Ŭ����(lambda (x) #f)�Ǥ���ꤹ�����
   (is-wildcard?-pred
     :accessor is-wildcard?-pred-of
     :init-keyword :is-wildcard?-pred
     :init-value is-wildcard?-default)

   ;; entry-list�ϡ��ʲ��Τ褦��list�Ȥ���
   ;; '((entity . template-alist) ...)
   (entry-list
     :accessor entry-list-of
     :init-keyword :entry-list
     :init-value '())

   ;; alist-table��®�����ΰ٤γƼ省��å������򥳥�����¸����
   (cached-tree
     :accessor cached-tree-of
     :init-value #f)
   ))




(define-method alist-table-put! ((self <alist-table>) template entity)
  ;; cached-tree�򥯥ꥢ
  (cached-tree-delete! self)
  ;; entry-list���ɲ�
  (set!
    (entry-list-of self)
    (acons
      entity template
      (alist-delete template (entry-list-of self) alist-equal?))))





(define-method alist-table-get ((self <alist-table>)
                                target-alist . opt-fallback)
  (define (do-fallback)
    (if (null? opt-fallback)
      (errorf "alist table doesn't have an entry for key ~s" target-alist)
      (car opt-fallback)))

  (if (null? (entry-list-of self))
    (do-fallback)
    (parameterize ((is-wildcard?-parameter (is-wildcard?-pred-of self)))
      (unless (cached-tree-of self)
        (cached-tree-update! self))
      (let1 result (resolve-tree (cached-tree-of self) target-alist)
        (if (eq? result *fallback-is-empty*)
          (do-fallback)
          result)))))

(define (resolve-tree cached-tree target-alist)
  (let1 fallback (cadr cached-tree)
    (let resolving ((tree (car cached-tree)))
      (if (null? tree)
        fallback
        (receive (current-branch next-trunk) (car+cdr tree)
          (let ((tree-key (car current-branch))
                (tree-val-list (cadr current-branch))
                )
            (let1 target-key+val (assoc tree-key target-alist)
              (if (not target-key+val)
                (resolving next-trunk)
                (let* ((target-val (cdr target-key+val))
                       (node (wildcard-assoc-ref tree-val-list target-val))
                       )
                  ;; node�ϡ����Τɤä�����
                  ;; - another-cached-tree
                  ;; -- �����ڤ�����Ф�����
                  ;; - #f
                  ;; -- wildcard-assoc-ref��#f���֤������Τ߽и���
                  ;; --- �Ĥޤꡢkey�Ϥ��뤬��value�����פ��ʤ��ä���硣
                  ;;     ���λ��ϡ�fallback������ʤ顢fallback���֤��٤���
                  (if node
                    (let1 result (resolve-tree node target-alist)
                      (if (eq? result *fallback-is-empty*)
                        fallback
                        result))
                    (resolving next-trunk)))))))))))



(define-method cached-tree-get ((self <alist-table>))
  (cached-tree-of self))


(define-method cached-tree-delete! ((self <alist-table>))
  (set! (cached-tree-of self) #f))


(define-method cached-tree-update! ((self <alist-table>))
  (parameterize ((is-wildcard?-parameter (is-wildcard?-pred-of self)))
    (set!
      (cached-tree-of self)
      (make-cached-tree (entry-list-of self)))))



(define (make-cached-tree entry-list)
  (receive (fallbacks others) (partition
                                (lambda (entry)
                                  (null? (cdr entry)))
                                entry-list)
    (list
      (entry-list->internal-tree others)
      (if (null? fallbacks)
        *fallback-is-empty*
        (caar fallbacks)))))

;; note: key��sort��ϡ��и����١�
;; note: value��sort��ϡ�����value���̤�alist��Ĺ����(â����wildcard�ϺǸ�)��
(define (entry-list->internal-tree entry-list)
  (map
    (lambda (key)
      (let1 entry-list/key (filter
                             (lambda (entry)
                               (assoc key (cdr entry)))
                             entry-list)
        (list
          key
          (map
            (lambda (val)
              (cons
                val
                (make-cached-tree
                  (filter-map
                    (lambda (entry)
                      (receive (entity template) (car+cdr entry)
                        (and
                          (member (cons key val) template)
                          (cons
                            entity
                            (alist-delete key template equal?)))))
                    entry-list/key))))
            (uniq-sort-value/wildcard
              key
              (map cdr entry-list/key))))))
    (get-freq-key-list (map cdr entry-list))))


;; alist��Ĺ�����sort�������ꥭ����cdr����(alist-val)��uniq������Τ��֤���
;; â����sort����ݤϡ�alist-val��wildcard��ޤ��Τ���ֺǸ�ˤ��롣
(define (uniq-sort-value/wildcard key target-alists)
  (define (get-wildcard-level val)
    (let search ((target val)
                 (level 0)
                 )
      (cond
        ((is-wildcard? target) level)
        ((not (pair? target)) #f)
        (else
          (let ((car-level (search (car target) (+ 1 level)))
                (cdr-level (search (cdr target) (+ 1 level))))
            (if (and car-level cdr-level)
              (min car-level cdr-level)
              (or car-level cdr-level)))))))

  (delete-duplicates ; ��ʣ����
    (map
      cadr ; �ͤ�������Ф�
      (sort
        ;; �ǽ�ˡ�sort���䤹���褦�ˡ�wildcard������ɤ����ȡ�
        ;; ����wildcard�ΰ��֤ο�����Ĵ�٤Ƥ���
        (map
          (lambda (alist)
            (let1 val (assoc-ref alist key)
              (list*
                (get-wildcard-level val)
                val
                alist)))
          target-alists)
        (lambda (x y)
          (let ((x-wildcard-level (car x))
                (y-wildcard-level (car y)))
            (cond
              ((and x-wildcard-level y-wildcard-level)
               ;; wildcard-level���㤤(���ܤ�wildcard������)�����ˤ���
               ;; Ʊ���ʤ顢���ν�̤��ݤ�
               (<= y-wildcard-level x-wildcard-level))
              ;; wildcard����������ˤ���
              (x-wildcard-level #f)
              (y-wildcard-level #t)
              (else
                ;; Ĺ������¤٤�
                ;; Ʊ���ʤ顢���ν�̤��ݤ�
                (<= (length y) (length x))))))))))


;; �Ƶ�Ū�ˡ�freq-key����Ƥ���
;; (sort���줿cached-tree������٥��keys����������)
(define (get-freq-key-list alists)
  (if (null? alists)
    '()
    (let1 freq-key (get-most-freq-key (concatenate alists))
      (if (not freq-key)
        '()
        (cons
          freq-key
          (get-freq-key-list
            (filter
              (lambda (alist)
                (not (assoc freq-key alist)))
              alists)))))))


;; alist����ǰ������٤ι⤤key���֤�
(define (get-most-freq-key alist)
  (let loop ((left alist) ; Ĵ���о�
             (stat '())) ; key��key��val���и����η��alist
    (if (null? left)
      ;; �Ǹ�ޤǸ��������ʤ顢stat����ǡ��Ǥ⥫����Ȥι⤤key���֤���
      ;; stat�����ʤ�#f���֤���
      (car (fold
             (lambda (new old)
               (if (< (cdr new) (cdr old)) old new))
             '(#f . 0) ; fallback
             stat))
      ;; �Ƶ�Ū��left���������
      (loop
        (cdr left)
        (let1 key (caar left)
          (acons
            key
            (+ (assoc-ref stat key 0) 1)
            (alist-delete key stat equal?)))))))


;; tree�ι�¤��:
;; `(
;;   (
;;    ;; pair ( template-key . child-alist )
;;    (template-key1 (
;;                    ;; pair ( template-value . child-node )
;;                    (template-value1 . ,another-tree-frame1)
;;                    (template-value2 . ,another-tree-frame2)
;;                    (:* . ,another-tree-frame3)
;;                    ))
;;    (template-key2 ( ... ))
;;    (template-key3 ( ... ))
;;    ...
;;    )
;;   ,entity) ; fallback(���alist����key�ǰ����ʤ��ä����˻Ȥ���)






(define-method alist-table-fold ((self <alist-table>) proc knil)
  (fold
    (lambda (entry r)
      (proc (cdr entry) (car entry) r))
    knil
    (entry-list-of self)))
(define-method alist-table-fold-right ((self <alist-table>) proc knil)
  (fold-right
    (lambda (entry r)
      (proc (cdr entry) (car entry) r))
    knil
    (entry-list-of self)))
(define-method alist-table-map ((self <alist-table>) proc)
  (map
    (lambda (entry)
      (proc (cdr entry) (car entry)))
    (entry-list-of self)))
(define-method alist-table-for-each ((self <alist-table>) proc)
  (for-each
    (lambda (entry)
      (proc (cdr entry) (car entry)))
    (entry-list-of self)))
(define-method alist-table-any ((self <alist-table>) proc)
  (any
    (lambda (entry)
      (proc (cdr entry) (car entry)))
    (entry-list-of self)))


;;; --------
;;; ����¾�δؿ�


;; alist�λ����Ǥ�(sort����٤�)ʸ���󲽤���sort�����֤���
(define (alist->string&sort alist)
  (sort
    (map
      (cut write-to-string <>)
      alist)))

;; alist�򡢽���򵤤ˤ�������Ӥ���ؿ�
(define (alist-equal? x y)
  (or
    (equal? x y)
    (and
      (list? x)
      (list? y)
      (eqv? (length x) (length y))
      (let loop ((x-left (alist->string&sort x))
                 (y-left (alist->string&sort y)))
        (cond
          ((and (null? x-left) (null? y-left)) #t)
          ((not (equal? (car x-left) (car y-left))) #f)
          (else
            (loop (cdr x-left) (cdr y-left))))))))




;;; ----

(provide "www/cgi/dispatch-tir/alist-table")

