;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;; continual constant state holder

;; TODO:
;; ���ΤȤ������󥹥��󥹤�Ʊ����Ƚ��ϡ�identity-symbol����äƤ��뤬��
;; ����Ǥ����꤬���롣
;; ���Υ��󥹥��󥹤����ꡢ�����ơ�����������������̤��Υ��󥹥��󥹤�
;; �����硢����ϸ��ߤμ����Ǥ���������ӤǤ��롣
;; ��������������Υ��󥹥��󥹤���ʣ����̤��Υ��󥹥��󥹤�����������硢
;; ���줾���̤��Υ��󥹥���Ʊ�Τ�Ʊ��Ƚ�ꤵ���٤��Ǥʤ��褦�˻פ��롣
;; (�����Ʊ��Ƚ������Ӥˤ�롢�����ܺ٤Ͼ�ά)
;; �������������硢identity-symbol�ˤ������ǤϤʤ���
;; ̤��Υ��󥹥�����ˡ����Υ��󥹥��󥹤ؤΥ�ե���󥹤��ݻ�����
;; �褦�ˤ���ɬ�פ����롣
;; ����������������ȡ����ĤޤǤ��äƤ���Υ��󥹥��󥹤�GC�������
;; �Ǥ��ʤ��ʤäƤ��ޤ�����⤢�롣
;; (���餫��weak reference�����ˤ���С�����ϲ����ǽ����)


;; ���򤹤��Τ�:
;; - ���ַв�(�������Ѳ�)�˱����ơ����֥������Ȥξ��֤��Ѳ����뤬��
;;   ��������ݥ���ȤǤΥ��֥������Ȥ��Τ�Τ��Ѳ����ʤ���
;;   (�Ĥޤꡢ����Ū�ʲ��ؤΥ����������ݾڤ���)
;;   �Ȥ���������¸�����٤ˡ����֥������Ȥξ��֤��Ѳ����˲�Ū�ѹ��ǤϤʤ���
;;   �־��֤ΰ㤦���������֥������Ȥ�spawn��������Ϥ��ä���ؤ��褦�ˤ����
;;   �Ȥ��������ˤ�������
;; - �Ĥޤꡢ�����Ѥ�̵�������֥������ȹ������󶡤��롣
;; - �����Ѥ�����������ǡ�����Ū������������ǽ�ʥ��󥹥��󥹤�¸����롣

;; �Ȥ���:
;; - <continual-constant-state-holder>��Ѿ��������饹���롣

;; ��ħ:
;; - immutable�Ǥ��롣
;; - ���󥹥��󥹤ξ��֤��Ѳ��ϡ����������󥹥��󥹤�spawn����
;;   ���������󥹥��󥹤ξ��֤��Ѳ�����������򿷤������֤Ȥ��롣
;; -- �Ť����󥹥��󥹤�ؤ��Ƥ����Τϡ��Ť����󥹥��󥹤�ؤ����ޤޤ�
;;    ���Ƥ��������Ǥ��롣
;; - ���󥹥�����ˡ�����Ʊ����������å�����٤μ��̻Ҥ���ġ�
;; -- spawn���������줿���󥹥��󥹤Ȥ����������Υ��󥹥��󥹤ϡ�
;;    Ʊ��μ��̻Ҥ���ġ�
;; -- ���μ��̻Ҥϡ����ˤ��������ǽ�Ȥ��롣
;; - ���Υ��饹�Υ��󥹥��󥹤ϡֶ��פξ��֤���ǽ�Ȥ��롣
;; -- ���̻Ҥ�����ʤ����󥹥��󥹤����ơֶ��װ����Ȥ��롣
;; -- �ֶ���Ʊ�Τ���Ӥ�#f���֤���ΤȤ��롣

;; ����:
;; - spawn���˥���åȤ���(�λ���)�򥳥ԡ�����ط��塢
;;   ���ۥ���åȤ������������꤬ȯ�������ǽ�������롣
;;   ���ۥ���åȤϻȤ�ʤ�����̵��
;; - spawn���˥���åȤ��ͤ򥻥åȤ���٤�:init-keyword��Ȥ��١�
;;   ����åȤˤ�ɬ��:init-keyword�����ꤷ�ʤ��ƤϤʤ�ʤ�������դ������

(define-module tir04.util.ccsh
  (use srfi-1)
  (use srfi-2)

  (export
    <continual-constant-state-holder>
    identity-symbol-of
    void?
    spawn ; ������������å����Ƥΰ㤦�̥��󥹥��󥹤򿷤����������롣
    ))
(select-module tir04.util.ccsh)


(define-class <continual-constant-state-holder> ()
  (
   (identity-symbol ; instance�μ���Ʊ������Ƚ�Ǥ���٤μ��̻�
     :accessor identity-symbol-of
     :init-keyword :identity-symbol
     :init-form (gensym))
   ))

;; TODO: :identity-symbol��#f�Υ��󥹥��󥹤ΰ�����ɤ����뤫���ޤ�̤��
;;       (�����餯��̵���ˤʤä����󥹥��󥹤��̣�����롩)
(define-method void? ((self <continual-constant-state-holder>))
  (not
    (not
      (identity-symbol-of self))))

;; equal?�ǡ�identity-symbol��Ʊ��(�Ĥޤꡢͳ�褬Ʊ��)���ɤ�����
;; Ĵ�٤���褦�ˤ��롣
;; ������Ʊ�줫��Ĵ�٤�ˤ�eq?��Ȥ�����
;; NB: ����ϥȥ�֥�θ��ˤʤ��ǽ�������롣
(define-method object-equal? ((obj1 <continual-constant-state-holder>)
                              (obj2 <continual-constant-state-holder>))
  (and
    (not (void? obj1))
    (not (void? obj2))
    (eq?
      (identity-symbol-of obj1)
      (identity-symbol-of obj2))))

(define (keywords-merge self keywords)
  ;; ����:
  ;; self�λ��ĥ���åȤ��饭����ɤ���Ф��������keywords�Ǿ�񤭤���
  ;; ���η�̤�list���֤���
  ;; (��̤Ȥ��ơ�(apply make (class-of self) result)�Ȥ���Ŭ�ѤǤ���
  ;;  result���֤������ˤʤ�)
  ;; ��â��������åȤ�:init-keyword������ʤ���硢�����̵�뤵���
  ;;   (:init-keyword̵�� = spawn���˾�񤭤��ʤ��Ƥ褤���Ȥ�����̣�ˤȤ館��)
  (apply
    append
    (filter-map
      (lambda (slot-define)
        ;; :init-keyword����Τʤ�����åȤϥ��ԡ����ʤ�
        (and-let* ((init-keyword (get-keyword :init-keyword
                                              (cdr slot-define)
                                              #f)))
          (let1 slot-val (get-keyword init-keyword
                                      keywords
                                      (slot-ref self (car slot-define)))
            (list init-keyword slot-val))))
      (class-slots (class-of self)))))

(define-method spawn ((self <continual-constant-state-holder>) . keywords)
  (let1 merged-keywords (keywords-merge self keywords)
    (apply
      make
      (class-of self)
      merged-keywords)))




(provide "tir04/util/ccsh")
