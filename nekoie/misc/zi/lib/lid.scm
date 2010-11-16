;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; local id (lid) module
;;; - �������ΰ���id�ץ⥸�塼��

;;; ����:
;;; ���Υ⥸�塼��ϡ������ΰ���ˤơ���ͭ��id�Ȥ��Ƶ�ǽ���륪�֥������ȵڤ�
;;; ���Υ��֥������ȤΥ��饹����������٤˵������󶡤��롣
;;; �פϡ������ΰ���ǤΤ����Ѥ���(gensym)�פΤ褦�ʤ�Τ��󶡤��롣
;;; ����lid�ϡ�write/read invariance���ݤ���Ƥ��롣
;;; - ��ñ�ˡ�����prefix�ǻϤޤ�symbol��Ȥ��פǤ���Ѿ�������̵���Ȼפ�����
;;;   �������ʣ�����ǽ�����ӽ��������Τǡ����Τ褦�ʻ��Ȥߤˤʤä���

;;; �Ȥ���:
;;; (define-lid-class <lid-sample>
;;;   :gensym gensym-thunk
;;;   :getter getter-proc
;;;   :setter setter-proc
;;;   :initializer init-proc
;;;   )
;;; �ǡ�lid���饹��������롣���������ƾ�ά��ǽ��
;;; - :gensym �ˤϡ�(�Ȥꤢ�������Υ��饹���)��ʣ���ʤ�����ܥ���֤�
;;;   thunk�����ꤹ�롣��ά���ˤϡ�Ŭ�������������顢��ʣ���ʤ�����ܥ뤬
;;;   ��������롣
;;; - :getter �ڤ� :setter �ϡ�object-apply��
;;;   ��lid���󥹥��󥹤μ��Ρפ˥�����������٤�proc�����ꤹ�롣
;;;   :getter�ΰ����ϼ�ʬ���ȡ����Τμ��Ф��˼��Ԥ������ϡ�
;;;   ���顼�㳰���ꤲ�Ƥ�褤����Ŭ����fallback�ͤ��֤��Ƥ�褤��
;;;   :setter�ΰ����ϼ�ʬ���Ȥȿ������ͤ����͡�
;;;   �����Υǥե���Ȥ�#f��
;;;   ���λ��ϡ�object-apply���¹Ԥ����ȥ��顼�㳰���ꤲ�롣
;;;   (�դ˸����ȡ�object-apply��³����Ȥ�ʤ��ʤ��ά���Ƥ�����ʤ�)
;;; - :initializer �ϡ����Υ��饹�Υ��󥹥��󥹤��������줿���˸ƤФ��
;;;   proc�����ꤹ�롣����ϼ�ˡ�lid�����������Ʊ���ˡ����ȥ졼�����
;;;   ���Υ���ȥ���������������˻Ȥ���
;;;   (���ȥ졼���ˤ�äƤϡ��֥���ȥ�������פȡ֥���ȥ�ι����פ�
;;;    ��̩�˶��̤�������ɬ�פ�����٤ˡ����λ��Ȥߤ��Ѱդ�����
;;;    �����Ǥʤ����ȥ졼���Ǥ���С����μ�³�������ꤻ���ˡ�
;;;    ���̤�make���Ƥ��餹���˹�����³����Ƥ֤����Ǥ褤��)
;;;
;;; lid���饹��������줿�顢�ʲ��Τ褦�ˤ���lid���󥹥��󥹤��������롣
;;; (let1 lid (make <lid-sample>)
;;;   ...)
;;; ����lid���󥹥��󥹤�write/read invariance���ݤ���Ƥ���١�
;;; ľ�ܡ����ȥ졼������¸�����ꡢ���ȥ졼�������ɤ߹�����ꤹ�������ǽ��
;;; (â�����ɤ߹������ˡ�define-lid-class���¹Ԥ���Ƥ���ɬ�פ����롣
;;;  ������ǥ꡼�����ޥ��������������Ƥ���١�)
;;; ���λ���ʸ����ɽ���ϡ�������Ǥ����ȡ�
;;; #,(<lid-sample> ???)
;;; �Ȥʤ�(???�ΤȤ���ˤϡ�unique��symbol������)
;;;
;;; ���饹�������:getter :setter��������Ƥ���ʤ顢�ʲ��Υ桼�ƥ���ƥ�
;;; ��³�������Ѳ�ǽ�ˤʤ롣
;;; (lid) => lid�˷���դ���줿���Τ���Ф�(:getter��ƤӽФ�)��
;;;          ����ȥ꤬¸�ߤ��ʤ����ν����ϡ�:getter��¸��
;;;          (���顼�㳰���ꤲ�Ƥ⤤������Ŭ����fallback�ͤ��֤��Ƥ�褤)
;;; (lid new-val) => lid�˷���դ���줿���Τ򹹿�����(:setter��ƤӽФ�)��
;;;                  ���Ρֹ����פμ����ˤĤ��Ƥϡ�:setter��¸��
;;;                  (�˲�Ū������ԤäƤ⤤������new-val���ξ��ˤ�ä�
;;;                   �ºݤˤϲ��⹹���򤷤ʤ��褦�ʵ�ư�ˤ��Ƥ�褤)


(define-module lid
  (use util.match)

  (export
    <lid-meta> ; ��lid�Υ��饹����������٤Υ᥿���饹
    <lid-abstract> ; �������줿��lid�ζ���method���󶡤���٤���ݥ��饹
    define-lid-class
    ))
(select-module lid)


;;; ----


;; fallback-gensym-thunk�Ѥ�������
(define *internal-counter* 0)


;;; ----
;;; ���饹�����󥹥ȥ饯����reader��writer


;; �Ƽ��<lid>����������٤Υ᥿���饹
(define-class <lid-meta> (<class>)
  (
   (symbol :init-keyword :symbol ; reader�ǻȤ�����ܥ�
           :init-form (error "must be need :symbol"))
   (gensym :init-keyword :gensym ; gensymŪ��thunk����Ͽ��̵���Ƥ�ok
           :init-value #f)       ; (���ξ���timestamp���������������)
   (getter :init-keyword :getter ; dbm-getŪ��proc����Ͽ��̵���Ƥ�ok
           :init-value #f)       ; (���ξ���object-apply�ϻ����Բ�)
   (setter :init-keyword :setter ; dbm-set!Ū��proc����Ͽ��̵���Ƥ�ok
           :init-value #f)       ; (���ξ���object-apply�ϻ����Բ�)
   (initializer :init-keyword :initializer :init-value #f)
   ))


;; �������줿��lid�ζ���method���󶡤���٤���ݥ��饹
(define-class <lid-abstract> ()
  ((key))) ; (gensym)������key�μ���(unique�ʥ���ܥ�)
(define-method initialize ((self <lid-abstract>) initargs)
  (receive (k args) (if (null? initargs)
                      (values (generate-key self) '())
                      (values (car initargs) (cdr initargs)))
    (next-method self args)
    (slot-set! self 'key k)
    (let1 initializer (slot-ref (class-of self) 'initializer)
      (when initializer
        (initializer self)))))

(define-method write-object ((self <lid-abstract>) out)
  (format out "#,~s"
          (list
            (slot-ref (class-of self) 'symbol)
            (slot-ref self 'key))))

(define-method object-equal? ((obj1 <lid-abstract>) (obj2 <lid-abstract>))
  (equal?
    (slot-ref obj1 'key)
    (slot-ref obj2 'key)))

(define-method object-hash ((self <lid-abstract>))
  (hash (slot-ref self 'key)))


(define (fallback-gensym-thunk)
  (string->symbol
    (receive (epoch micro) (sys-gettimeofday)
      (let1 result (format "lid_~d_~d_~d_~d"
                           epoch
                           micro
                           (sys-getpid)
                           *internal-counter*)
        (set! *internal-counter* (+ 1 *internal-counter*))
        result))))

(define (generate-key self)
  (let1 gensym-thunk (or
                       (slot-ref (class-of self) 'gensym)
                       fallback-gensym-thunk)
    (gensym-thunk)))

#|
;; �ʲ��Τ褦�ʥ����ɤǡ�<lid>����������
(define-class <lid-sample> (<lid-abstract>)
  ()
  :metaclass <lid-meta>
  :symbol '<lid-sample>
  :gensym (lambda () ...)
  :getter (lambda (key fallback) ...)
  :setter (lambda (key val) ...)
  )
(define-reader-ctor '<lid-sample>
  (lambda (key)
    (make <lid-sample> key)))
;; �����ơ��ʲ��Τ褦�ʥ����ɤǡ�lid�򹥤��ʤ��������Ǥ���
(define lid (make <lid-sample>))
;; �����ޥ�������
(define-lid-class <lid-sample>
                  :gensym thunk
                  :getter proc1
                  :setter proc2
                  :initializer proc3
                  )
|#
(define-macro (define-lid-class symbol . keywords)
  ;; NB: keywords������ʤ顢eval����ɬ�פ�����
  ;;     ��������eval����ɬ�פ�����Τ�keywords��value�������ǡ�
  ;;     ������ʬ��Ÿ�����define-class�Υ����������
  ;;     ľ��Ÿ�������Τǡ�����̵��Ȧ
  (let-keywords keywords ((gensym #f)
                          (getter #f)
                          (setter #f)
                          (initializer #f)
                          )
    `(begin
       (define-class ,symbol (<lid-abstract>)
         ()
         :metaclass <lid-meta>
         :symbol ',symbol
         :gensym ,gensym
         :getter ,getter
         :setter ,setter
         :initializer ,initializer
         )
       (define-reader-ctor ',symbol
         (lambda (key)
           (make ,symbol key))))))



;;; ----
;;; �桼�ƥ���ƥ���³��


;;; object-apply
(define-method object-apply ((self <lid-abstract>))
  (let1 getter (slot-ref (class-of self) 'getter)
    (if getter
      (getter self)
      (error "has not getter" (class-of self) self))))
(define-method object-apply ((self <lid-abstract>) new-value)
  (let1 setter (slot-ref (class-of self) 'setter)
    (if setter
      (setter self new-value)
      (error "has not setter" (class-of self) self))))


;;; ----

(provide "lid")

