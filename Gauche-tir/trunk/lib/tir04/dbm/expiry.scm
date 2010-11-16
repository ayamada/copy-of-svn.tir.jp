;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ����:
;;; dbm�˻��ַв�ˤ��gc����Ϳ���롣
;;; ���ּ�����(sys-time)�ˤ��١������ʴĶ���¸�򵯤���������դ������

;;; ��¸����:
;;; dbm��key�Ȥ���sid��val�Ȥ���'(data timestamp-epoch)����¸���롣
;;; ��äơ�������¤Ū�ˤϡ�val���Τ�#f�ˤϤʤ�ʤ���
;;; �ޤ���������¸�����ΰ١�����dbm���̤ȸߴ�����̵���ʤ롣
;;; (�Ĥޤꡢ���礷�ƻȤ��ʤ��ʤ롣)

;;; ����:
;;; ������dbm���饹���б�����٤ˡ��Ѿ��ˤ������Ȥ��롣

;;; ToDo: <fsdbm>�ξ��Τߡ��ºݤΥե����륿���ॹ����פ򸫤��̼����Ȥ��롣


(define-module tir04.dbm.expiry
  (use srfi-1)
  (use srfi-2)
  (extend dbm)

  (export
    <dbm-expiry>
    dbm-get! ; �ͤ���������Ʊ���˥����ॹ����פ򹹿�����
    dbm-touch! ; �����ॹ����פΤߤ򹹿����롣key��¸�ߤ��ʤ�����#f���֤�
    dbm-gc! ; ����Ū��gc��Ƥ�
    ))
(select-module tir04.dbm.expiry)


(define *not-found* (gensym))


(define-class <dbm-expiry-meta> (<dbm-meta>)
  ())


(define-class <dbm-expiry> (<dbm>)
  (
   ;; public
   (dbm-type
     :accessor dbm-type-of
     :init-keyword :dbm-type
     :init-value #f)
   (expire-second
     :accessor expire-second-of
     :init-keyword :expire-second
     :init-value (* 1 24 60 60))

   ;; private
   (dbm
     :accessor dbm-of
     :init-value #f)
   )
  :metaclass <dbm-expiry-meta>)


(define-method initialize ((self <dbm-expiry>) initargs)
  (next-method)
  (unless (dbm-type-of self)
    (error ":dbm-type must be required"))
  (set!
    (dbm-of self)
    (apply make (dbm-type-of self) (rewrite-to-initargs initargs)))
  #t)

(define (rewrite-to-initargs initargs)
  ;; ��˽񤭹��ߥ⡼�ɤ���S����¸��Ԥ�ɬ�פ�����
  (list*
    :rw-mode :write
    :value-convert #t
    (delete-keyword :dbm-type
                    (delete-keyword :rw-mode
                                    (delete-keyword :value-convert
                                                    initargs)))))


(define-method dbm-open ((self <dbm-expiry>))
  (next-method) ; <dbm>����Ѿ���������åȤ�����٤�ɬ�ס�
  (dbm-open (dbm-of self)))


(define-method dbm-close ((self <dbm-expiry>))
  (next-method)
  (dbm-close (dbm-of self)))


(define-method dbm-closed? ((self <dbm-expiry>))
  (next-method)
  (dbm-closed? (dbm-of self)))


(define-method dbm-db-exists? ((class <dbm-expiry-meta>) name)
  (next-method)
  (dbm-db-exists? (dbm-of self) name))


(define-method dbm-db-remove ((class <dbm-expiry-meta>) name)
  (next-method)
  (dbm-db-remove (dbm-of self) name))


;;; �ʲ���method�ϡ�expire�������ɲäǼ¹Ԥ���롣


(define-method dbm-put! ((self <dbm-expiry>) key val)
  (next-method)
  (begin0
    (dbm-put! (dbm-of self) key (list val (sys-time)))
    (gc-check! self)))


(define-method dbm-get ((self <dbm-expiry>) key . args)
  (next-method)
  (let1 val (if (null? args)
              (dbm-get (dbm-of self) key)
              (dbm-get (dbm-of self) key *not-found*))
    (if (eq? val *not-found*)
      (car args)
      (if (< (+ (cadr val) (expire-second-of self)) (sys-time))
        (begin
          (dbm-delete! self key)
          (car args))
        (car val)))))


(define-method dbm-exists? ((self <dbm-expiry>) key)
  (next-method)
  (not
    (eq? *not-found* (dbm-get (dbm-of self) key *not-found*))))


(define-method dbm-delete! ((self <dbm-expiry>) key)
  (next-method)
  (dbm-delete! (dbm-of self) key))


(define-method dbm-fold ((self <dbm-expiry>) proc seed)
  (next-method)
  (dbm-gc! self)
  (dbm-fold
    (dbm-of self)
    (lambda (key val prev)
      (proc key (car val) prev))
    seed))


;;; �ʲ���method�ϡ�<dbm-expiry>��ͭ�Τ�Ρ�


(define-method dbm-get! ((self <dbm-expiry>) key . args)
  (if (null? args)
    (begin0
      (dbm-get self key args)
      (dbm-touch! self key))
    (let1 result (apply dbm-get self key *not-found*)
      (if (eq? result *not-found*)
        (car args)
        (begin
          (dbm-touch! self key)
          result)))))


(define-method dbm-touch! ((self <dbm-expiry>) key)
  ;; NB: ����method�Τߡ������ॹ����פ��ڤ�Ƥ��Ƥ⡢
  ;;     ����򵤤ˤ����˹�������ɬ�פ����롣
  ;;     (dbm-get���Ƥ��鲿�餫�ν�����Ԥ����Ǹ��dbm-touch!������̤�
  ;;      �ͤ���졢���δ֤�ͭ�����¤��ڤ�Ƥ��ޤ���
  ;;      �ȥ�󥶥�����󤬼����Ƥ��ޤ��١����Τ褦�ʻ��ͤȤ�����
  ;;      ����dbm-put!�����ϥ���ȥ̵꤬��(gc�Ǽ���줿)����
  ;;      �����˺��������١��ä�����ϵ�����ʤ���)
  (and-let* ((val+timestamp (dbm-get (dbm-of self) key #f)))
    (dbm-put! self key (car val+timestamp))))


(define-method dbm-gc! ((self <dbm-expiry>))
  (let1 timeout-keys (filter
                       (lambda (x) x)
                       (dbm-map
                         (dbm-of self)
                         (lambda (key val)
                           (if (< (+ (cadr val) (expire-second-of self))
                                  (sys-time))
                             key
                             #f))))
    (for-each
      (lambda (key)
        (dbm-delete! (dbm-of self) key))
      timeout-keys)))




(define (gc-check! self)
  ;; ToDo: �����ϲ�����;�Ϥ�����
  (when (= 0 (remainder (values-ref (sys-gettimeofday) 1) 16))
    (dbm-gc!)))

;;; --------


(provide "tir04/dbm/expiry")
