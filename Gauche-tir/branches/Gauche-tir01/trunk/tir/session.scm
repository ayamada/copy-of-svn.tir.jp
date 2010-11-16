#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; session module

;;; ����:
;;; - prefix������ʸ�����key�ϡ�expire�����ॹ������ݻ��ѤʤΤǻȤ�ʤ���
;;; - key��ʸ����Τ߸����б�
;;; - value��S����read/write�Τ߸����б�
;;; - �Ť��ʤä����å�����GC�ϡ�
;;;   ����Ū��delete-all-expired-sessions!��¹Ԥ�����ǹԤ���
;;; - exists?��read����#f���֤äƤ������ϡ��ʲ��Τɤ��餫��
;;;   ���������ɤ���ˤ��Ƥ⡢ɽ�̾�ϡ֥����ॢ���Ȥ����פȤ������ˤ��٤���
;;; -- ������������
;;; -- expire����GC���줿

;;; note: ���ߤ�make���dbm-*����åȤ��ѹ����ƤϤʤ�ʤ�(ȿ�Ǥ���ʤ�)
;;; ToDo: �嵭��������������

;;; ToDo: ��å����פʵ������롣�����˥�å����פʤ顢dbmwl��Ȥ鷺�ˡ����Τޤ�
;;;       dbm��Ȥ��褦�ˤ���


(define-module tir.session
  (use gauche.regexp) ; regexp-quote
  (use srfi-2) ; and-let*

  (use tir.dbmwl)

  (export
    <session>
    session-exists?
    read-session
    touch-session!
    read&touch-session!
    write-session!
    delete-session!
    delete-all-expired-sessions!
    ))
(select-module tir.session)


(define-class <session> ()
  (
   ;; storage����
   (dbm-type
     :accessor dbm-type-of
     :init-keyword :dbm-type
     :init-value #f)
   (dbm-path
     :accessor dbm-path-of
     :init-keyword :dbm-path
     :init-value #f) ; ɬ��
   ;; session�μ�̿����¸����٤�key��prefix
   (expire-key-prefix
     :accessor expire-key-prefix-of
     :init-keyword :expire-key-prefix
     :init-value "expire-epoch:")
   ;; session�μ�̿
   (expire-second
     :accessor expire-second-of
     :init-keyword :expire-second
     :init-value (* 1 24 60 60))

   ;; �����ѿ��ѥ���å�
   (dbm-obj
     :accessor dbm-obj-of
     :init-value #f)
   ))

(define-method initialize ((self <session>) initargs)
  (next-method)
  (unless (dbm-path-of self)
    (error "this class must be need to :dbm-path"))
  (set!
    (dbm-obj-of self)
    (make
      <dbmwl>
      :dbm-type (dbm-type-of self)
      :dbm-path (dbm-path-of self)
      )))


;;; ----

(define (get-expire-key self key)
  (string-append (expire-key-prefix-of self) key))

(define-method session-exists? ((self <session>) key)
  (let1 dbm (dbm-obj-of self)
    (and
      (dbm-exists? dbm key)
      (and-let* ((expire-key (get-expire-key self key))
                 (expire-value (dbm-get dbm expire-key))
                 )
        ;; expire���Ƥ�����Τߡ����ߤΥǡ����������Ƥ���#f���֤�
        ;; expire���Ƥ��ʤ����#t
        (when (or
                (not (integer? expire-value)) ; ���餫�ΰ۾���
                (< expire-value (sys-time)) ; expire���Ƥ���
                )
          (delete-session! key)
          #f)))))


(define-method read-session ((self <session>) key)
  (let1 dbm (dbm-obj-of self)
    (with-dbm-write-lock ; ���å�����˴��򤹤�٤�write lock�Ǥ���ɬ�פ�����
      dbm
      (lambda ()
        (and
          (session-exists? self key)
          (dbm-get dbm key))))))


(define-method touch-session! ((self <session>) key)
  (let* ((dbm (dbm-obj-of self))
         (expire-key (get-expire-key self key))
         (expire-value (+ (sys-time) (expire-second-of self)))
         )
    (dbm-put! dbm expire-key expire-value)))


(define-method read&touch-session! ((self <session>) key)
  (let1 dbm (dbm-obj-of self)
    (with-dbm-write-lock
      dbm
      (lambda ()
        (begin0
          (read-session self key)
          (touch-session! self key))))))


(define-method write-session! ((self <session>) key value)
  (let1 dbm (dbm-obj-of self)
    (with-dbm-write-lock
      dbm
      (lambda ()
        (begin0
          (dbm-put! dbm key value)
          (touch-session! self key))))))


(define-method delete-session! ((self <session>) key)
  (let ((dbm (dbm-obj-of self))
        (expire-key (get-expire-key self key))
        )
    (with-dbm-write-lock
      dbm
      (lambda ()
        (dbm-delete! dbm key)
        (dbm-delete! dbm expire-key)))))


(define-method delete-all-expired-sessions! ((self <session>))
  (let ((matcher (string->regexp
                   (string-append
                     "^"
                     (regexp-quote (expire-key-prefix-of self)))))
        (dbm (dbm-obj-of self))
        )
    (for-each
      (lambda (key)
        (or
          (matcher key) ; expire�����ॹ�����key�ξ��ϥѥ�����
          (session-exists? self key)))
      (dbm-map
        dbm
        (lambda (key value) key)))))


(provide "tir/session")

