#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; dbm with lock module

;;; ����:
;;; - key��ʸ����Τ߸����б�
;;; - value��S����read/write�Τ߸����б�

;;; note: dbm-get��dbm-put!���Ͼ���˥�å���������Τǰ�����
;;;       (���Υ⥸�塼���ͳ�ǻȤäƤ���¤��)
;;;       â����read-lock�򤫤������֤�dbm-put!���Ǥ��Ƥ��ޤ���
;;;       ���ξ�硢atomic�����ݾڤ���ʤ��ʤäƤ��ޤ��Τǡ�
;;;       �����ʤ�ʤ��褦���⥸�塼���Ȥ�¦�������դ��������������

;;; note: pth/forkư����ˡ�dbm-get/put!����
;;;       race condition���������ǽ��������
;;;       ��pth/fork��Ȥ�ʤ���а�����

;;; ToDo: dbm�λ��ͤ���������

;;; ToDo: read lock��write lock��Ʊ���ʤ��褦�ˡ��ɤ���Υ�å�����Ƚ�̤���
;;;       �٤Υ���åȤ��ɲä���������б�������
;;;       ���ξ�硢read lock���write lock�������ϡ�lock���ѹ�����褦��
;;;       ���٤������������ˤ��ȡ�������֤�ȯ�������ǽ�������뤬��
;;;       ��å��ե������ɤ����lock���Ǥ�rw�⡼�ɤǳ����褦�ˤ���С�
;;;       ������֤�������ʤ��褦�ˤǤ���Τǡ��������롣


(define-module tir.dbmwl
  (use srfi-2) ; and-let*
  (use dbm)

  (use tir.lock)

  (export
    <dbmwl>
    get-inner-dbm ; ľ��dbm���󥹥��󥹤���Ф�����ʬ��dbm-close����ɬ��ͭ��
    dbm-read-lock
    dbm-write-lock
    dbm-unlock
    with-dbm-read-lock
    with-dbm-write-lock
    dbm-get
    dbm-put!
    dbm-exists?
    dbm-delete!
    dbm-auto-increment-insert
    ))
(select-module tir.dbmwl)

(autoload dbm.fsdbm <fsdbm>)


(define-class <dbmwl> ()
  (
   (dbm-type
     :accessor dbm-type-of
     :init-keyword :dbm-type
     :init-form <fsdbm>)
   (dbm-path
     :accessor dbm-path-of
     :init-keyword :dbm-path
     :init-value #f) ; ɬ��

   ;; �����ѿ��ѥ���å�
   (dbm-obj
     :accessor dbm-obj-of
     :init-value #f)
   (lock
     :accessor lock-of
     :init-value #f)
   ))

(define-method initialize ((self <dbmwl>) initargs)
  (next-method)
  (unless (dbm-path-of self)
    (error "this class must be need to :dbm-path"))
  )


;;; ----


(define *lockfile-ext* ".dbmwl")

(define-method get-lockfile-path ((self <dbmwl>))
  (string-append (dbm-path-of self) *lockfile-ext*))

(define-method dbm-read-lock ((self <dbmwl>))
  (unless (lock-of self)
    (set!
      (lock-of self)
      (read-lock (get-lockfile-path self)))))

(define-method dbm-write-lock ((self <dbmwl>))
  (unless (lock-of self)
    (set!
      (lock-of self)
      (write-lock (get-lockfile-path self)))))

(define-method dbm-unlock ((self <dbmwl>))
  (let1 lp (lock-of self)
    (when lp
      (unlock lp)
      (set! (lock-of self) #f))))


(define (with-dbm-lock locker self thunk)
  (if (lock-of self)
    (thunk)
    (dynamic-wind
      (lambda ()
        (locker self))
      thunk
      (lambda ()
        (dbm-unlock self)))))

(define-method with-dbm-read-lock ((self <dbmwl>) thunk)
  (with-dbm-lock dbm-read-lock self thunk))
(define-method with-dbm-write-lock ((self <dbmwl>) thunk)
  (with-dbm-lock dbm-write-lock self thunk))


(define-method get-inner-dbm ((self <dbmwl>) rw-mode)
  (dbm-open
    (dbm-type-of self)
    :path (dbm-path-of self)
    :rw-mode rw-mode
    :key-convert #f
    :value-convert #t))


(define (dbm-manipulate/lock locker self thunk)
  (if (lock-of self)
    (thunk)
    (locker
      thunk
      (get-lockfile-path self))))

(define-method dbm-get ((self <dbmwl>) key . opt)
  (dbm-manipulate/lock
    with-read-locks
    self
    (lambda ()
      (let* (
             ;; �ޤ�dbm��̵������create����
             (dbm (with-error-handler
                    (lambda (e)
                      (get-inner-dbm self :create)
                      (get-inner-dbm self :read))
                    (lambda ()
                      (get-inner-dbm self :read))))
             (result (apply dbm-get dbm key opt))
             )
        (dbm-close dbm)
        result))))

(define-method dbm-put! ((self <dbmwl>) key value)
  (dbm-manipulate/lock
    with-write-locks
    self
    (lambda ()
      (let* ((dbm (get-inner-dbm self :write))
             (result (dbm-put! dbm key value))
             )
        (dbm-close dbm)
        result))))

(define-method dbm-exists? ((self <dbmwl>) key)
  (dbm-manipulate/lock
    with-read-locks
    self
    (lambda ()
      (let* ((dbm (get-inner-dbm self :read))
             (result (dbm-exists? dbm key)))
        (dbm-close dbm)
        result))))

(define-method dbm-delete! ((self <dbmwl>) key)
  (dbm-manipulate/lock
    with-write-locks
    self
    (lambda ()
      (let* ((dbm (get-inner-dbm self :write))
             (result (dbm-delete! dbm key)))
        (dbm-close dbm)
        result))))

;; last-insert-id���֤�
;; key-format�� "key-~8,'0d" �Τ褦��ʸ�������ꤹ����ǡ�
;; "key-00000001" �򥭡��Ȥ���value��insert�Ǥ���
;; ����key-format�ϡ�auto-increment���������ˤ�ȤäƤ���Τ������ѹ��Բ�
(define-method dbm-auto-increment-insert ((self <dbmwl>) key-format value)
  (dbm-manipulate/lock
    with-write-locks
    self
    (lambda ()
      (let* ((auto-increment-key (string-append "auto-increment:" key-format))
             (recent-id (dbm-get self auto-increment-key 0)))
        (let loop ((last-insert-id (+ recent-id 1)))
          (let1 key (format key-format last-insert-id)
            (if (dbm-exists? self key)
              (loop (+ last-insert-id 1)) ; ���˥쥳���ɤ�����ʤ鼡���ֹ��
              (begin
                (dbm-put! self key value)
                (dbm-put! self auto-increment-key last-insert-id)
                last-insert-id))))))))


(provide "tir/dbmwl")

