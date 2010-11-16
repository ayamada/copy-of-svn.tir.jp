#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; simple lock module

;;; ����:
;;; - ��å�����ȡ�port���֤���
;;; - �����å�������ϡ���å���������port���Ϥ���
;;; - fcntl����
;;; - ���ΤȤ����Υ�֥�å��󥰥⡼�ɤϥ��ݡ��Ȥ��Ƥ��ʤ�
;;;   (ɬ�פˤʤäƤ�����)
;;; - �ǥåɥ�å������ǽ��������



(define-module tir.lock
  (use gauche.fcntl)
  (use srfi-2) ; and-let*
  (use file.util)

  (export
    read-lock
    write-lock
    unlock
    with-read-locks
    with-write-locks
    ))
(select-module tir.lock)


;; ----


(define *read-lock*
  (make
    <sys-flock>
    :type F_RDLCK))
(define *write-lock*
  (make
    <sys-flock>
    :type F_WRLCK))
(define *unlock*
  (make
    <sys-flock>
    :type F_UNLCK))

(define (touch-path path)
  (or
    (file-exists? path)
    (touch-file path)))

(define (read-lock path)
  (touch-path path)
  (let1 p (open-input-file path)
    (sys-fcntl p F_SETLKW *read-lock*)
    p))

(define (write-lock path)
  (let1 p (open-output-file
            path
            :if-does-not-exist :create
            :if-exists :append
            )
    (sys-fcntl p F_SETLKW *write-lock*)
    p))

(define (unlock p)
  (when (output-port? p)
    (flush p))
  (sys-fcntl p F_SETLKW *unlock*)
  (cond
    ((input-port? p) (close-input-port p))
    ((output-port? p) (close-output-port p))
    (else (errorf "cannot unlock ~s" p))))


(define (with-lock locker path thunk)
  (let1 p #f
    (dynamic-wind
      (lambda ()
        (set! p (locker path)))
      thunk
      (lambda ()
        (unlock p)))))

(define (with-locks locker thunk . pathes)
  (if (null? pathes)
    (thunk)
    (with-lock
      locker
      (car pathes)
      (lambda ()
        (apply with-locks locker thunk (cdr pathes))))))

(define (with-read-locks thunk . pathes)
  (apply with-locks read-lock thunk pathes))

(define (with-write-locks thunk . pathes)
  (apply with-locks write-lock thunk pathes))


(provide "tir/lock")

