#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(use gauche.parameter)

(use file.util)
(use rfc.uri)
(use text.html-lite)
(use text.tree)
(use util.list)

(use dbm.qdbm)

(use tir.misc)

(add-load-path ".")
(use socket-cs)
(use config)

;; ----
;; common proc

;; ToDo: ʣ����dbm��Ʊ�������Ǥ���褦�ˤ���
;; ToDo: �����ȥ�å�����������flock�Ȥ��ǡ�ñ�Τ��̥⥸�塼��ˤ������
(define lock-dbm-class (make-parameter #f))
(define lock-dbm-path (make-parameter #f))
(define (lock-with-dbm dbm-class path thunk)
  (parameterize ((lock-dbm-class dbm-class)
                 (lock-dbm-path path))
    ;; �ޤ�¸�ߤ��Ƥ��ʤ��ʤ顢�����˺������Ƥ���
    (unless (dbm-db-exists? dbm-class path)
      (let1 dbm (dbm-open dbm-class :path path :rw-mode :create)
        (dbm-close dbm)))
    (thunk)))
(define (dbm-read key . optional-default)
  ;; ToDo: parameter�Υ����å���#f�ʤ�assertion�򵯤����褦�ˤ����
  (let1 dbm (make
              (lock-dbm-class)
              :path (lock-dbm-path)
              :rw-mode :read
              )
    (dbm-open dbm)
    (begin0
      (apply dbm-get dbm key optional-default)
      (dbm-close dbm))))

(define (dbm-write key value)
  (let1 dbm (make
              (lock-dbm-class)
              :path (lock-dbm-path)
              :rw-mode :write
              )
    (dbm-open dbm)
    (dbm-put! dbm key value)
    (dbm-close dbm)))


;; ----


(define (ss-thunk)
  (let1 line (read-line)
    (cond
      ((eof-object? line) #f)
      ((string=? line "!quit") #f)
      ((string=? line "!shutdown") (shutdown-server *server*) #f)
      (else
        (print line)
        #t))))

(define *server*
  (make
    <socket-server>
    :socket-spec *socket-spec*
    :thunk ss-thunk
    ))

(define (server-mode)
  (start-server *server*))

(define (main args)
  (start-server *server*)
  0)

