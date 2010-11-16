#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; mydbm client/server module

;;; 仕様:
;;; - keyは文字列のみ固定対応
;;; - valueはS式のread/writeのみ固定対応？
;;; - serverはsocketで待ち受ける
;;; - 認証は、socketに接続できるか否か、のみとする


;;; 機能:
;;; - 


;;; インターフェース:
;;; - (create table)
;;; - (drop table)
;;; - (exists table)
;;; - (copy table)
;;; - (read table key)
;;; - (write table key val)
;;; - (exists table key)
;;; - (delete table key)
;;; - (lock table)
;;; - (lock table write)
;;; - (lock table read)
;;; - (unlock table)
;;; - (unlock)
;;; - (fold table proc knil)
;;; - (for-each table proc)
;;; - (map table proc)
;;; - (flush table)
;;; - (flush&lock table)


;;; ToDo: dbmの仕様を満たす事

(define-module tir.mydbm
  (use srfi-2) ; and-let*
  (use dbm)

  (use tir.lock)

  (export
    <mydbm-server>
    mydbm-server-start

    <mydbm-client>
    ))
(select-module tir.mydbm)


(autoload dbm.fsdbm <fsdbm>)


(define-class <mydbm-server> ()
  (
   (database-dir
     :accessor database-dir-of
     :init-keyword :database-dir
     :init-value #f) ; 必須
   (dbm-type
     :accessor dbm-type-of
     :init-keyword :dbm-type
     :init-form <fsdbm>)

   ;; 以下のようなリストを指定する。
   ;; '(unix "/tmp/hoge.sock" [:reuse-addr? flag])
   ;; '(inet port-number [:reuse-addr? flag])
   ;; '(tcp server-addr port-number [:reuse-addr? flag]) ; 独自拡張。
   (socket-spec
     :accessor socket-spec-of
     :init-keyword :socket-spec
     :init-value #f) ; 指定必須

   ;; 内部変数用スロット
   (database-table
     :accessor database-table-of
     :init-value (make-hash-table 'eq))
   ))

(define-method initialize ((self <mydbm-server>) initargs)
  (next-method)
  (unless (database-dir-of self)
    (error "this class must be need to :database-dir"))
  (unless (socket-spec-of self)
    (error "this class must be need to :socket-spec"))
  )


;;; ----


(define *lockfile* "mydbm.lock")

(define-method hoge ((self <mydbm-server>))
  #f)


;;; ----


(provide "tir/mydbm")

