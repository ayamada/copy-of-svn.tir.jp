#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; session module

;;; 仕様:
;;; - prefixが特定文字列のkeyは、expireタイムスタンプ保持用なので使わない事
;;; - keyは文字列のみ固定対応
;;; - valueはS式のread/writeのみ固定対応
;;; - 古くなったセッションのGCは、
;;;   明示的にdelete-all-expired-sessions!を実行する事で行われる
;;; - exists?やread時に#fが返ってきた場合は、以下のどちらか。
;;;   しかし、どちらにしても、表面上は「タイムアウトした」という事にすべき。
;;; -- 不正アクセス
;;; -- expireしてGCされた

;;; note: 現在はmake後にdbm-*スロットを変更してはならない(反映されない)
;;; ToDo: 上記の問題を修正する事

;;; ToDo: ロック不要な気がする。本当にロック不要なら、dbmwlを使わずに、そのまま
;;;       dbmを使うようにする


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
   ;; storage設定
   (dbm-type
     :accessor dbm-type-of
     :init-keyword :dbm-type
     :init-value #f)
   (dbm-path
     :accessor dbm-path-of
     :init-keyword :dbm-path
     :init-value #f) ; 必須
   ;; sessionの寿命を保存する為のkeyのprefix
   (expire-key-prefix
     :accessor expire-key-prefix-of
     :init-keyword :expire-key-prefix
     :init-value "expire-epoch:")
   ;; sessionの寿命
   (expire-second
     :accessor expire-second-of
     :init-keyword :expire-second
     :init-value (* 1 24 60 60))

   ;; 内部変数用スロット
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
        ;; expireしている時のみ、現在のデータを削除してから#fを返す
        ;; expireしていなければ#t
        (when (or
                (not (integer? expire-value)) ; 何らかの異常値
                (< expire-value (sys-time)) ; expireしている
                )
          (delete-session! key)
          #f)))))


(define-method read-session ((self <session>) key)
  (let1 dbm (dbm-obj-of self)
    (with-dbm-write-lock ; セッション破棄をする為にwrite lockである必要がある
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
          (matcher key) ; expireタイムスタンプkeyの場合はパスする
          (session-exists? self key)))
      (dbm-map
        dbm
        (lambda (key value) key)))))


(provide "tir/session")

