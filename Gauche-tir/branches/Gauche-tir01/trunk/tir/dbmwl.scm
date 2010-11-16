#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; dbm with lock module

;;; 仕様:
;;; - keyは文字列のみ固定対応
;;; - valueはS式のread/writeのみ固定対応

;;; note: dbm-getやdbm-put!時は勝手にロックがかかるので安全。
;;;       (このモジュール経由で使っている限りは)
;;;       但し、read-lockをかけた状態でdbm-put!ができてしまう。
;;;       この場合、atomic性は保証されなくなってしまうので、
;;;       そうならないよう、モジュールを使う側が気を付ける事！！！！！

;;; note: pth/fork動作時に、dbm-get/put!時に
;;;       race conditionが起こる可能性がある
;;;       （pth/forkを使わなければ安全）

;;; ToDo: dbmの仕様を満たす事

;;; ToDo: read lockとwrite lockを混同しないように、どちらのロックかを判別する
;;;       為のスロットを追加し、それに対応させる
;;;       その場合、read lock中にwrite lockした場合は、lockを変更するように
;;;       すべきか？今の方針だと、競合状態が発生する可能性があるが。
;;;       ロックファイルをどちらのlock時でもrwモードで開くようにすれば、
;;;       競合状態が起こらないようにできるので、そうする。


(define-module tir.dbmwl
  (use srfi-2) ; and-let*
  (use dbm)

  (use tir.lock)

  (export
    <dbmwl>
    get-inner-dbm ; 直接dbmインスタンスを取り出す。自分でdbm-closeする必要有り
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
     :init-value #f) ; 必須

   ;; 内部変数用スロット
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
             ;; まだdbmが無い時はcreateする
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

;; last-insert-idを返す
;; key-formatに "key-~8,'0d" のような文字列を指定する事で、
;; "key-00000001" をキーとしてvalueをinsertできる
;; このkey-formatは、auto-incrementキー管理にも使っているので途中変更不可
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
              (loop (+ last-insert-id 1)) ; 既にレコードがあるなら次の番号へ
              (begin
                (dbm-put! self key value)
                (dbm-put! self auto-increment-key last-insert-id)
                last-insert-id))))))))


(provide "tir/dbmwl")

