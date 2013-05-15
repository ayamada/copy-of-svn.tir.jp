;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; 概要:
;;; dbmをデータストレージとして利用するセッションマネージャ。
;;; セッションにはread/write invarianceの保証されたデータのみ保存できる。

;;; 保存形式:
;;; dbmのkeyとしてsidを、valとして'(data timestamp-epoch)を保存する。
;;; よって、val自体は#fにはならない。

#|
(define *session* (make <session-dbm>
                        :dbm-type <fsdbm>
                        :dbm-path "/path/to/dbm-file"
                        :expire-second (* 1 24 60 60)))
(with-session
  *session*
  sid ; or #f
  (lambda (session-data)
    ;; sidが#fまたは不正またはタイムアウトしていたら、session-dataには#fが入る
    ;; session-dataが#fの時のみ、create-session!が可能
    (let1 new-sid (create-session! new-session-data)
      ;; 新たなセッションを作成し、そこにデータを保存し、そのsidを受け取る
      ;; (このsidを、次のwith-session呼び出し時に渡す)
      ...)

    ;; session-dataが#f以外の時のみ、update-session!とremove-session!が可能
    (update-session! new-session-data) ; セッション内のデータを更新する
    (remove-session!) ; セッションを削除する(ログアウト処理等で使う)
    ...) ; このprocの返り値は、with-sessionの返り値になる
|#


(define-module tir04.session.dbm
  (use srfi-1)
  (use srfi-2)
  (use dbm)
  (use tir04.dbm.expiry)
  (extend tir04.session)

  (export
    <session-dbm>
    with-session
    create-session!
    update-session!
    remove-session!
    ))
(select-module tir04.session.dbm)

(define-class <session-dbm> (<session>)
  ((dbm-type
     :accessor dbm-type-of
     :init-keyword :dbm-type
     :init-value #f)
   (dbm-path
     :accessor dbm-path-of
     :init-keyword :dbm-path
     :init-value #f)
   (dbm
     :accessor dbm-of
     :init-value #f)
   ))

(define-method initialize ((self <session-dbm>) initargs)
  (next-method)
  (unless (dbm-type-of self)
    (error ":dbm-type must be required"))
  (unless (dbm-path-of self)
    (error ":dbm-path must be required"))
  #t)



(define-method inner:with-session-prehandler ((self <session-dbm>) sid)
  (set! (dbm-of self)
    (dbm-open <dbm-expiry>
              :dbm-type (dbm-type-of self)
              :path (dbm-path-of self)
              :rw-mode :write
              :key-convert #f
              :value-convert #t)))

(define-method inner:with-session-posthandler ((self <session-dbm>) sid)
  (dbm-close (dbm-of self))
  (set! (dbm-of self) #f))

(define-method inner:gc-session! ((self <session-dbm>))
  ;; TODO: バージョンアップに伴い動かなくなってる、あとで対策する事
  ;(dbm-gc! (dbm-of self))
  )

(define-method inner:get-session ((self <session-dbm>) sid fallback)
  (dbm-get (dbm-of self) sid #f))

(define-method inner:touch-session! ((self <session-dbm>) sid)
  ;; TODO: バージョンアップに伴い動かなくなってる、あとで対策する事
  ;(dbm-touch! (dbm-of self) sid #f)
  )

(define-method inner:put-session! ((self <session-dbm>) sid session-data)
  (dbm-put! (dbm-of self) sid session-data))

(define-method inner:delete-session! ((self <session-dbm>) sid)
  (dbm-delete! (dbm-of self) sid))



(define-method with-session ((self <session-dbm>) sid proc)
  (next-method))
(define-method create-session! ((self <session-dbm>) session-data)
  (next-method))
(define-method update-session! ((self <session-dbm>) session-data)
  (next-method))
(define-method remove-session! ((self <session-dbm>))
  (next-method))



;;; --------


(provide "tir04/session/dbm")
