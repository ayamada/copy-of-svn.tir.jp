;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; 概要:
;;; メモリ(hash-table)をデータストレージとして利用するセッションマネージャ。
;;; セッションにはread/write invarianceの保証されたデータ以外にも、
;;; Gaucheで扱える、あらゆる値を保存する事が可能だが、
;;; プロセスが終了するとデータは消える。

;;; 保存形式:
;;; hash-tableのkeyとしてsidを、valとして'(data timestamp-epoch)を保存する。
;;; よって、val自体は#fにはならない。

#|
(define *session* (make <session-memory>
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


(define-module tir04.session.memory
  (use srfi-1)
  (use srfi-2)
  (extend tir04.session)

  (export
    <session-memory>
    with-session
    create-session!
    update-session!
    remove-session!
    ))
(select-module tir04.session.memory)

(define-class <session-memory> (<session>)
  ((table
     :accessor table-of
     :init-form (make-hash-table 'equal?))
   ))



(define-method inner:gc-session! ((self <session-memory>))
  (let1 timeout-sids (filter
                       (lambda (x) x)
                       (hash-table-map
                         (table-of self)
                         (lambda (key val)
                           (if (< (+ (cadr val) (expire-second-of self))
                                  (sys-time))
                             key
                             #f))))
    (for-each
      (lambda (sid)
        (hash-table-delete! (table-of self) sid))
      timeout-sids)))

(define-method inner:get-session ((self <session-memory>) sid fallback)
  (let1 val (hash-table-get (table-of self) sid #f)
    (cond
      ((not val) fallback)
      ((< (+ (cadr val) (expire-second-of self)) (sys-time))
       (inner:delete-session! self sid)
       fallback)
      (else
        (car val)))))

(define-method inner:touch-session! ((self <session-memory>) sid)
  (and-let* ((old-val (hash-table-get (table-of self) sid #f)))
    (hash-table-put! (table-of self)
                     sid
                     (list
                       (car old-val)
                       (sys-time)))))

(define-method inner:put-session! ((self <session-memory>) sid session-data)
  (hash-table-put! (table-of self)
                   sid
                   (list
                     session-data
                     (sys-time))))

(define-method inner:delete-session! ((self <session-memory>) sid)
  (hash-table-delete! (table-of self) sid))



(define-method with-session ((self <session-dbm>) sid proc)
  (next-method))
(define-method create-session! ((self <session-dbm>) session-data)
  (next-method))
(define-method update-session! ((self <session-dbm>) session-data)
  (next-method))
(define-method remove-session! ((self <session-dbm>))
  (next-method))


;;; --------


(provide "tir04/session/memory")
