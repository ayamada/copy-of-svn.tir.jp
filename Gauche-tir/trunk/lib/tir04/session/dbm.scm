;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ����:
;;; dbm��ǡ������ȥ졼���Ȥ������Ѥ��륻�å����ޥ͡����㡣
;;; ���å����ˤ�read/write invariance���ݾڤ��줿�ǡ����Τ���¸�Ǥ��롣

;;; ��¸����:
;;; dbm��key�Ȥ���sid��val�Ȥ���'(data timestamp-epoch)����¸���롣
;;; ��äơ�val���Τ�#f�ˤϤʤ�ʤ���

#|
(define *session* (make <session-dbm>
                        :dbm-type <fsdbm>
                        :dbm-path "/path/to/dbm-file"
                        :expire-second (* 1 24 60 60)))
(with-session
  *session*
  sid ; or #f
  (lambda (session-data)
    ;; sid��#f�ޤ��������ޤ��ϥ����ॢ���Ȥ��Ƥ����顢session-data�ˤ�#f������
    ;; session-data��#f�λ��Τߡ�create-session!����ǽ
    (let1 new-sid (create-session! new-session-data)
      ;; �����ʥ��å�����������������˥ǡ�������¸��������sid��������
      ;; (����sid�򡢼���with-session�ƤӽФ������Ϥ�)
      ...)

    ;; session-data��#f�ʳ��λ��Τߡ�update-session!��remove-session!����ǽ
    (update-session! new-session-data) ; ���å������Υǡ����򹹿�����
    (remove-session!) ; ���å�����������(�������Ƚ������ǻȤ�)
    ...) ; ����proc���֤��ͤϡ�with-session���֤��ͤˤʤ�
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
  ;; TODO: �С�����󥢥åפ�ȼ��ư���ʤ��ʤäƤ롢���Ȥ��к������
  ;(dbm-gc! (dbm-of self))
  )

(define-method inner:get-session ((self <session-dbm>) sid fallback)
  (dbm-get (dbm-of self) sid #f))

(define-method inner:touch-session! ((self <session-dbm>) sid)
  ;; TODO: �С�����󥢥åפ�ȼ��ư���ʤ��ʤäƤ롢���Ȥ��к������
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
