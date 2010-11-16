;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ����:
;;; ����(hash-table)��ǡ������ȥ졼���Ȥ������Ѥ��륻�å����ޥ͡����㡣
;;; ���å����ˤ�read/write invariance���ݾڤ��줿�ǡ����ʳ��ˤ⡢
;;; Gauche�ǰ����롢�������ͤ���¸���������ǽ������
;;; �ץ�������λ����ȥǡ����Ͼä��롣

;;; ��¸����:
;;; hash-table��key�Ȥ���sid��val�Ȥ���'(data timestamp-epoch)����¸���롣
;;; ��äơ�val���Τ�#f�ˤϤʤ�ʤ���

#|
(define *session* (make <session-memory>
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
