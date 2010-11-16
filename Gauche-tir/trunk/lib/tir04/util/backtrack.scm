;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; �Хå��ȥ�å����ݡ��ȥ⥸�塼�롣
;;; ����Ʃ������̵�����˲�Ū�����򵯤����ʤ��Τǡ�
;;; ¾��call/cc�ȶ�¸��ǽ��

;;; TODO:
;;; �ʲ��Τ褦�ʷ��Υޥ���ˤ�������ʬ����䤹���Ȼפ���
;;; (backtrack-let backtrack ((a 1) (b 2) (c 3))
;;;   ...)

#|
�ʲ��Τ褦�˻Ȥ���

(receive (backtrack n m) (backtrack/values 0 20)
  ;; ��n�ˤ�0����m�ˤ�20���Ϥ���롣
  ;;   backtrack��¹Ԥ���ȡ���������äƤ��롣
  ;;   �����ơ�backtrack��¹Ԥ����ݤΰ�����n��m�˿������Ϥ���롣
  ;;   (n��m���ͤ�����ͤ��ɤ����ǡ����¹Ԥ��Хå��ȥ�å�����Ƚ�̤Ǥ���)
  ;;   ��äƤ�����Ǥ⡢backtrack���Ѳ����ʤ���
  ;;   (����Ǥ�backtrack������ѤǤ���)
  (print n)
  (when (< n m)
    ;; ���ǡ�receive�ΤȤ���ޤ���롣
    (backtrack (+ 1 n) m))
  (print "done."))
|#


(define-module tir04.util.backtrack
  (export
    backtrack/values
    ))
(select-module tir04.util.backtrack)


(define-syntax backtrack/values
  (syntax-rules ()
    ((_ . initial-values)
     (let1 backtrack-point #f
       (receive backtrack-values (let/cc cont
                                   ;; ����set!��letrec���ӤʤΤ�������̵��
                                   (set! backtrack-point cont)
                                   (values . initial-values))
         (apply values backtrack-point backtrack-values))))))


(provide "tir04/util/backtrack")

