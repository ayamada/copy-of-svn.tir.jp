;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ���������Ϣ�⥸�塼�롣

;;; usage:
;;; - (dice) => 1-6�Τɤ줫���Ф�
;;; - (dice 10) => 1-10�Τɤ줫���Ф�
;;; - (dice-1 10) => 0-9�Τɤ줫���Ф�

;;; note: �����seed����¸����٤˼��Ф����ꡢ���åȤ����ꤹ��
;;;       ���󥿡��ե��������Ѱդ��롩����

;;; note: normal-distribution-random�ˤĤ��Ƥϡ��ʲ���url����������򻲾�
;;; - http://ja.wikipedia.org/wiki/%E4%B9%B1%E6%95%B0
;;; -- �ܥå���=�ߥ塼�顼ˡ�ˤ�����



(define-module tir03.util.random
  (use math.mt-random)
  (use math.const)

  (export
    dice-1
    ;; (dice-1) �ϡ�0-5��������֤���
    ;; (dice-1 10) �ϡ�0-9��������֤���
    dice
    ;; (dice) �ϡ�1-6��������֤���
    ;; (dice 10) �ϡ�1-10��������֤���
    list-dice
    ;; (list-dice target-list) �ϡ�target-list�����Ǥΰ�Ĥ��������֤���
    ;; target-list�ϡ�������list�Ǥ������
    two-standard-normal-distribution-random
    ;; (two-standard-normal-distribution-random) �ϡ�
    ;; ��Ĥ�ɸ������ʬ�������¿�ͤ��֤���
    ;; ��®�����̤�����������������ݤ����Ѳ�ǽ��
    standard-normal-distribution-random
    ;; (standard-normal-distribution-random) �ϡ�
    ;; ��Ĥ�ɸ������ʬ��������֤���
    normal-distribution-random
    ;; (normal-distribution-random . keywords) �ϡ�
    ;; Ϳ����줿keywords���顢�׵��̤������������֤���
    ;; keywords�μ���Ϥ��δؿ�����Ƭ���򻲾Ȥ������
    ))
(select-module tir03.util.random)


;; �ͤ�����̡�use����*mt*�򥻥åȤ���褦�ˤ�����ˤ�����
;; ��������ȡ����make-mt!��¹Ԥ��ƥ����å�����ɬ�פ�̵���ʤ롣
;; (����ˡ�����dice����Ȥ�ʤ����ˤ�*mt*������cpu�ȥ������񤹤뤬)
(define *mt*
  (make <mersenne-twister>
        :seed (receive (epoch micro) (sys-gettimeofday)
                (receive (q r) (quotient&remainder epoch (+ (sys-getpid) 1))
                  (+ q r micro)))))

(define (dice-1 . opt-args)
  (let1 num (get-optional opt-args 6)
    (if (zero? num)
      0
      (mt-random-integer *mt* num))))

(define (dice . opt-args)
  (+ 1 (apply dice-1 opt-args)))

(define (list-dice target-list)
  (if (null? target-list)
    (error "null-list cannot use to list-dice")
    (let* ((list-length (length target-list))
           (selected (dice-1 list-length)))
      (list-ref target-list selected))))



(define (two-standard-normal-distribution-random)
  ;; �ޤ����֡�0, 1�ϡפ����������Ѱդ���
  (define (get-r)
    (- 1 (mt-random-real0 *mt*)))
  (let ((alpha (get-r))
        (beta (get-r)))
    ;; �ʲ��η׻���Ԥ����ǡ���Ĥ���ؤ�̵����������������롣
    ;; ((-2 * log(��))^0.5) * sin(2 * �� * ��)
    ;; ((-2 * log(��))^0.5) * cos(2 * �� * ��)
    ;; ����(result-alpha-part)�ȡ�sin/cos����̣(result-beta-part)����˵���
    (let ((result-alpha-part (expt
                               (* -2 (log alpha))
                               0.5))
          (result-beta-part (* 2 pi beta)))
      ;; ��Ĥ�����������֤�
      (values
        (* result-alpha-part (sin result-beta-part))
        (* result-alpha-part (cos result-beta-part))))))

(define (get-sndr-1)
  (receive (r1 r2) (two-standard-normal-distribution-random)
    (set! *sndr-maker* (lambda ()
                         (set! *sndr-maker* get-sndr-1)
                         r2))
    r1))

(define *sndr-maker* get-sndr-1)


(define (standard-normal-distribution-random)
  (*sndr-maker*))


(define (normal-distribution-random . keywords)
  (let-keywords* keywords (
                           (sigma 1) ; ʬ���٤���ꡣ
                           ;; sigma���礭����ʿ��Ū��ʬ������
                           ;; sigma��0�˶ᤤ��mu���դ����Фʤ��ʤ롣
                           (sigma-plus #f) ; +������-�����ǡ�sigma���ͤ�
                           (sigma-minus #f) ; �ѹ����������˸��̤˻��ꤹ�롣
                           ;; ����sigma-plus��sigma-minus�˥ޥ��ʥ����ͤ�
                           ;; ���ꤹ����ǡ�Ⱦʬ���ޤ��֤�������ʬ�ۤ�
                           ;; �������ǽ��
                           (mu 0) ; ���ֽФ䤹��������(ʿ�Ѥ����)����ꡣ
                           (clamp-min #f) ; ��̤�clamp������˻���
                           (clamp-max #f) ; ��̤�clamp������˻���
                           )
    (let1 ndr (standard-normal-distribution-random)
      (clamp
        (+ mu
           (* ndr
              (or
                (if (positive? ndr) sigma-plus sigma-minus)
                sigma)))
        clamp-min
        clamp-max))))


(provide "tir03/util/random")

