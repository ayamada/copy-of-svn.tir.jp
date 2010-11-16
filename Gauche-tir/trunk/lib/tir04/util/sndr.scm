;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ������������⥸�塼�롣

;;; note: normal-distribution-random�ˤĤ��Ƥϡ��ʲ���url�Ρ���������פ򻲾�
;;; - http://ja.wikipedia.org/wiki/%E4%B9%B1%E6%95%B0
;;; -- �ܥå���=�ߥ塼�顼ˡ�ˤ�����



(define-module tir04.util.sndr
  (use math.mt-random)

  (export
    <sndr>
    set-seed! ; seed�ͤ�����ꤹ�롣
    get-state ; ������mt�ξ��֤��ɤ߽Ф���
    set-state! ; ������mt�ξ��֤�����ꤹ�롣

    ;; methods
    two-standard-normal-distribution-random
    ;; (two-standard-normal-distribution-random sndr) �ϡ�
    ;; ��Ĥ�ɸ������ʬ�������¿�ͤ��֤���
    ;; ��®�����̤�����������������ݤ����Ѥ��롣
    standard-normal-distribution-random
    ;; (standard-normal-distribution-random sndr) �ϡ�
    ;; ��Ĥ�ɸ������ʬ��������֤���
    ;; ��Ψ��two-standard-normal-distribution-random��Ⱦʬ��
    normal-distribution-random
    ;; (normal-distribution-random sndr . keywords) �ϡ�
    ;; (sndr-translation (standard-normal-distribution-random sndr))
    ;; ��Ʊ����sndr-translation�ˤĤ��Ƥϸ�ҡ�

    ;; procs
    sndr-translation
    ;; (sndr-translation sndr-number . keywords) �ϡ�
    ;; ɸ������ʬ�����(sndr-number)��keywords�ǻ��ꤵ�줿�ѥ�᡼����
    ;; ��������Ӥ�Ŭ����褦���ѷ�������桼�ƥ���ƥ���³����
    ;; keywords�μ���Ϥ��μ�³������Ƭ���򻲾Ȥ������
    seed->standard-normal-distribution-random
    seed->normal-distribution-random
    ;; (seed->standard-normal-distribution-random seed) �ȡ�
    ;; (seed->normal-distribution-random seed . keywords) �ϡ�
    ;; seed�ͤ���ľ�ܡ������������������桼�ƥ���ƥ���³����
    ;; seed�ͤ�keywords��Ʊ���Ǥ���С���������������������Ʊ���ˤʤ�١�
    ;; ���Υޥåԥ󥰤Ȥ������ѤǤ��롣
    ;; â������Ψ�Ϥ��ʤ갭����
    ))
(select-module tir04.util.sndr)


(define-class <sndr> ()
  (
   (mt
     :accessor mt-of
     :init-keyword :mt
     :init-value #f)
   ))


(define-method initialize ((self <sndr>) initargs)
  (next-method)
  (set! (mt-of self) (make <mersenne-twister>))
  (set-seed! self (or
                    (get-keyword :seed initargs #f)
                    (receive (epoch micro) (sys-gettimeofday)
                      (receive (q r) (quotient&remainder epoch
                                                         (+ (sys-getpid) 1))
                        (+ q r micro)))))
  #t)

(define-method set-seed! ((self <sndr>) seed)
  (mt-random-set-seed! (mt-of self) seed))

(define-method get-state ((self <sndr>))
  (mt-random-get-state (mt-of self)))

(define-method set-state! ((self <sndr>) state)
  (mt-random-set-state! (mt-of self) state))



(define-method two-standard-normal-distribution-random ((self <sndr>))
  ;; �ޤ����֡�0, 1�ϡפ����������Ѱդ���
  (define (get-r)
    (- 1 (mt-random-real0 (mt-of self))))
  (let* ((alpha (get-r)) ; ���ǰ�ΰ١�let*��ɾ������ݾڤ��Ƥ���
         (beta  (get-r)))
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


(define-method standard-normal-distribution-random ((self <sndr>))
  (values-ref (two-standard-normal-distribution-random self) 0))


(define (sndr-translation sndr-number . keywords)
  (let-keywords* keywords (
                           (sigma 1) ; ʬ���٤���ꡣ
                           ;; sigma���礭����ʿ��Ū��ʬ������
                           ;; sigma��0�˶ᤤ��mu���դ����Фʤ��ʤ롣
                           (sigma-plus #f)  ; +������-�����ǡ�sigma���ͤ�
                           (sigma-minus #f) ; �ѹ����������˸��̤˻��ꤹ�롣
                           ;; ����sigma-plus��sigma-minus�˥ޥ��ʥ����ͤ�
                           ;; ���ꤹ����ǡ�Ⱦʬ���ޤ��֤�������ʬ�ۤ�
                           ;; �������ǽ��
                           (mu 0) ; ���ֽФ䤹��������(ʿ�Ѥ����)����ꡣ
                           (clamp-min #f) ; ��̤�clamp������˻���
                           (clamp-max #f) ; ��̤�clamp������˻���
                           )
    (clamp
      (+ mu
         (* sndr-number
            (or
              (if (positive? sndr-number) sigma-plus sigma-minus)
              sigma)))
      clamp-min
      clamp-max)))


(define-method normal-distribution-random ((self <sndr>) . keywords)
  (apply
    sndr-translation
    (standard-normal-distribution-random self)
    keywords))


(define (seed->standard-normal-distribution-random seed)
  (standard-normal-distribution-random (make <sndr> :seed seed)))

(define (seed->normal-distribution-random seed . keywords)
  (apply
    normal-distribution-random
    (make <sndr> :seed seed)
    keywords))




(provide "tir04/util/sndr")

