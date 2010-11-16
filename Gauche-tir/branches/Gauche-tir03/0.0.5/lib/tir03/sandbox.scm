;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; sandbox module

;;; ���Υ⥸�塼��ϡ�������¿���������³���Ф��ơ�
;;; ���줾����Ω����̵̾�⥸�塼����֤��󶡤���٤Ρ�
;;; sandbox�⥸�塼�����������٤δؿ�����󶡤���⥸�塼��Ǥ���
;;; - �ޤ�����ö��sandbox�θ��Ȥʤ롢�ƥ�ץ졼�ȥ⥸�塼��(̾��ͭ��)��
;;;   make-template-module�ˤ�äơ��������ޤ���
;;; - �����ơ����θ塢make-sandbox-module-from-template-module�ˤ�äơ�
;;;   �ƥ�ץ졼�ȥ⥸�塼�뤫�顢̵̾�⥸�塼����������ޤ���

;;; note: ���Υ⥸�塼��ϡ�(���դ���äƤ��뤫���Τ�ʤ�)������¿����
;;;       S����ɾ��������٤Ρ����а�����sandbox�����������ΤǤϤ���ޤ���
;;;       ���ߤΤȤ���null�١�����sandbox�Ǥ��äƤ⡢�ưפ�DoS��
;;;       ������������ǽ�Ǥ���
;;;       �����ޤǤ⡢������֤��򤱤�٤Τ�ΤǤ��������դ��Ʋ�������

;;; note: ���ߤΤȤ���gauche�١�����sandbox�Ǥϡ������뵡ǽ��
;;;       ͭ���ˤʤäƤ��ޤ���
;;;       ����ϡ��⥸�塼���require/provide���䡢���饹��������ˡ�
;;;       ������֤�ȯ�������ꤹ���ǽ��������Ȥ������Ǥ���
;;;       ���Τ褦��������򤱤������ϡ�scheme(�ޤ���r5rs)�١�����sandbox��
;;;       ��������褦�ˤ��Ʋ�������
;;;       �����ϡ����Τ褦�ʾ��֤ˤʤ�ʤ��褦�ˡ���տ������Ѥ���褦��
;;;       ���Ʋ�������

;;; note: scheme�١�����sandbox��������Ϥ��ݤˡ�stdin/stdout/error�γ�port�ϡ�
;;;       �ѹ����Ƥ����������������Ǥ���
;;;       (�����ޤǤϡ����Υ⥸�塼��Ǥ����ݤ򸫤��������ޤ���)


(define-module tir03.sandbox
  (export
    make-template-module
    make-sandbox-module-from-template-module
    ))
(select-module tir03.sandbox)


;;; --------


(define-syntax disable-bindings
  (syntax-rules ()
    ((_ module) #f)
    ((_ module name . names)
     (begin
       (eval `(define (name . args)
                (,errorf "~a can't be used within this module"
                         'name))
             module)
       (disable-bindings module . names)))))


(define (disable-unsafe-r5rs-bindings module)
  (disable-bindings module
    open-input-file open-output-file
    call-with-input-file call-with-output-file
    with-input-from-file with-output-to-file
    load
    null-environment scheme-report-environment 
    interaction-environment)
  module)


;;; --------


(define (make-template-module module-name-symbol . opt-type)
  (let ((type (get-optional opt-type 'gauche))
        (module (make-module module-name-symbol)))
    (case type
      ((gauche)
       ;; ���λ��ϡ��ä˶�§����������ʤ���
       (eval '(extend gauche)))
      ((scheme r5rs)
       (eval '(extend scheme))
       ;; �����Ǥʤ���ǽ���Τ���«����ػߤ��롣
       (disable-unsafe-r5rs-bindings module))
      ((null)
       ;; ��§���������פ�Ȧ�ġġ�
       (eval '(extend null)))
      (else
        (error "invalid type" type)))
    ;; ToDo: ¾��ɬ�פʽ����ϡ�
    module))


(define (make-sandbox-module-from-template-module template-name)
  (let1 template-module (find-module template-name)
    (let1 sandbox-module (make-module #f)
      ;; sandbox-module�οƤ�template-module�ˤ���
      (eval `(extend ,template-name) sandbox-module)
      (when (global-variable-bound? sandbox-module 'interaction-environment)
        ;; interaction-environment�ϡ���ʬ���ȤȤ���
        (eval `(define (interaction-environment) ,sandbox-module)
              sandbox-module))
      sandbox-module)))


;;; --------


(provide "tir03/sandbox")


