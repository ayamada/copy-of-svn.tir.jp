;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; cgi�ѥ�᡼�����׵ᡢ�������٥�ե�������Ԥ����饹��

;;; ��Ū:
;;; - html�ե�������ϡ��ѥ�᡼���Υ��˥�������
;;;   (space�Υȥ�ߥ󥰤�Ⱦ����������)�Ѵ���verify��
;;;   �����ͤΥ��顼ɽ�����ץ�ӥ塼ɽ����(ʸ���󤫤�������ؤ�)���Ѵ���
;;;   (input hidden�ˤ��)CGI�ѥ�᡼�����ݻ���
;;;   �����ϡְ�Ϣ��CGI�ѥ�᡼�������פȤ������ǰ�Ӥ��Ƥ��뤬��
;;;   �̾���������˥Х�Х�˼����������ˤʤ롣
;;;   �����򲿤Ȥ����ư�ս�ˤޤȤ᤿����

;;; ����:
;;; - text.html-lite���ĥ�����ߥ˸���(?)���������ɬ�פ����롣
;;; -- (html:input :type "text" :name "hoge" :value "hoge"
;;;                :x-conv conv-proc
;;;                :x-checker checker-proc
;;;                :x-class <integer>)
;;; - ???


(define-module tir03.cgi.form
  (use gauche.parameter)

  ;(use text.tree)
  ;(use text.html-lite)
  ;(use www.cgi)

  ;(use tir03.cgi)

  (export
    <cgi-form>
    expand-to-form-html ; html�ե������Ÿ��
    expand-to-confirm-html ; ���Ϥ��줿���Ƥ�ץ�ӥ塼ɽ��html��Ÿ��
    ;with-??? ; �ޤ���������
    ;form->hiddens ; �ե�������������Ƥ򡢰�Ϣ��hidden�������Ѵ����롣
    ;; ToDo: ������ʥǡ��������������ʤ��󶡤���ɬ�פ�����
    ))
(select-module tir03.cgi.form)


;;; --------


(define-class <cgi-form> ()
  (
   (template-html
     :accessor template-html-of
     :init-keyword :template-html
     :init-value #f)

   ;; internal slot
   (parsed-chunk
     :accessor parsed-chunk-of
     :init-value #f)
   ))


(define-method initialize ((self <cgi-form>) initargs)
  (next-method)
  ;; ���ΤȤ����̵��
  ;; (���λ����Ǥ�parsed-chunk���������ʤ����ٱ����������)
  )


;;; --------


(define-method expand-to-form-html ((self <cgi-form>) . extra-keywords)
  (error "sorry, not implemented yet"))

(define-method expand-to-confirm-html ((self <cgi-form>) . extra-keywords)
  (error "sorry, not implemented yet"))



;;; --------


(provide "tir03/cgi/form")
