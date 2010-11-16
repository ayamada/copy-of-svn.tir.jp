;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; cgi���form�򰷤��䤹������٤Υ��ץ��벽���饹

;;; ����:
;;; TODO: ���Ȥǽ�


;;; TODO: XML�ѡ��������Ѥ��ʤ����ˤ��롣
;;;       ����ˡ�text.html-lite��html:form��Ϣ�򶯲�������
;;;       cgi:form�Τ褦�����ؼ�³�����󶡤���
;;;       �����ȤäƤ�餦�褦�ˤ���
;;;       (�����μ�³���ϡ�cgi�ѥ�᡼���䤽�ν���������θ����
;;;        html���Ǥ��褦�ˤʤ�)
;;; TODO: form���󥹥��󥹤ϡ����줾���ȼ��˥������ޥ������줿���饹�ˤʤ롩
;;;       ����åȤ��ͤ��������ͤ�����硢��������ʬ����䤹����
;;;       ����Ȥ�ʬ����ˤ�����


;;; TODO: �����tir04�Ȥ�̵�ط��ˤ��롩
;;;       �⤦�����ͤ���ɬ�פϤ��롣



;;; ɬ�פʥѥ�᡼���ϡ�
;;; - �ե�������html��(����Ϥ����³��)(ɬ��)
;;; - confirm��html��(����Ϥ����³��)(optional)
;;; - �����ͤ򸡾ڤ���٤μ�³��(optional�����¼���ɬ��)
;;; - �����ͤ���(���������)����٤μ�³��(optional)
;;; - �����ͤν����(optional?)
;;; - ¾�ˤϡ�

;;; ɬ�פʤ�Τϡ�
;;; - form���󥹥��󥹤�form������method
;;; - form���󥹥��󥹤�confirm������method
;;; - form���󥹥��󥹤�cgi�ѥ�᡼�����Ϥ�method
;;; - form���󥹥��󥹤���cgi�ѥ�᡼������Ф�method
;;; - cgi�ѥ�᡼����ޤ�form���󥹥��󥹤θ��ڼ�³����¹Ԥ���
;;;   ���顼����(�ޤ���#f)����Ф�method
;;; - ���ڼ�³��/������³����ǻȤ��٤Υ桼�ƥ���ƥ���³����
;;; -- ǯ���������ϥե�������ʬ�����褯�Ȥ���ѥ������������������³����
;;; - cgi�ѥ�᡼����ޤ�form���󥹥��󥹤����ͤ�hidden������method
;;;   (confirm���Υܥ�����)
;;;   (��ǽ�ʤ顢�⤦�����ɤ����󥿡��ե�������ͤ�����)

;;; ����¾�λ���:
;;; - ���ץ���ʥ�����ǡ����󥹥�������ͤ򥪡��С��饤�ɤǤ���Ȥ��������
;;; - �ե����ಽ����ݤˡ�hidden�ѥ�᡼���ϡ�
;;;   </form>��ľ������������褦�ˤ�����
;;; - form���󥹥��󥹤ϥ������󲽤Ǥ���ɬ�פ�����
;;;   (�פ���ˡ�ʣ���Υڡ����ˤޤ��������Ϲ��ܤ��Ϥ���ɬ�פ�����)
;;;   ���μ�����ͤ���ɬ�פ����롪
;;; -- ����ץ�ˡ�ʣ����form���󥹥��󥹤�Ȥ��Τ����ִ�ñ��
;;;    â����������ˡ���Ȱ����Υڡ��������Ϥ����ѥ�᡼����
;;;    ����Ū��hidden�Ǵޤޤ���ɬ�פ����ꡢ
;;;    �����դ�򤤤�������ư�ǻ��ꤷ�Ƥ����ΤϾ������ݤ����Τ�ʤ���

;;; ���;��������:
;;; - TL��setForm������ɬ�פ������ɤ��¸����롩
;;;   �����ܤ�XML�Ȥ��ƥѡ������������ܤ˹�����Ԥ�����
;;;   ���Ѥ��������ġĤ�뤷��̵���褦����
;;; -- �񤭴�����ɬ�פˤʤ�Τϡ��ʲ��Υ�����
;;; --- <form ...> �Ρ�action
;;; --- <input type="text" ... /> �Ρ�value
;;; --- <input type="password" ... /> �Ρ�value
;;; --- <input type="hidden" ... /> �Ρ�value
;;; --- <input type="submit" ... /> �Ρ�value
;;; --- <input type="radio" ... /> �Ρ�checked
;;; --- <input type="checkbox" ... /> �Ρ�checked
;;; --- <textarea ...>...</textarea> �Ρ��ƥ�������ʬ
;;; --- <option ...>...</option> �Ρ�selected

#|
<cgi-form>�ϡ�
|#

(define-module tir04.cgi.form
  (use srfi-1)
  (use srfi-13)
  (use text.html-lite)
  (use www.cgi)
  (use tir04.cgi.util)

  (export
    <cgi-form>
    <cgi-form/params>
    <cgi-form-dispatch>
    ))
(select-module tir04.cgi.form)


(define-class <cgi-form> ()
  (
   ;; �ե�����html�Υ�����(ʸ���� / text-tree / ������³��)����¸����
   ;; (�ºݤˤϡ����󥹥�����������XML�ѡ���������Τ����Τ����Ѥ���)
   (html-form
     :accessor html-form-of
     :init-keyword :html-form
     :init-value #f)

   ;; confirm�Ѥ�html�Υ���������¸����(optional)
   ;; (<span id="...">...</span>�����Ǥ�񤭴�����褦�ˤ���)
   (html-confirm
     :accessor html-confirm-of
     :init-keyword :html-confirm
     :init-value #f)

   ;; cgi.dispatch���˥ǥ����ѥå�����٤�Ƚ����list����¸���롣
   ;; ���Υ��󥹥��󥹤ǥǥ����ѥå����ʤ����ϡ�#f�ǹ���ʤ���
   (dispatch-alist
     :accessor dispatch-alist-of
     :init-keyword :dispatch-alist
     :init-value #f)
   ;; dispatch-alist�ǥǥ����ѥå���������֤����٤��͡�
   ;; (�����ɤ��Ȥ�����̤��)
   (dispatch-result
     :accessor dispatch-result-of
     :init-keyword :dispatch-result
     :init-value #f)

   ;; �ʲ��ϡ���������å�
   (parsed-html-form
     :accessor parsed-html-form-of
     :init-value #f)
   (parsed-html-confirm
     :accessor parsed-html-confirm-of
     :init-value #f)
   ))


(define-method initialize ((self <cgi-form>) initargs)
  (next-method)
  ;; TODO: parsed-html-form parsed-html-confirm �����������
  )

(define-class <cgi-form/params> ()
  (
   ))

(define-method hoge ((self <cgi-form>) . args)
  #f)


(provide "tir04/cgi/form")
