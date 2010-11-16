#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; usage:
;;; - CGI/1.1�Ķ�������
;;; - initialize-thunk����åȤϡ�̾����ȿ���ơ�main-thunk�¹��������ɬ��
;;;   �¹Ԥ���롣�����˰��Τ߼¹Ԥ������褦��ʪ�ϡ�cgi-framework-main
;;;   �¹����˹ԤäƤ�������delay/force��Ȥ�����
;;; -- ����initialize-thunk���˥��顼���Ф���硢���ƥʥ󥹲��̤�ɽ�������
;;; - ���Ȥǽ񤭤ޤ�

;;; spec:
;;; - �ǥ����ѥå���form�ѥ�᡼����path-info�ѥ�᡼����cookie�ѥ�᡼����
;;;   session�ѥ�᡼������ڤ����Ǥ���褦�ˤ���
;;; -- �����Υѥ�᡼����parameterize�ѿ���ͳ�ǥ�����������
;;; - form�ѥ�᡼�������ܸ쥳���ɼ�ư�Ѵ�
;;; - �ºݤ˼¹Ԥ���thunk�ϡ��ʲ��Τɤ줫���ͤ��֤����ǡ�
;;;   HTTP���饤����Ȥ��Ф�������򥫥����ޥ���������������
;;; -- HTTP�쥹�ݥ󥹤�text.tree���Τ��(�����̤��ư��)
;;; -- (values :redirect url) ; ������쥯�Ȥ���
;;; -- (values :html-body body) ; body�γ���Ŭ����xhtml���Ȥ����Ϥ����
;;; -- (values :session session-alist :html-body body) ; ���å����ȯ��
;;; -- (values :session session-alist :redirect url) ; ���å����ȯ��
;;; -- (values :direct-output #t) ; ����ľ��stdout�˽��Ϥ��줿�Τǲ��⤷�ʤ�
;;; -- (values :cookie cookie :html-body body) ; ���å����Ȥ鷺ľ��cookie����


;;; note: parameterize������Ū�˻Ȥäơ���¦�����Ǥ⥹�å��ꤷ�������ˤ���


;;'((title "(untitled)")
;;  (css-url "http://css.tir.jp/tir.css")
;;  (body-class #f)
;;  )


(define-module tir.cgi-framework
  (use gauche.parameter)
  (use srfi-2) ; and-let*
  (use rfc.uri)
  (use text.html-lite)
  (use text.tree)
  (use util.list)
  (use www.cgi)

  (use tir.cgi)

  (export
    <cgi-framework>
    cgi-framework-main
    cgi-form-param
    cgi-path-info
    cgi-cookie-param
    cgi-session-param
    ))
(select-module tir.cgi-framework)


(define-class <cgi-framework> ()
  (
   ;; ��ư���褵���html������ʬ������
   (html-settings
     :accessor html-settings-of
     :init-keyword :html-settings
     :init-value '())
   ;; ���顼����������᡼�륢�ɥ쥹
   (contact-mail-address
     :accessor contact-mail-address-of
     :init-keyword :contact-mail-address
     :init-value #f)

   (initialize-thunk
     :accessor initialize-thunk-of
     :init-keyword :initialize-thunk
     :init-value #f)
   (main-thunk
     :accessor main-thunk-of
     :init-keyword :main-thunk
     :init-value #f)
   (maintenance-page-thunk
     :accessor maintenance-page-thunk-of
     :init-keyword :maintenance-page-thunk
     :init-value #f)
   (error-page-thunk
     :accessor error-page-thunk-of
     :init-keyword :error-page-thunk
     :init-value #f)

   ;; �ʲ��ϡ������ѥ���å�
   ))

(define-method initialize ((self <cgi-framework-lite>) initargs)
  (next-method)
  (unless (main-thunk-of self)
    (error "this class must be need to :main-thunk"))
  )


;; ----


(define (make-page self body)
  (list
    (cgi-header)
    (html-tree-make
      :encoding "EUC-JP"
      :title (title-of self)
      :css-url (css-url-of self)
      :body-class (body-class-of self)
      :body (html:div
              body
              (html:hr)
              (html:address (hes (title-of self)))
              ))))





(define (get-default-error-page self)
  (make-page
    self
    (list
      (html:p
        (hes "sorry, this service was temporarily unavailable now.")
        (html:br)
        (hes "please access later."))
      (html:p
        (hes "�������ʤ��������ߡ����Ū�˥����ӥ�����ߤ��Ƥ��롣")
        (html:br)
        (hes "�ޤ���ǥ����������Ƥ��졣"))
      (let1 e (mail-address-of self)
        (if e
          (list
            (html:hr)
            (html:p
              (html:small
                (hes "(")
                (hes "�⤷����ʬ��Ĺ���֥����ӥ�����ߤ��Ƥ���ΤǤ���С�")
                (hes "���Υܥ���򲡤����ˤ�äơ�")
                (hes "�����ӥ������Ԥ˺�¥���������ǽ")
                (hes ")")))
            (html:form
              :action (self-url)
              :method "post"
              :target "_blank"
              :style "text-align:center"
              (html:input
                :type "hidden"
                :name "error_c"
                :value "send")
              ;(hes "Ϣ���å����� : ")
              ;(html:input
              ;  :type "input"
              ;  :name "error_msg"
              ;  :value "")
              (html:input
                :type "submit"
                :value "��¥����")
              ))
          '())))))

(define (get-sended-page self bool)
  (make-page
    self
    (html:h1
      (hes
        (if bool
          "* ��¥���ޤ��� *"
          "* ��¥�Բ�ǽ *")))))

(define (send-mail to header-alist body)
  ;; ���Ȥ�
  #f)


;;;;

(define-method cgi-framework-main ((self <cgi-framework>))
  (define (do-initialize)
    (let1 initializer (initialize-thunk-of self)
      (when initializer
        (with-error-handler
          (lambda (e)
            (report-error e)
            #f)
          (lambda ()
            (initializer)
            #t)))))
  (define (do-main)
    (cgi-main
      (lambda (params)
        ((main-proc-of self) params))))
  (define (do-error)
    (cgi-main
      (lambda (params)
        (define (r key)
          (cgi-get-parameter key params))

        (let (
              (error_c (r "error_c"))
              (error_msg (r "error_msg"))
              )
          (if (equal? error_c "send")
            (let1 email (mail-address-of self)
              (get-sended-page
                self
                (and
                  email
                  (send-mail
                    email
                    '()
                    (or error_msg "���顼����/��¥���Ԥ��ޤ���")))))
            (let1 ept (error-page-thunk-of self)
              (if ept
                (list
                  (cgi-header :pragma "no-cache")
                  (ept))
                (get-default-error-page self))))))))

  (if (do-initialize)
    (do-main)
    (do-error)))


(provide "tir/cgi-framework")


