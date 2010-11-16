#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ToDo: ����åȤ򸫤ơ�������ǽ�������Ȥ�ʤ��ʤ顢
;;;       �����Υ������󥯤ϽФ��ʤ��褦�ˤ���



(define-module cgi-client
  (use gauche.parameter)
  (use srfi-2) ; and-let*
  (use rfc.uri)
  (use text.html-lite)
  (use text.tree)
  (use util.list)
  (use www.cgi)
  (use qdbm)

  (use tir.cgi)
  (use tir.cgi-framework-lite)
  (use tir.sexp-cs)
  (use tir.dbmwl)

  (export
    <cgi-client>
    cgi-client-main
    ))
(select-module cgi-client)


(define-class <cgi-client> ()
  (
   (title
     :accessor title-of
     :init-keyword :title
     :init-value "cgi client")
   (css-url
     :accessor css-url-of
     :init-keyword :css-url
     :init-value "http://css.tir.jp/tir.css")
   (body-class
     :accessor body-class-of
     :init-keyword :body-class
     :init-value "wireframe01")
   (mail-address
     :accessor mail-address-of
     :init-keyword :mail-address
     :init-value "nekoie-cgi-client@tir.jp")

   (socket-spec
     :accessor socket-spec-of
     :init-keyword :socket-spec
     ;:init-value '()
     :init-value '(tcp "127.0.0.1" 12345 :reuse-addr? #t)
     )
   (session-cookie-key
     :accessor session-cookie-key-of
     :init-keyword :session-cookie-key
     :init-value "gs-common")
   (loginid-cookie-key
     :accessor loginid-cookie-key-of
     :init-keyword :loginid-cookie-key
     :init-value "gs-loginid")
   (description-text
     :accessor description-text-of
     :init-keyword :description-text
     :init-value #f)
   (session-dbm-path
     :accessor session-dbm-path-of
     :init-keyword :session-dbm-path
     :init-value "./session-dbm")
   (account-dbm-path
     :accessor account-dbm-path-of
     :init-keyword :account-dbm-path
     :init-value "./account-dbm")

   ;; �ץ饤�١��ȥ���å�
   (sexp-client
     :accessor sexp-client-of
     :init-value #f)
   (cgi-framework
     :accessor cgi-framework-of
     :init-value #f)
   (session-dbm
     :accessor session-dbm-of
     :init-value #f)
   (account-dbm
     :accessor account-dbm-of
     :init-value #f)
   ))

(define-method initialize ((self <cgi-client>) initargs)
  (next-method)
  (unless (session-dbm-path-of self)
    (error "this class must be need to :session-dbm-path"))
  (unless (account-dbm-path-of self)
    (error "this class must be need to :account-dbm-path"))
  ;; dbm���Ѱդ���
  (set!
    (session-dbm-of self)
    (make
      <dbmwl>
      ;;
      :dbm-path (session-dbm-path-of self)
      ))
  (set!
    (account-dbm-of self)
    (make
      <dbmwl>
      ;;
      :dbm-path (account-dbm-path-of self)
      ))
  )


;; ----


(define now-url self-url/path)

(define (self-url/param param-alist)
  (make-url-from-form-params (self-url) param-alist))

(define (now-url/param param-alist)
  (make-url-from-form-params (now-url) param-alist))


;; ----


(define (ss-ping self)
  #t)


(define (ss-connect self)
  (let1 sexp-client (make
                      <sexp-client>
                      :socket-spec (socket-spec-of self)
                      )
    (set! (sexp-client-of self) sexp-client)
    ;; �����ǡ����餫���̿��ƥ��Ȥ�Ԥ�ɬ�פ����뤫���Τ�ʤ�
    #t))


(define (ss-version-check self)
  #t)


(define *default-description-text*
  (string-append
    "���Ȥǡ�����ʸ��񤭤ޤ�"
    ""
    ""
    ""
    ""
    ))


(define (get-session-obj self)
  (if (not (session-dbm-path-of self))
    #f
    (begin
      ;; �ޤ���cookie�������ʸ������������
      (and-let* ((cookie-str (cgi-get-parameter
                               (session-cookie-key-of self)
                               (cgi-parse-parameters
                                 :query-string ""
                                 :merge-cookies #t))))
        ;; �ޤ���������
        #f
        ))))

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

(define (html-href label param)
  (html:a
    :href (now-url/param param)
    :target "_self"
    (hes label)))


(define (response:main self p c session-obj)
  (define (link-login/out)
    (apply
      html-href
      (if session-obj
        (list "logout" '(("cc" "logout")))
        (list "login (���Υ����Ȥ�����)" '(("cc" "login_form"))))))

  (make-page
    self
    (list
      (if (session-dbm-path-of self)
        (list
          (html:address (link-login/out))
          (html:hr))
        '())
      ;; ���������Τ��
      (html:p (hes "�ޤ�������Ǥ�"))
      (if p
        (html:div
          (html:p
            (hes
              "���ߡ���"
              p
              "�פˤ��ޤ�")))
        '())
      ;; �ޤ�
      ;; - GET/POST�ˤ��ե�����ѥ�᡼����PATH_INFO����Ĥξ����Ʊ���˻���
      ;; -- GET�ϡ������Ѥ�̵�����Τߤˤ����(��������Ϥʤ�)
      ;; -- POST�ϡ������Ѥ�ͭ�����Τߤˤ����(�ºݤ�����Ԥ���ư����)
      ;; --- â���������Ѥ�ͭ�����Ǥ⡢�֤˳�ǧ���̤򶴤����ϡ�
      ;;     �Ǹ��submit�Τ�POST����Ȥ�������ޤǤ�GET/POST�����˻Ȥ�ʬ����
      ;; -- PATH_INFO�Ǥϡ�where(���־���Ū�ʤ��)����ꤹ��
      ;; 
      ;; - PATH_INFO��̵�����ϡ����å�����̵ͭ�ǵ�ư���ѹ�����
      ;; -- ���å����̵���λ��ϡ������ȥ��ɽ��
      ;; -- ���å����ͭ��λ��ϡ����߰��֤ؤȥ�����쥯��
      ;; --- �ޤ��ϡ֡����ϡߡߤ˵�ޤ��פȤ��ä���å�������href
      ;; ����:
      ;; - ��������󤬤���ʤ顢����������¹�
      ;; -- sexp���̿���Ԥ�����̤�������
      ;; -- 
      ;; - ���߰��֤ξ����sexp�Ǽ���
      ;; -- 
      ;; - ���ߤμ�ʬ���Ȥξ����sexp�Ǽ���
      ;; -- 
      ;; - ����줿��̤������󥰤���
      ;; -- Ŭ����
      ;; ����¾:
      ;; - ����������ɽ���Τ߲�ǽ�ʥ���������ԲġˤȤ��뤬��
      ;;   �����ϸ�󤷤ˤ����
      ;; - ����ܥ�å����Ū�ʤ�Τ��Ѱդ���
      ;; -- /self/�ϡ���ʬ���Ȥ򺹤����������ϥ����ǹԤ�
      ;; --- /self/��ľ�ܺ������ˡ�proc�ߤ������������롩
      ;; --- �����դ�plan9�򸫤Ƴ�ǧ�������Ȥ���
      ;; - ���������¹Ը塢��󡢼�ʬ���ȤΥ��ơ�������������ơ�
      ;;   �Ҷ���ɽ��������褦�ˤ�����

      ;; �ʲ��ϡ�����ʤ顢�������ˤΤ�ɽ������
      (html:form
        :action (now-url)
        :method "post"
        :target "_self"
        :style "text-align:center"
        "# "
        (html:input
          :type "hidden"
          :name "c"
          :value "console_cmd")
        (html:input
          :style "width:75%"
          :type "text"
          :name "console_cmd"
          :value "")
        )
      )))


(define (make-form url hidden-alist . htmls)
  (html:form
    :action url
    :method "post"
    :target "_self"
    (map
      (lambda (x)
        (html:input
          :type "hidden"
          :name (car x)
          :value (cdr x)))
      hidden-alist)
    htmls))


(define (response:login-form self params . errors)
  ;; �ʲ��λ��Ĥ�ɽ������
  ;; - ������ե�����
  ;; - �����󥨥顼��å�����(���顼���Τ�)
  ;; - ����ʸ
  ;; - ��Ͽ�ե�����ؤΥ��
  ;; ���å�������Ǹ�Υ�����id����������ޤ��
  ;; â����������id�������ͤ�����ʤ顢�������ͥ�褹���
  (define (html:error-message)
    (if (null? errors)
      '()
      (html:div
        :class "error"
        (html:ul
          (map
            (lambda (x)
              (html:li
                (hes x)))
            errors)))))
  (define (html:login-form)
    (make-form
      (now-url)
      '(("cc" . "login"))
      (hes "login id : ")
      (html:input
        :type "text"
        :name "login_id"
        :value "") ;; ToDo: ���ȤǸ��ߤ��ͤ�ȿ�Ǥ����
      (html:br)
      (hes "login password : ")
      (html:input
        :type "password"
        :name "password"
        :value "")
      (html:br)
      (html:input
        :type "submit"
        :value "������")
      ))
  (define (html:description)
    ;; ToDo: ��ñ�ʡ��ƥ����Ȣ�html�Ѵ��ؿ����Ѱդ�������˳ݤ����
    (let1 description (or
                        (description-text-of self)
                        *default-description-text*)
      (html:pre
        (hes description))))
  (define (html:link-regist)
    (html:ul
      (html:li
        (html-href "������Ͽ" '(("cc" "register"))))))

  (make-page
    self
    (list
      (html:error-message)
      (html:login-form)
      (html:hr)
      (html:description)
      (html:hr)
      (html:link-regist)
      )))

(define (response:login self params)
  ;; �ѥ�᡼���򸡾ڤ�����������аʲ��ε�ư��Ԥ�
  ;; - ��������֤Ȥ���
  ;; - ���å���󥯥å����ȥ�����id�����ѥ��å���������ȯ��
  ;; - response:logined�ؤȥ�����쥯��
  ;; �������ʤ���С��֤ɤΤ褦�����꤬����Τ��פΥ�å������ȶ��ˡ�
  ;; response:login-form�˺����᤹
  (let* (
         (login_id (cgi-get-parameter "login_id" params))
         (password (cgi-get-parameter "password" params))
         ;;;;
         )
    ;; �����������Ԥ�
    ;;;; �ޤ�
    ;; (ǧ�ڤ����������ʤ�)
    ;; ���å���󥯥å������֤��Ĥġ�������λ���̤ؤȥ�����쥯��
    ;;;; ���ѥ⥸�塼�����褦�ˤ����
    (cgi-header
      :location (now-url/param '(("cc" "logined"))))))

(define (response:logined self params)
  ;; �ޤ���äƤʤ�
  (make-page
    self
    (list
      (html:p (hes "�ޤ�������Ǥ�"))
      (html:ul
        (html:li
          (html:a
            :href (now-url)
            (hes "���")))
        ))))

(define (response:logout self params)
  ;; �ޤ���äƤʤ�
  (make-page
    self
    (html:p (hes "�ޤ�������Ǥ�"))))

(define (response:logouted self params)
  ;; �ޤ���äƤʤ�
  (make-page
    self
    (list
      (html:p (hes "�ޤ�������Ǥ�"))
      (html:ul
        (html:li
          (html:a
            :href (now-url)
            (hes "���")))
        ))))

(define (response:register self params)
  ;; �ޤ���äƤʤ�
  (make-page
    self
    (list
      (html:p (hes "���ϡ��Ȥ��Ƥ��ʤ�login id�ȥѥ���ɤ����Ϥ������"
                   "��Ͽ��ǽ�Ǥ�"))
      (html:ul
        (html:li
          (html:a
            :href (now-url/param '(("cc" "login_form")))
            (hes "���")))
        ))))


(define (ss-main self params)
  (let (
        (p (cgi-get-metavariable "PATH_INFO")) ; ɽ���������åȻ����
        (c (cgi-get-parameter "c" params)) ; ���Τλؼ���
        (cc (cgi-get-parameter "cc" params)) ; ������/����������
        (session-obj (get-session-obj self))
        )
    (define (go)
      (response:main self p c session-obj))

    ;; ToDo: ���Ȥ�table������
    ;; ToDo: table��������Τ򡢹����̴ؿ�������¾�Ǥ�Ȥ���褦�ˤ���
    (if (not cc)
      (go)
      (if session-obj
        (cond
          ((string=? "logined" cc) (response:logined self params))
          ((string=? "logout" cc) (response:logout self params))
          (else (go)))
        (cond
          ((string=? "login_form" cc) (response:login-form self params))
          ((string=? "login" cc) (response:login self params))
          ((string=? "logouted" cc) (response:logouted self params))
          ((string=? "register" cc) (response:register self params))
          (else (go)))))))


(define-method cgi-client-main ((self <cgi-client>))
  (let1 cgi-framework (cgi-framework-of self)
    (if (not cgi-framework)
      (begin
        (set!
          (cgi-framework-of self)
          (make
            <cgi-framework-lite>
            :title (title-of self)
            :css-url (css-url-of self)
            :body-class (body-class-of self)
            :mail-address (mail-address-of self)
            :initialize-thunk (lambda ()
                                (if (sexp-client-of self)
                                  (or
                                    (ss-ping self)
                                    (error "connection disconnected"))
                                  (and
                                    (or
                                      (ss-connect self)
                                      (error "cannot connect to server"))
                                    (or
                                      (ss-version-check self)
                                      (error "mismatch protocol version"))
                                    )))
            :main-proc (lambda (params)
                         (ss-main self params))
            ))
        (cgi-client-main self))
      (cgi-framework-lite-main cgi-framework))))


(provide "cgi-client")


