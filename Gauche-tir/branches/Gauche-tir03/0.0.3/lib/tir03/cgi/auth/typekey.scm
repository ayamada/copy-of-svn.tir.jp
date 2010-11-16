;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(define-module tir03.cgi.auth.typekey
  (extend tir03.cgi.auth)
  (use gauche.charconv)
  (use gauche.parameter)

  (use srfi-13)

  (use file.util)

  (use text.html-lite)
  (use text.tree)
  (use www.cgi)
  (use util.digest)

  (use tir03.cgi)

  (use typekey)

  (export
    <cgi-auth-typekey>
    get-description-html
    cgi-auth:dispatch-login
    cgi-auth:dispatch-logout
    cgi-auth:dispatch-logined
    cgi-auth:dispatch-logouted
    ))
(select-module tir03.cgi.auth.typekey)


;;; --------


(define-class <cgi-auth-typekey> (<cgi-auth>)
  (
   ;; typekey����
   (typekey-token
     :accessor typekey-token-of
     :init-keyword :typekey-token
     :init-value #f)

   ;; internal slot
   (typekey-manager
     :accessor typekey-manager-of
     :init-value #f)
   ))


(define-method initialize ((self <cgi-auth-typekey>) initargs)
  (next-method)
  ;; check :typekey-token
  (unless (typekey-token-of self)
    (errorf "~a must be need to :typekey-token" (class-name (class-of self))))
  ;; make typekey-manager
  (set!
    (typekey-manager-of self)
    (make
      <typekey>
      :token (typekey-token-of self)
      :key-cache-path (string-append (storage-dir-of self) "/regkeys.txt")
      ))
  )


;;; --------


(define-method get-description-html ((self <cgi-auth-typekey>))
  (list
    (html:h1 "TypeKeyǧ��")
    (html:p
      "���Υ����ӥ������Ѥ���ˤϡ�TypeKeyǧ�ڤ�ɬ�פǤ���"
      (html:br)
      "������ܥ���򲡤���TypeKeyǧ�ڤ�ԤäƲ�������"
      (html:br)
      "(��������֤ϡ��֥饦�����Ĥ���Ȳ������ޤ�)"
      )
    (html:p
      "TypeKey�ˤĤ��Ƥϡ������Υ����Ȥ��ǧ���Ʋ�������"
      (html:br)
      "�ޤ���TypeKey��������Ȥμ����������ѹ����⡢"
      "���������Ȥ���Ԥ��ޤ���")
    (html:ul
      (html:li
        (html:a
          :href "http://www.sixapart.jp/typekey/"
          :target "_blank"
          "http://www.sixapart.jp/typekey/")))
    (html:hr)
    (html:p
      "��������ˤϡ�"
      "cookie��ͭ���ˤʤäƤ���ɬ�פ�����ޤ���"
      (html:br)
      "������ܥ���򲡤��Ƥ������Ǥ��ʤ����ϡ�"
      "�֥饦����cookie�����ľ���Ʋ�������"
      (html:br)
      "�ޤ���������֡�����������̵���ä����⡢��ưŪ��"
      "�������Ȥ���ޤ��Τǡ��ƥ����󤷤Ʋ�������"
      )))


;;; --------


(define (redirect url)
  (write-tree
    (cgi-header
      :pragma "no-cache"
      :cache-control "no-cache"
      :location url)))

(define-method cgi-auth:dispatch-login ((self <cgi-auth-typekey>) params)
  ;; typekey�ؤȥ�����쥯�Ȥ�Ԥ���
  (redirect
    (get-login-url
      (typekey-manager-of self)
      (string-append
        (self-url/path-info)
        "?"
        (cgi-auth-dispatch-key) "=" "logined"
        ))))

(define-method cgi-auth:dispatch-logined ((self <cgi-auth-typekey>) params)
  ;; typekey�ѥ�᡼����verify��Ԥ���session����¸����
  ;; cookie��ȯ�Ԥ��Ĥ�redirect���롣
  (define (get-auth-info)
    (define (reader key)
      (cgi-get-parameter key params))
    (define (utf-8->native str)
      (with-error-handler
        (lambda (e) #f)
        (lambda ()
          (ces-convert str "utf-8"))))
    (and-let* (
               (email (reader "email"))
               (name (reader "name"))
               (nick (reader "nick"))
               (ts (reader "ts"))
               (sig (reader "sig"))
               )
      (and
        (verify (typekey-manager-of self) email name nick ts sig)
        (list
          :unique-id (string-append "typekey:" (digest-hexify name))
          :auth-type 'typekey
          :email (utf-8->native email)
          :name (utf-8->native name)
          :nick (utf-8->native nick)
          :ts (x->number ts)
          :sig (string-map
                 (lambda (c)
                   (if (eqv? c #\space)
                     #\+
                     c))
                 sig)))))
  (let1 typekey-auth-info (get-auth-info)
    (if (not typekey-auth-info)
      (cgi-auth:dispatch-login self params) ; �ѥ�᡼���۾����ǧ�ڤ�����
      (let1 sid (cgi-auth-info-save! self typekey-auth-info)
        ;;;; ToDo: �Ť�sid���ݻ����Ƥ���ʤ顢��˾õ��Ԥ��٤���
        (redirect/cookie self sid)))))

(define-method cgi-auth:dispatch-logout ((self <cgi-auth-typekey>) params)
  ;; typekey�ؤȥ�����쥯�Ȥ�Ԥ���
  (redirect
    (get-logout-url
      (typekey-manager-of self)
      (string-append
        (self-url/path-info)
        "?"
        (cgi-auth-dispatch-key) "=" "logouted"
        ))))

(define-method cgi-auth:dispatch-logouted ((self <cgi-auth-typekey>) params)
  ;; typekey�Υ������Ƚ���������ä���ΤȤ��ơ�
  ;; session�����������cookie��ȯ�Ԥ��Ĥ�redirect���롣
  ;;;; ToDo: ���Ȥǡ�cookie��sid����äƤ���ʤ顢session����������褦��ľ��
  ;;;;       (�����餯���������ƥ��塢���ס�)
  ;(cgi-auth-info-delete! self sid)
  (redirect/cookie self "logouted"))


;;; --------


(provide "tir03/cgi/auth/typekey")

