;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; html��form�ǥ����󤷡�dbm�˥�������Ⱦ�����ݻ����롢
;;; �ʰ���Ͽ/ǧ�ڤ�Ԥ����饹

;;; TODO: ���Ȥ�email��������¾�ξ����Ʊ������Ͽ/�ݻ��Ǥ���褦��
;;;       ��ĥ��������θ���ƺ���

;;; TODO: �ѥ�����ѹ���ǽ����������Ⱥ����ǽ��ɬ�ס�
;;;       ������������Ⱥ����ǽ�ϡ�ñ�˺������ΤǤϤʤ���
;;;       ����ե饰��Ω�Ƥ�褦�ˤ��ơ�
;;;       ���塢Ʊ��id�Υ�������Ȥ�����ʤ��褦�ˤ���ɬ�פ����롣

;;; TODO: ������塢���å����Ǥ��Ф餯�ͤ򲱤��������åѡ����̤˺��
;;; TODO: ��������̤�JavaScript���ɲ�

;;; ����:
;;; ���Υ⥸�塼��ϡ�form�ˤ��桼�����̤�Ԥ��٤Υ��饹���󶡤��롣
;;; �桼�����̤ΰ٤ξ���ϻ��ꤵ�줿dbm�ե�����˵�Ͽ����롣
;;; ���Υ��饹�ϡ��桼���μ��̵�ǽ�Τߤ��󶡤���١����ӡ�
;;; ���å���󥯥饹���Ѱդ���ɬ�פ�����(tir04.cgi.session)��

#|
(define *cgi-ident*
  (make <cgi-ident-form>
        :internal-key-prefix "cgi-ident"
        :error-html-proc ...
        :error-html-keywords '(...)
        :dbm-type <qdbm>
        :dbm-path "/path/to/dbm"
        :rw-mode :write
        :html-keywords '(...)
        ))

(cgi-main
  (lambda (orig-params)
    (with-cgi-ident
      *cgi-ident*
      orig-params ; �����ͤˤ�äơ�with-cgi-ident�������򲣼�ꤹ�뤫����ޤ�
      (lambda (true-params ident-info)
        ;; true-params�ˤϡ�:internal-key-prefix�ǻϤޤ�key������줿
        ;; params�����롣
        ;; ǧ�ڤ��Ԥ�졢���줬�����������Τߡ�ident-info���ͤ����롣
        ;; ident-info��#f�ʳ����ä���硢�桼����������ܥ���򲡤���
        ;; with-cgi-ident������ꤷ���������ǧ�ڽ������Ԥ�졢������ǧ�ڤ��줿
        ;; �Ȥ������ʤΤǡ�ident-info���������params���Υ����å��������
        ;; �������ν����ؤȥǥ����ѥå����ʤ��ƤϤʤ�ʤ���
        ;; ident-info�ϰʲ��Τ褦��list�Ȥʤ롣
        ;; '(
        ;;   :ident-type 'form ; ����
        ;;   :ident-path "/path/to/dbm" ; :dbm-path���ͤ�����
        ;;   :ident-id "..." ; ��Ͽ���줿������ID(�Դ���ʸ����β�ǽ��ͭ��)
        ;;   :uid "..." ; "form:/path/to/dbm:������ID"��sha1 digest����hexify����ʸ����
        ;;   :x-ident-info '() ; ���ΤȤ���̤����
        ;;   )
        ;; ǧ�ڥڡ����ؤ�url��������
        (make-ident-url *cgi-ident* callback-params)
        ;; ǧ�ڥܥ�������
        (make-ident-form *cgi-ident* callback-params html-tree . keywords)
        ... ; �͡��ʽ�����Ԥ�
        ))))
|#


(define-module tir04.cgi.ident.form
  (use dbm)
  (use text.tree)
  (use text.html-lite)
  (use www.cgi)
  (use tir04.cgi.util)
  (extend tir04.cgi.ident)
  (use rfc.sha1)
  (use util.digest)
  (use tir04.dbm.util)

  (export
    <cgi-ident-form>
    with-cgi-ident
    make-ident-url
    make-ident-form
    ))
(select-module tir04.cgi.ident.form)



(define-class <cgi-ident-form> (<cgi-ident>)
  (
   (dbm-type
     :accessor dbm-type-of
     :init-keyword :dbm-type
     :init-value #f)
   (dbm-path
     :accessor dbm-path-of
     :init-keyword :dbm-path
     :init-value #f)
   (rw-mode ; :read or :write
     :accessor rw-mode-of
     :init-keyword :rw-mode
     :init-value :write)
   (html-keywords
     :accessor html-keywords-of
     :init-keyword :html-keywords
     :init-value '())
   ))


(define-method initialize ((self <cgi-ident-form>) initargs)
  (next-method)
  (unless (dbm-type-of self)
    (error ":dbm-type must be required"))
  (unless (dbm-path-of self)
    (error ":dbm-path must be required"))
  )

(define-method get-html-keywords ((self <cgi-ident-form>)
                                  keyword . opt-fallback)
  (apply get-keyword keyword (html-keywords-of self) opt-fallback))

;; <cgi-ident>�λҥ��饹���������٤�methods
(define-method dispatch-or-ident-info ((self <cgi-ident-form>) dispatch-val
                                                               cgi-params
                                                               true-params
                                                               ident-info-cont)
  ;; dispatch-val�ˤ�äƥǥ����ѥå���Ԥ�
  ;; �ʲ��Τ褦�����ܤˤʤ�Ȼפ��롣
  ;; - "login"�ʤ顢������ե��������Ͽ�ե�����ؤΥ�󥯤�Ф�
  ;; - "login-submit"�ʤ顢������ѥ�᡼���򸡾ڤ�������ʤ����
  ;;   dispatch-val̵��url�ؤȥ�����쥯���ᤷ
  ;;   (�ޥ���������б��ΰ٤ˡ����url�ϥ������ޥ�����ǽ�ˤ����)
  ;; - "register"�ʤ顢��Ͽ�ե������Ф�(:rw-mode��:write���Τ�)
  ;; - "register-confirm"�ʤ顢��Ͽ�ե�����γ�ǧ���̤�Ф�
  ;; - "register-submit"�ʤ顢��Ͽ�ѥ�᡼���򸡾ڤ���"registered"��
  ;; - "registered"�ʤ顢��Ͽ��λ��å������ȥ�����ե�����ؤΥ�󥯤�Ф�
  ;; -- ľ�ܥ�������֤ˤ������ͤ�������
  ;;    ��ǧ�ΰ�̣��ޤ�ơ�������ե�������᤹���ˤ���
  ;; - ����ʳ��ʤ顢"login"��Ʊ�������Ȥ���
  (let1 proc (cond
               ((equal? "login-submit" dispatch-val)
                form:login-submit)
               ((equal? "register" dispatch-val)
                form:register)
               ((equal? "register-confirm" dispatch-val)
                form:register-confirm)
               ((equal? "register-submit" dispatch-val)
                form:register-submit)
               ((equal? "registered" dispatch-val)
                form:registered)
               (else ; or "login"
                 form:login))
    (proc self cgi-params true-params ident-info-cont)))

(define (make-page self form-flag . body)
  ;; TODO: �������ޥ�����ǽ�ˤ����
  (cgi-tree-make
    :http-header '(:pragma "no-cache")
    :encoding (x->string (cgi-output-character-encoding))
    :css-url (get-html-keywords self :css-url #f)
    :robots "NOINDEX,NOFOLLOW"
    :js-body "function f(){document.f.l.focus();}"
    :title (get-html-keywords self :title #f)
    :body-header #f
    :body-footer (list
                   (html:hr)
                   (html:address
                     (html:a
                       :name "bottom"
                       (get-html-keywords self :title #f))))
    :body-attr (if form-flag
                 '(:onload "f()")
                 #f)
    :body body))

(define (make-error-page self . body)
  (make-page
    self
    #f
    (html:h1 "error")
    body))

(define (form:login self cgi-params true-params ident-info-cont)
  ;; TODO: �������ޥ�����ǽ�ˤ����
  ;; ������ե������Ф�
  ;; �ޤ���(rw-mode-of self)��:write�ʤ顢��Ͽ��󥯤�Ф�
  (make-page
    self
    #t
    (make-form
      (self-path/path-info)
      `((,(dispatch-key-of self) "login-submit")
        )
      (list
        (html:div
          (text->inline-html
            (tree->string
              (get-keyword :login-message (html-keywords-of self) '()))))
        (html:div
          (html:tt
            "login-id: "
            (html:input
              :style "ime-mode:disabled"
              :type "text"
              :name "login-id"
              :id "l"
              :value "")))
        (html:div
          (html:tt
            "password: "
            (html:input
              :type "password"
              :name "password"
              :value "")))
        (html:div
          (html:tt
            (html:input
              :type "submit"
              :value "login")))
        (if (not (eq? (rw-mode-of self) :write))
          '()
          (list
            (html:hr)
            (html:p
              (html:a
                :target "_self"
                :href (append-params-to-url
                        (self-url/path-info)
                        `((,(dispatch-key-of self) "register")))
                "������Ͽ")))))
      :name "f")))

(define (form:login-submit self cgi-params true-params ident-info-cont)
  (let ((login-id (cgi-get-parameter "login-id" cgi-params :default ""))
        (password (cgi-get-parameter "password" cgi-params :default ""))
        )
    (guard (e (else
                ;; TODO: ���Ȥǡ�����¾�Υ��顼�ˤ��б��Ǥ���褦�ˤ����
                (make-error-page
                  self
                  (html:div
                    "������id���ѥ���ɤ��ְ�äƤ��ޤ�"))))
      (let1 login-info (get-login-info self login-id) ; keywords or #f
        (unless login-info
          (error "login-info not found"))
        (let1 password-digest (password->digest password)
          (if (not
                (string=?
                  (get-keyword :password-digest login-info "")
                  password-digest))
            (error "password not matched")
            (ident-info-cont
              `(
                :ident-type 'form
                :ident-path (dbm-path-of self)
                :ident-id login-id
                :uid ,(digest-hexify
                        (sha1-digest-string
                          (string-join
                            (list
                              "form"
                              (dbm-path-of self)
                              login-id)
                            ":")))
                :x-ident-info '()))))))))

(define (form:register self cgi-params true-params ident-info-cont)
  (if (not (eq? (rw-mode-of self) :write))
    (make-error-page
      self
      (html:div "���ߡ���Ͽ����ߤ��Ƥ��ޤ�"))
    (make-page
      self
      #f
      (make-form
        (self-path/path-info)
        `((,(dispatch-key-of self) "register-confirm")
          )
        (list
          (html:div
            (het
              "login-id��password����Ƥ���������"
              "(���ȤǾܤ����������ͽ��)"
              ))
          (html:div
            (html:div
              (html:tt
                "login-id: "
                (html:input
                  :style "ime-mode:disabled"
                  :type "text"
                  :name "login-id"
                  :value "")))
            (html:div
              (html:tt
                "password: "
                (html:input
                  :type "password"
                  :name "password"
                  :value "")))
            (html:div
              (html:tt
                "password: "
                (html:input
                  :type "password"
                  :name "password"
                  :value "")
                "(��ǧ�ΰ١�Ʊ�����Ƥ�������Ϥ��Ƥ�������)"))
            (html:div
              (html:tt
                (html:input
                  :type "submit"
                  :value "��Ͽ")))))))))
(define (form:register-confirm self cgi-params true-params ident-info-cont)
  ;; TODO: ���ȤǼ���������ˤ���
  (form:register-submit self cgi-params true-params ident-info-cont))
(define (form:register-submit self cgi-params true-params ident-info-cont)
  (define (error-html . msg)
    (make-error-page self
                     (html:div (apply het msg))))

  (let ((login-id (cgi-get-parameter "login-id" cgi-params))
        (passwords (cgi-get-parameter "password" cgi-params :list #t))
        )
    ;; �ѥ�᡼�������å�
    (cond
      ((or
         (not login-id)
         (zero? (string-length login-id)))
       (error-html "login-id����ꤷ�Ƥ�������"))
      ((not (#/^\w+$/ login-id)) ; TODO: ���Ȥǥ������ޥ�����ǽ��
       (error-html
         "login-id�ˤ�Ⱦ�ѥ���ե��٥åȤȿ����ȥ�������С������Ȥ��ޤ���"))
      ((< 16 (string-size login-id)) ; TODO: ���Ȥǥ������ޥ�����ǽ��
       (error-html "login-id��Ĺ�᤮�ޤ�"))
      ((not (= 2 (length passwords)))
       (error-html "�ѥ�᡼���˰۾郎����ޤ�"))
      ((not (string=? (car passwords) (cadr passwords)))
       (error-html "��ĤΥѥ���ɤ����פ��ޤ���"))
      ((zero? (string-length (car passwords)))
       (error-html "�ѥ���ɤ���ꤷ�Ƥ�������"))
      (else
        (let1 password-digest (password->digest (car passwords))
          (guard (e (else
                      ;(report-error e)
                      (error-html
                        "����login-id�ϴ��ˡ�¾�����ˤ�ä���Ͽ����Ƥ��ޤ���"
                        "�̤�login-id����ꤷ�Ƥ�������")))
            (set-login-info!
              self
              login-id
              `(
                :password-digest ,password-digest
                ;; TODO: �ե������¾���ͤ⵭Ͽ������Ǥ���褦�ˤ����
                ;;       (�����������餫�Υ��󥿡��ե�������ͤ���ɬ��ͭ��)
                ))
            (location
              (append-params-to-url
                (self-url/path-info)
                `((,(dispatch-key-of self) "registered"))))))))))
(define (form:registered self cgi-params true-params ident-info-cont)
  (make-page
    self
    #f
    (html:p
      "��Ͽ���ޤ�����")
    (html:ul
      (html:li
        (html:a
          :href (append-params-to-url
                  (self-url/path-info)
                  `((,(dispatch-key-of self) "login")))
          "login")))))

(define (get-login-info self login-id)
  ;; keywords���֤�
  ;; �ǡ�����̵������#f���֤�
  (guard (e (else #f))
    (with-dbm-open
      (dbm-type-of self)
      :path (dbm-path-of self)
      :rw-mode :read
      :key-convert #f
      :value-convert #t
      (lambda (dbm)
        (dbm-get dbm login-id)))))

(define (set-login-info! self login-id login-info)
  ;; ������Ͽ����Ƥ������ϥ��顼�㳰���ꤲ���
  (with-dbm-open
    (dbm-type-of self)
    :path (dbm-path-of self)
    :rw-mode (rw-mode-of self)
    :key-convert #f
    :value-convert #t
    (lambda (dbm)
      (if (dbm-exists? dbm login-id)
        (error "login-id already exists")
        (dbm-put! dbm login-id login-info)))))


(define (password->digest password)
  (digest-hexify
    (sha1-digest-string password)))




(define-method get-remove-params-key-list ((self <cgi-ident-form>))
  '("login-id" "password"))

(define-method with-cgi-ident ((self <cgi-ident-form>) cgi-params proc)
  (next-method))

(define-method make-ident-url ((self <cgi-ident-form>) misc-params)
  (next-method))

(define-method make-ident-form ((self <cgi-ident-form>)
                               misc-params
                               html-tree . keywords)
  (next-method))


(provide "tir04/cgi/ident/form")
