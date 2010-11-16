;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; cgi�Ѥ�ǧ�ڥ����ƥ����

;;; ToDo: acl�Υ����å���lazy�˹Ԥ���褦�ˡ�proc����ꤹ��褦���ѹ��������
;;;       (���ߤλ��ͤǤϵ����acl������Ǥ��ʤ���)

;;; note: ���Υ⥸�塼��ϡ�dbm��Ʊ�ͤ���ݥ⥸�塼��Ǥ��롣

;;; note: tir02�Ȥϰ㤤��typekey���γ���ǧ�ڤ�Ȥ����Ǥ⡢
;;;       ��������쥯�Ȥ�typekey�����Ф��褦�ˤ��롣
;;;       (�Ĥޤꡢtypekey����ȤäƤ��Ƥ⡢��������url��
;;;        ���CGI������ץȼ��ȤΤ�Τˤʤ롣)
;;;       ����ˤ�äơ�������url����

;;; WARN: cgi-main�ϡ�with-auth-info����¦�ǸƤֻ���

;;; note: ���Υ⥸�塼�뤬����ꤹ��ѥ�᡼����ϡ�GET���ΤߤȤ��롣
;;;       POST���ˤ�ȿ�����ʤ�������ա�
;;;       �ޤ�����method��POST�Ǥʤ��׻�������ˡ�
;;;       POST���ˤ�ư��ʤ������ɤ��񤫤�Ƥ�����ˤ���դ��롣

;;; note:
;;; tir02�Ǥϡ�ǧ�ڤ����ɬ�פȤ�������ǥ�������Τߵ�ǽ����褦�˺�ä�����
;;; ����ϡ�cgi-auth-info�ѥ�᡼�������󶡤���⥸�塼��Ȥ��ƺ�롣

;;; �Ȥ���:
;;; - �ޤ���Ŭ�ڤ˥��󥹥��󥹤��������Ƥ�����
;;; - with-auth-info��Ǥϡ�cgi-auth-info�ѥ�᡼�����顢ǧ�ھ������Ф��롣
;;; -- cgi-auth-info��#f�λ��ϡ�̤ǧ�ھ��֡�
;;; - �����˻Ȥ��٤Υ桼�ƥ���ƥ��ؿ����󶡤��롣
;;; -- ���ǧ�ڤ�ɬ�פʥڡ����ΰ٤Ρ��ʰ�ǧ�ڲ��̴ؿ�
;;; -- ������/����������url/href/form�����ؿ�

;;; ����:
;;; cgi-auth-info�ϡ��ʲ������Ǥ����keywords�Ȥ��롣
;;; - :unique-id (�桼����ͭ��id)
;;;              (����ϡ���ͭ�ΥХ��ʥ�ǡ�����digest-hexify����ʸ����Ȥ���)
;;; - :auth-type (ǧ�ڵ����μ��̡��㤦ǧ�ڵ�����ʣ�����Ѳ�ǽ�ˤ����ݤˡ�
;;;               �����졢:unique-id��Ʊ��ˤʤäƤ��ޤ���ǽ��������Τ�)
;;; - ����¾�Υ������ (����¾�Ρ���ǧ�ڵ����ȼ�����)
;;; ����cgi-auth-info��list�ǤϤʤ�#f�ʤ顢̤��������֤Ǥ���Ȥ��롣

;;; ��¤:
;;; - ǧ�ھ���ϡ�����Ū�ˡ����Υ⥸�塼��γ��˻��Ļ��Ȥ���
;;;   (���Υ⥸�塼���storage-dir��ȤäƤ⤤����)
;;; - ǧ�ڤ�̵ͭ�ϡ�(session)cookie��ȤäƹԤ���
;;;   (�̾�cookie���ǽ)
;;; - cookie�Ǥϡ�sha1����ʸ�������Ȥꤹ�롣
;;; - �嵭�γ���ʸ����ˤϡ�cgi-auth-info���Τ��(keywords)���б����롣
;;; - cgi-auth-info��keywords��session-lite����¸���롣


(define-module tir03.cgi.auth
  (use gauche.parameter)

  (use file.util)

  (use rfc.cookie)
  (use text.tree)
  (use text.html-lite)
  (use www.cgi)

  (use tir03.cgi)
  (use tir02.session.lite)
  ;(autoload tir02.session.lite <session-lite>)

  (export
    cgi-auth-dispatch-key ; CGI�ѥ�᡼���Υ���ʸ����(unique�������ʤ���)
    cgi-auth-info ; parameterize���줿ǧ�ھ���(keywords)

    <cgi-auth>

    with-auth-info
    ;; usage: (with-auth-info <cgi-auth> thunk)
    ;; cgi�����򲣼�ꤷ��ɬ�פʤ顢ǧ�ڤ�Ԥ���

    must-required-auth-with

    ;; �ʲ��δؿ��ϡ����ץ���ʥ�����Ȥ���'login 'logout 'flip�Τɤ줫����
    ;; (�ǥե���ȤǤ�'flip)
    ;; CGI�ѥ�᡼���Ȥ���flip���ꤲ����Ϥ��ʤ�(���֥륯��å���������ȯ��)
    cgi-auth:make-url
    cgi-auth:make-html:a
    cgi-auth:make-html:form
    ;; �������ϡ�method�ǤϤʤ��ؿ����Ȥ���������ա�

    ;; �ʲ��ϡ���ݥ᥽�åɡ��ҥ��饹���������ʤ��ƤϤʤ�ʤ���
    get-description-html ; ǧ�������ν񤫤줿html-tree���֤��ؿ�
    cgi-auth:dispatch-login ; ����ɬ�ܡ�
    cgi-auth:dispatch-logout ; ����ɬ�ܡ�
    cgi-auth:dispatch-logined ; �Ȥ�ʤ��ʤ顢�������ʤ��Ƥ�褤
    cgi-auth:dispatch-logouted ; �Ȥ�ʤ��ʤ顢�������ʤ��Ƥ�褤

    ;; �ʲ��ϡ��ҥ��饹�����Ѥ���٤������ѥ᥽�åɡ�
    cgi-auth-info-save!
    cgi-auth-info-delete!
    ;; cgi-auth-info�򿷤������ꤷ������sid���֤���
    ;; ��ˡ��ҥ��饹��ǧ�ڷ�̤���¸����٤˻Ȥ���
    redirect/cookie ; cookie��ȯ�Ԥ��Ĥ�redirect���롣cgi-main����ǤϻȤ��ʤ�
    ))
(select-module tir03.cgi.auth)


;;; --------


(define cgi-auth-dispatch-key (make-parameter "_auth"))
(define cgi-auth-info (make-parameter #f))


;;; --------


(define-class <cgi-auth> ()
  (
   ;; access control list
   ;; #t�ʤ������ġ�#f�ʤ����Ѳ���list�ʤ�ʲ��ν񼰤�acl�Ȥ��Ƶ�ǽ����
   ;; '(("aaa" . #t) ; :unique-id��"aaa"�Υ�������Ȥϥ������
   ;;   ("bbb" . #t) ; :unique-id��"bbb"�Υ�������Ȥϥ������
   ;;   ("ccc" . #f) ; :unique-id��"ccc"�Υ�������Ȥϥ������Բ�
   ;;   )
   ;; ToDo: �⤦�����������Τ���񼰤ˤ��������ġġ�
   (acl
     :accessor acl-of
     :init-keyword :acl
     :init-value #t)
   ;; �嵭��acl�˴ޤޤ�ʤ���������ȤΥݥꥷ����
   (acl-fallback-policy
     :accessor acl-fallback-policy-of
     :init-keyword :acl-fallback-policy
     :init-value #f) ; #t��#f�Τߡ�#t�ʤ������ġ�#f�ʤ��Բġ�
   ;; �嵭��acl�ˤҤä����ä��桼���˸�����html
   (acl-drop-html
     :accessor acl-drop-html-of
     :init-keyword :acl-drop-html
     :init-form (list
                  (cgi-auth:make-html:form :mode 'logout)
                  (html:hr)
                  (html:p
                    "���Υ����ӥ��ϥץ饤�١��ȥ⡼�ɤ����ꤵ��Ƥ��ޤ���"
                    "���ꤵ�줿�桼���ʳ������ѤǤ��ޤ���"
                    )))
   ;; ���������html�˻Ȥ������귲
   (html-keywords
     :accessor html-keywords-of
     :init-keyword :html-keywords
     :init-value (list
                   ;; ���Ρ��ʲ����ͤ�ɬ�ס�
                   ;:encoding (symbol->string (gauche-character-encoding))
                   ;:base-url #f
                   ;:css-url #f
                   ;:css-body #f
                   ;:js-url #f
                   ;:js-body #f
                   ;:js #f
                   :robots "NOINDEX,NOFOLLOW"
                   :title "ǧ�ڤ��Ʋ�����"
                   ;:body-header #f
                   ;:body-footer #f
                   ))
   ;; �ե�������ϡ����Υǥ��쥯�ȥ�������������
   (storage-dir
     :accessor storage-dir-of
     :init-keyword :storage-dir
     :init-value #f)
   ;; �եå�(proc����ꤹ��)
   (login-hook
     :accessor login-hook-of
     :init-keyword :login-hook
     :init-value #f)
   (logout-hook
     :accessor logout-hook-of
     :init-keyword :logout-hook
     :init-value #f)
   ;; ���å�����Ϣ
   ;; ToDo: �⤦������꤯��åԥ󥰤��������ġġ�
   (session-maker
     :accessor session-maker-of
     :init-keyword :session-maker
     :init-form (lambda (self)
                  (make
                    <session-lite>
                    :dbm-path (string-append (storage-dir-of self) "/session")
                    )))

   ;; ���å���󥯥å����ˤĤ��Ƥξ���
   (cookie-name
     :accessor cookie-name-of
     :init-keyword :cookie-name
     :init-value "tir03-auth")
   ;; keywords�ξܺ٤ϡ�rfc.cookie��construct-cookie-string�򻲾�
   (cookie-keywords
     :accessor cookie-keywords-of
     :init-keyword :cookie-keywords
     :init-value '(
                   :discard #t
                   :path "/"
                   ))

   ;; internal slot
   (session-manager
     :accessor session-manager-of
     :init-value #f)
   ))


(define-method initialize ((self <cgi-auth>) initargs)
  (next-method)
  ;; check :storage-dir
  (unless (storage-dir-of self)
    (errorf "~a must be need to :storage-dir" (class-name (class-of self))))
  ;; prepare :storage-dir
  (unless (file-exists? (storage-dir-of self))
    (make-directory* (storage-dir-of self)))
  ;; make session-manager
  (set!
    (session-manager-of self)
    ((session-maker-of self) self))
  )


;;; --------


(define-method cgi-auth-info-save! ((self <cgi-auth>) new-cgi-auth-info
                                                     . opt-sid)
  (let1 new-cgi-auth-info (cond
                            ((list? new-cgi-auth-info) new-cgi-auth-info)
                            ((is-a? new-cgi-auth-info <parameter>)
                             (new-cgi-auth-info))
                            (else
                              (error "invalid parameter" new-cgi-auth-info)))
    (let-optionals* opt-sid ((sid #f))
      (session-save! (session-manager-of self) sid new-cgi-auth-info))))


(define-method cgi-auth-info-delete! ((self <cgi-auth>) sid)
  (session-delete! (session-manager-of self) sid))


(define-method redirect/cookie ((self <cgi-auth>) sid . opt-url)
  (let-optionals* opt-url ((url (self-url/path-info)))
    (write-tree
      (cgi-header
        :pragma "no-cache"
        :cache-control "no-cache"
        :location url
        :cookies (construct-cookie-string
                   (list
                     (list*
                       (cookie-name-of self)
                       sid
                       (cookie-keywords-of self))))))))


(define (get-cgi-auth-info-from-cookie self)
  ;; cookie����cgi-auth-info(�ޤ���#f)����Ф���
  (and-let* ((sid (cgi-get-parameter
                    (cookie-name-of self)
                    (cgi-parse-parameters :query-string ""
                                          :merge-cookies #t))))
    (session-load (session-manager-of self) sid #f)))


(define (get-cgi-params)
  (and
    (equal? "GET" (cgi-get-metavariable "REQUEST_METHOD"))
    (and-let* ((query-string (cgi-get-metavariable "QUERY_STRING")))
      (cgi-parse-parameters :query-string query-string))))


(define (redirect-to-self)
  (write-tree
    (cgi-header
      :pragma "no-cache"
      :cache-control "no-cache"
      :location (self-url/path-info))))
(define-method cgi-auth:dispatch-login ((self <cgi-auth>) params)
  (errorf "internal error occured in ~s" (class-of self)))
(define-method cgi-auth:dispatch-logout ((self <cgi-auth>) params)
  (errorf "internal error occured in ~s" (class-of self)))
(define-method cgi-auth:dispatch-logined ((self <cgi-auth>) params)
  (redirect-to-self))
(define-method cgi-auth:dispatch-logouted ((self <cgi-auth>) params)
  (redirect-to-self))
(define *dispatch-table*
  (hash-table
    'equal?
    `("login" . ,cgi-auth:dispatch-login)
    `("logout" . ,cgi-auth:dispatch-logout)
    `("logined" . ,cgi-auth:dispatch-logined)
    `("logouted" . ,cgi-auth:dispatch-logouted)
    ))


(define-method with-auth-info ((self <cgi-auth>) thunk)
  (or
    (and-let* ((cgi-params (get-cgi-params))
               (dispatch-key (cgi-get-parameter
                               (cgi-auth-dispatch-key) cgi-params))
               (dispatch-proc (hash-table-get *dispatch-table*
                                              dispatch-key
                                              #f))
               )
      (dispatch-proc self cgi-params)
      #t)
    (parameterize ((cgi-auth-info (get-cgi-auth-info-from-cookie self)))
      (define (allowed)
        (thunk))
      (define (denied)
        (cgi-main
          (lambda (params)
            (apply
              cgi-tree-make
              :body (acl-drop-html-of self)
              (append
                (html-keywords-of self)
                (list
                  ;; fallback
                  :robots "NOINDEX,NOFOLLOW"
                  :title "ǧ��"
                  ))))))
      (cond
        ((not (cgi-auth-info)) (allowed)) ; not logined (all ok)
        ((eq? #t (acl-of self)) (allowed)) ; all allowed
        ((not (acl-of self)) (denied)) ; all denied
        ((list? (acl-of self))
         (let1 unique-id (get-keyword :unique-id (cgi-auth-info))
           (if (assoc-ref
                 (acl-of self) unique-id (acl-fallback-policy-of self))
             (allowed)
             (denied))))
        (else
          (error "assertion"))))))


(define-method must-required-auth-with ((self <cgi-auth>) thunk . keywords)
  (if (cgi-auth-info)
    (thunk)
    (let-keywords* keywords ((html (list
                                     (cgi-auth:make-html:form)
                                     (html:hr)
                                     (get-description-html self)
                                     )))
      (cgi-main
        (lambda (params)
          (apply
            cgi-tree-make
            :body html
            (append
              (html-keywords-of self)
              (list
                ;; fallback
                :robots "NOINDEX,NOFOLLOW"
                :title "ǧ�ڤ��Ʋ�����"
                ))))
        ;:on-error cgi-on-error/stack-trace
        ))))


(define-method get-description-html ((self <cgi-auth>))
  (errorf
    "internal error occured in ~s (not defined get-description-html)"
    (class-of self)))


(define-syntax choose-from-mode
  (syntax-rules ()
    ((_ mode logined-now logouted-now)
     (if (case mode
           ((flip) (cgi-auth-info))
           ((login) #f)
           ((logout) #t)
           (else
             (error ":mode must be 'flip or 'login or 'logout")))
       logouted-now
       logined-now))))


(define (cgi-auth:make-url . keywords)
  (let-keywords* keywords ((mode 'flip)) ; 'flip or 'login or 'logout
    (string-append
      (self-url/path-info)
      "?"
      (cgi-auth-dispatch-key) "=" (choose-from-mode mode
                                                    "login"
                                                    "logout"))))


(define (cgi-auth:make-html:a . keywords)
  (let-keywords* keywords ((mode 'flip) ; 'flip or 'login or 'logout
                           (label (choose-from-mode mode
                                    "������"
                                    "��������"))
                           (target #f)
                           )
    (html:a
      :href (cgi-auth:make-url :mode mode)
      :target target
      (html-escape-string label))))


(define (cgi-auth:make-html:form . keywords)
  (let-keywords* keywords ((mode 'flip)
                           (label (choose-from-mode mode
                                    "������"
                                    "��������"))
                           (target #f)
                           ;; ToDo: html�����Ǥ���褦�ˤ��٤���
                           )
    (make-form
      (self-url/path-info)
      `((,(cgi-auth-dispatch-key) ,(choose-from-mode mode
                                                     "login"
                                                     "logout")))
      (html:input :type "submit"
                  :value label)
      :method "get"
      :target target)))


;;; --------


(provide "tir03/cgi/auth")
