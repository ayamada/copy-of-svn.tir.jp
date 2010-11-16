;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; �ϤƤ�ǧ�ڤ�Ԥ����饹

;;; ����:
;;; ���Υ⥸�塼��ϡ��ϤƤ�ǧ�ڤ�Ԥ��٤Υ��饹���󶡤��롣
;;; ���Υ��饹�ϡ��桼���μ��̵�ǽ�Τߤ��󶡤���١����ӡ�
;;; ���å���󥯥饹���Ѱդ���ɬ�פ�����(tir04.cgi.session)��
;;; �ޤ��ϡ����å����Ȥ��̤ˡ�
;;; ���餫�ν��פ������Ѥ���Ĺ�ư(�ǡ����κ����ʪ�ʤι�����)��
;;; �桼�����Ԥ��ݤ�ñ�γ�ǧ���ӤȤ��Ƥ����Ѳ�ǽ��

;;; NB: �ϤƤ�ǧ�ڤǤϡ�ident-info�μ����������Ѥ���İ١�
;;;     ident-info�������Ǥ��������֤��ͤϡ�
;;;     location�ˤ�������쥯�ȤȤ���٤��Ǥ��롣
;;;     �������ʤ��ȡ��֥饦���������ư�ˤ�äƤ⤦���٥����������줿�ݤ�
;;;     Ʊ��cert�ˤ��ǧ�ڳ�ǧ���Ԥ��Ƥ��ޤ�����Ĥ�cert�ˤ�äƹԤ���
;;;     ǧ�ڳ�ǧ�ϰ�󤭤�ʤΤǡ����顼��ɽ������Ƥ��ޤ���

;;; NB: �嵭�������ȼ�������Υ⥸�塼���Ȥä�ident-info����������ݤˤϡ�
;;;     ;ʬ�ʡ�cert�פΥѥ�᡼�����ɲä���Ƥ��ޤ���
;;;     ���Υѥ�᡼�����Τ������Ǻ������褦�ˤʤäƤ��뤬��
;;;     ������Хå��ѥ�᡼���Ȥ��ơ�cert�פΥѥ�᡼��̾����Ѥ������
;;;     �Ǥ��ʤ���
;;;     ��äơ�����ǧ�ڥ⥸�塼�����Ѥ���ݤˤϡ�
;;;     ��cert�פΥѥ�᡼��̾��Ȥ�ʤ��褦����դ���ɬ�פ����롣

#|
(define *cgi-ident*
  (make <cgi-ident-hatena>
        :internal-key-prefix "cgi-ident"
        :error-html-proc ...
        :error-html-keywords '(...)
        :api-key "..."
        :sec-key "..."
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
        ;;
        ;; ����Ͻ��פʤΤǡ��⤦���ٷ����֤��Ƥ�����
        ;; �ϤƤ�ǧ�ڤǤϡ�ident-info�μ����������Ѥ���İ١�
        ;; ident-info�������Ǥ��������֤��ͤϡ�
        ;; location�ˤ�������쥯�ȤȤ���٤��Ǥ��롣
        ;; �������ʤ��ȡ��֥饦���������ư�ˤ�äƤ⤦���٥����������줿�ݤ�
        ;; Ʊ��cert�ˤ��ǧ�ڳ�ǧ���Ԥ��Ƥ��ޤ�����Ĥ�cert�ˤ�äƹԤ���
        ;; ǧ�ڳ�ǧ�ϰ�󤭤�ʤΤǡ����顼��ɽ������Ƥ��ޤ���
        ;; '(
        ;;   :ident-type 'hatena ; ����
        ;;   :ident-path "" ; ����
        ;;   :ident-id "..." ; �ϤƤ�ID
        ;;   :uid "..." ; "hatena::�ϤƤ�ID"��sha1 digest����hexify����ʸ����
        ;;   :x-ident-info '(("name" "�ϤƤ�ID")
        ;;                  ("image_url" "�ץ�ե����������URL")
        ;;                  ("thumbnail_url" "�ץ�ե������������ͥ����URL")
        ;;                  )
        ;;   )
        ;; ǧ�ڥڡ����ؤ�url��������
        (make-ident-url *cgi-ident* callback-params)
        ;; ǧ�ڥܥ�������
        (make-ident-form *cgi-ident* callback-params html-tree . keywords)
        ... ; �͡��ʽ�����Ԥ�
        ))))
|#


(define-module tir04.cgi.ident.hatena
  (use text.tree)
  (use text.html-lite)
  (use www.cgi)
  (use tir04.cgi.util)
  (extend tir04.cgi.ident)
  (use rfc.http)
  (use rfc.md5)
  (use rfc.sha1)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use sxml.tools)

  (export
    <cgi-ident-hatena>
    with-cgi-ident
    make-ident-url
    make-ident-form
    ))
(select-module tir04.cgi.ident.hatena)


(define *hatena-ident-url* "http://auth.hatena.ne.jp/auth")
(define *hatena-api-server* "auth.hatena.ne.jp")
(define *hatena-api-path* "/api/auth.xml")



(define-class <cgi-ident-hatena> (<cgi-ident>)
  (
   (api-key
     :accessor api-key-of
     :init-keyword :api-key
     :init-value #f)
   (sec-key
     :accessor sec-key-of
     :init-keyword :sec-key
     :init-value #f)
   ))


(define-method initialize ((self <cgi-ident-hatena>) initargs)
  (next-method)
  (unless (api-key-of self)
    (error ":api-key must be required"))
  (unless (sec-key-of self)
    (error ":sec-key must be required"))
  )


(define (get-callback-path)
  (if (cgi-get-metavariable "PATH_INFO")
    (cgi-get-metavariable "REQUEST_URI")
    ""))

(define (redirect self cpath true-params)
  (location
    (append-params-to-url
      cpath
      (cons
        (list (dispatch-key-of self) "logined")
        true-params))))

;; �ҥ��饹���������٤�methods
(define-method dispatch-or-ident-info ((self <cgi-ident-hatena>) dispatch-val
                                                        cgi-params
                                                        true-params
                                                        ident-info-cont)
  (if (equal? "logined" dispatch-val)
    (let1 cpath (or
                  (cgi-get-parameter (internal-key self "cpath") cgi-params)
                  "")
      (if (#/^\// cpath)
        (redirect self cpath true-params)
        (try-get-ident-info self true-params ident-info-cont)))
    (let1 params-without-api-sig (list*
                                   `("api_key" ,(api-key-of self))
                                   `(,(dispatch-key-of self) "logined")
                                   `(,(internal-key self "cpath") ,(get-callback-path))
                                   true-params)
      (location
        (append-params-to-url
          *hatena-ident-url*
          (cons
            (list "api_sig" (make-api-sig self params-without-api-sig))
            params-without-api-sig))))))


(define (make-api-sig self params)
  (digest-hexify
    (md5-digest-string
      (tree->string
        (list
          (sec-key-of self)
          (sort
            params
            (lambda (x y)
              (string<? (car x) (car y)))))))))

(define (try-get-ident-info self params ident-info-cont)
  (guard (e (else (get-error-html self (ref e 'message))))
    (ident-info-cont (get-ident-info self params))))

(define (get-ident-info self params)
  (let* ((cert (or (cgi-get-parameter "cert" params) (error "invalid access")))
         (api-sig (make-api-sig
                    self
                    `(("api_key" ,(api-key-of self))
                      ("cert" ,cert))))
         (hatena-api-path (append-params-to-url
                            *hatena-api-path*
                            `(("api_key" ,(api-key-of self))
                              ("cert" ,cert)
                              ("api_sig" ,api-sig))))
         )
    (receive (status-code headers body) (http-get
                                          *hatena-api-server*
                                          hatena-api-path
                                          :user-agent (x->string
                                                        (module-name
                                                          (current-module)))
                                          )
      (unless (#/^200/ (or status-code ""))
        (errorf "~a�ǥ��顼��ȯ�����ޤ�����" *hatena-api-server*))
      (let1 sxml (with-input-from-string
                   body
                   (cut ssax:xml->sxml (current-input-port) '()))
        (when (equal?
                "true"
                ((if-car-sxpath '(// has_error *text*)) sxml))
          (error
            (or
              ((if-car-sxpath '(// message *text*)) sxml)
              "assertion")))
        (let1 x-ident-info (map
                             (lambda (elem)
                               (list
                                 (x->string (sxml:element-name elem))
                                 (or ((if-car-sxpath '(*text*)) elem) "")))
                             ((sxpath '(// user *)) sxml))
          (let1 ident-id (cadr (assoc "name" x-ident-info))
            `(
              :ident-type hatena
              :ident-path ""
              :ident-id ,ident-id
              :uid ,(digest-hexify
                      (sha1-digest-string
                        (string-append "hatena::" ident-id)))
              :x-ident-info ,x-ident-info
              )))))))


(define-method get-remove-params-key-list ((self <cgi-ident-hatena>))
  '("cert"))


(define-method with-cgi-ident ((self <cgi-ident-hatena>) cgi-params proc)
  (next-method))

(define-method make-ident-url ((self <cgi-ident-hatena>) misc-params)
  (next-method))

(define-method make-ident-form ((self <cgi-ident-hatena>)
                               misc-params
                               html-tree . keywords)
  (next-method))


(provide "tir04/cgi/ident/hatena")
