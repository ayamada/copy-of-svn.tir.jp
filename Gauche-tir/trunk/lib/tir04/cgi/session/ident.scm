;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ǧ�ھ���򥻥å�������¸���륯�饹
;;; ñ�ʤ롢ǧ�ڥ��饹��session���饹���꤯�ƤӽФ��٤Υ�åѡ���

;;; ToDo: �����ॢ���Ȼ����б���ɤ����롩
;;;       �֥����ॢ���Ȥ��ޤ����פ��餤�ϽФ��������������Ǥ�
;;;       ������鷺��ident-info��#f�ˤʤ�١�Ƚ�꤬����
;;;       ���Ȥǹͤ��롣���ΤȤ�����б�̵���Ǻ�롣

#|
(define *csa*
  (make <cgi-session-ident>
        :dbm-type <fsdbm>
        :dbm-path "/path/to/dbm-file"
        :expire-second (* 1 24 60 60)
        :cookie-name "cgi-session"
        :cookie-keywords '(:discard #t :path "/") ; ������expires�����ꤷ�ʤ���
        :dispatch-keyname "cgi-session-ident" ; :cgi-ident��internal-key-prefix�Ȥ����ȤǤ���������
        :cgi-ident (make <cgi-ident-hatena>
                        :internal-key-prefix "cgi-ident"
                        :error-html-proc ...
                        :error-html-keywords '(...)
                        :api-key "..."
                        :sec-key "..."
                        )
        ...))

(cgi-main
  (lambda (params)
    (with-cgi-session-ident
      *csa*
      params
      (lambda (fixed-params ident-info session-parameter)
        ;; fixed-params�ˤϡ�dispatch-keyname������줿params�����롣
        ;; cgi.ident.*�Ȥϰ㤤���������Ͼ��ident-info��Ϳ�����롣
        ;; â���������ॢ���Ȥ�����#f�ˤʤ롣
        ;; ident-info��Ϳ�����Ƥ�����ϡ�(session-parameter obj)�ǡ�
        ;; read/write invariance��������ͤ򵭲������������롣
        ;; (session-parameter)�ǡ������ͤ򻲾ȤǤ��롣
        ;; ����ͤ�#f��
        ;; �����ͤϥ��å�����ڤ��ޤǤδ֤Τ�ͭ����
        ;;
        ;; ������/��������url���������롣
        ;; type�ˤϡ�'login 'logout�Τ����줫�����ꤹ�롣
        ;; ������/�������ȸ塢���ߤ�path�դ�url�ˡ�
        ;; callback-params�ǻ��ꤷ���ѥ�᡼���դ�����äƤ��롣
        (make-cgi-session-ident-url *csa* callback-params type)
        ;; ������/��������form���������롣
        (make-cgi-session-ident-form *csa*
                                    callback-params type html-tree . keywords)
        ;; ��������url��������ȥܥ��󤬲������ȡ����å�����˴����졢
        ;; ident-info��(session-parameter)��#f���֤��褦�ˤʤ롣
        ;; (���å������ΤϤ��Τޤ����֤���롣)
        ... ; �͡��ʽ�����Ԥ�
        ))))
|#


(define-module tir04.cgi.session.ident
  (use srfi-1)
  (use gauche.parameter)
  (use text.html-lite)
  (use www.cgi)
  (use tir04.cgi.util)
  (use tir04.cgi.ident)
  (extend tir04.cgi.session)

  (export
    <cgi-session-ident>
    with-cgi-session-ident
    make-cgi-session-ident-url
    make-cgi-session-ident-form
    ))
(select-module tir04.cgi.session.ident)


(define-class <cgi-session-ident> (<cgi-session>)
  (
   (cgi-ident ; ǧ�ڥ��󥹥��󥹤���ꤹ��
     :accessor cgi-ident-of
     :init-keyword :cgi-ident
     :init-value #f)
   (dispatch-keyname
     ;; with-cgi-session-ident�ǽ����򲣼�ꤹ��٤�cgi�ѥ�᡼����keyʸ����
     ;; ¾��key�����ʤ��褦�ˤ������
     :accessor dispatch-keyname-of
     :init-keyword :dispatch-keyname
     :init-value "tir04-cgi-session-ident")
   ))


(define-method initialize ((self <cgi-session-ident>) initargs)
  (next-method)
  (unless (cgi-ident-of self)
    (error ":cgi-ident must be required"))
  )


(define-method with-cgi-session-ident ((self <cgi-session-ident>) params proc)
  (with-cgi-ident
    (cgi-ident-of self)
    params
    (lambda (params-2 ident-info)
      (cgi-with-session
        self
        (lambda (old-session-data)
          (let ((dispatch-val (cgi-get-parameter (dispatch-keyname-of self)
                                                 params-2))
                (params-3 (alist-delete (dispatch-keyname-of self)
                                        params-2
                                        equal?)))
            (cond
              (ident-info (do-login self ident-info params-3))
              ((equal? dispatch-val "logout") (do-logout self params-3))
              (else
                (let ((session-ident-info (and
                                           old-session-data
                                           (car old-session-data)))
                      (session-parameter (make-parameter
                                           (and
                                             old-session-data
                                             (cadr old-session-data)))))
                  (let1 result (proc params-3
                                     session-ident-info
                                     session-parameter)
                    ;; ���˥��å����¸�ߤ�����Τߡ�update����
                    (when old-session-data
                      (cgi-update-session! self (list
                                                  (car old-session-data)
                                                  (session-parameter))))
                    result))))))))))

(define (do-login self ident-info params)
  (cgi-create-session! self (list ident-info #f))
  (location
    (append-params-to-url
      (self-url/path-info)
      params)))

(define (do-logout self params)
  (cgi-remove-session! self)
  (location
    (append-params-to-url
      (self-url/path-info)
      params)))


(define-method make-cgi-session-ident-url ((self <cgi-session-ident>)
                                          callback-params type)
  (case type
    ((login) (make-ident-url (cgi-ident-of self) callback-params))
    ((logout) (append-params-to-url
                (self-url/path-info)
                (list*
                  `(,(dispatch-keyname-of self) "logout")
                  callback-params)))
    (else (error "assertion"))))


(define-method make-cgi-session-ident-form ((self <cgi-session-ident>)
                                           callback-params
                                           type
                                           html-tree . keywords)
  (case type
    ((login) (apply make-ident-form
                    (cgi-ident-of self)
                    callback-params
                    html-tree
                    keywords))
    ((logout) (apply
                make-form
                (self-url/path-info)
                (list*
                  `(,(dispatch-keyname-of self) "logout")
                  callback-params)
                (or html-tree (html:input
                                :type "submit"
                                :value "logout"))
                keywords))
    (else (error "assertion"))))




(provide "tir04/cgi/session/ident")
