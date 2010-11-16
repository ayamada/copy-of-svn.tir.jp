;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ����:
;;; CGI�ѥ��å����ޥ͡����㡣
;;; ���å�����cookie����¸����롣

;;; note: ���å�����#f����¸������ϤǤ��ʤ���
;;;       (cgi-with-session���ˡ����å����̵���Τ���#f�����äƤ���Τ�
;;;        Ƚ�ǤǤ��ʤ��ʤ�١�)
;;;       #f����¸���褦�Ȥ���ȡ���¸�Υ��å�������������ͤȤ�����
;;;       #f��������¸���������ϡ�'(#f)���ˤ��٤���

;;; note: ���Υ⥸�塼��Ǥϡ�expires�����ꤷ�����å����ϻ��ѤǤ��ʤ���
;;;       (���Ӥ����å����ΰ١�)


#|
(define *cgi-session*
  (make
    <cgi-session>
    :dbm-type <fsdbm>
    :dbm-path "/path/to/dbm-file"
    :expire-second (* 1 24 60 60)
    :cookie-name "cgi-session"
    :cookie-keywords '(:discard #t :path "/") ; expires�����ꤷ�ʤ���
    ))
(cgi-main
  (lambda (params)
    (cgi-with-session
      *cgi-session*
      (lambda (session-data)
        ...
        ))))
|#

(define-module tir04.cgi.session
  (use gauche.parameter)
  (use rfc.cookie)
  (use www.cgi)
  (extend tir04.session.dbm)

  (export
    <cgi-session>
    cgi-with-session
    cgi-create-session!
    cgi-update-session!
    cgi-remove-session!
    ))
(select-module tir04.cgi.session)


(define-class <cgi-session> (<session-dbm>)
  (
   ;; ���å���󥯥å����ˤĤ��Ƥξ���
   (cookie-name
     :accessor cookie-name-of
     :init-keyword :cookie-name
     :init-value "cgi-session")
   ;; keywords�ξܺ٤ϡ�rfc.cookie��construct-cookie-string�򻲾�
   (cookie-keywords
     :accessor cookie-keywords-of
     :init-keyword :cookie-keywords
     :init-value '(
                   :discard #t
                   :path "/"
                   ))
   ;; �������줿Set-Cookie�إå�����Ū�˵������륹��å�
   (cookie-headers
     :accessor cookie-headers-of
     :init-form (make-parameter #f))
   ;; ����¾�Υ���åȤˤĤ��Ƥϡ�<session-dbm>��<session>�򻲾�
   ))


(define-method initialize ((self <cgi-session>) initargs)
  (next-method)
  ;; ���ΤȤ���ϡ��ä˽���̵��
  #t)


;;; --------


(define-method cgi-with-session ((self <cgi-session>) proc)
  ;; note: �ʲ�������������with-session�ȰۤʤäƤ��롣
  ;;       - cookie����sid���ɤ߽Ф�����
  ;;       - proc���֤��ͤˡ�ɬ�פǤ����Set-Cookie�إå����ɲä������
  ;; note: �⤷���塢���å�����expires��Ȥ���褦�ˤ�����ϡ�
  ;;       ���μ�³�������Set-Cookie����Ϥ���褦��ľ��ɬ�פ����롣
 (let1 sid (cgi-get-parameter
             (cookie-name-of self)
             (cgi-parse-parameters :query-string ""
                                   :merge-cookies #t))
    (parameterize (((cookie-headers-of self) '()))
      (let1 result (with-session self sid proc)
        (list ((cookie-headers-of self)) result)))))

(define-method cgi-create-session! ((self <cgi-session>) session-data)
  (let1 sid (create-session! self session-data)
    ;; ((cookie-headers-of self))�򹹿�����
    ((cookie-headers-of self) (list
                                "Set-Cookie: "
                                (car ; ���ΤȤ���cookie�ϰ�ĸ���
                                  (construct-cookie-string
                                    `((,(cookie-name-of self) ,sid
                                       ,@(cookie-keywords-of self)))
                                    0))
                                "\r\n"))))
(define-method cgi-create-session! (session-data)
  (unless (cgi-session)
    (error "cannot found <cgi-session>"))
  (cgi-create-session! (cgi-session) session-data))

(define-method cgi-update-session! ((self <cgi-session>) session-data)
  ;; ���ΤȤ����ɲä�ɬ�פʽ�����̵��
  (update-session! self session-data))
(define-method cgi-update-session! (session-data)
  (unless (cgi-session)
    (error "cannot found <cgi-session>"))
  (cgi-update-session! (cgi-session) session-data))

(define-method cgi-remove-session! ((self <cgi-session>))
  ;; ���ΤȤ����ɲä�ɬ�פʽ�����̵��
  ;; (��ǽ�ʤ顢�Ť����å�����ä��褦�ˤ�������������ä�̵���äݤ�)
  (remove-session! self))
(define-method cgi-remove-session! ()
  (unless (cgi-session)
    (error "cannot found <cgi-session>"))
  (cgi-remove-session! (cgi-session)))


;;; --------


(provide "tir04/cgi/session")
