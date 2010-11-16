#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;(add-load-path "lib")
(use gauche.parameter)
(use srfi-1)
(use srfi-2)
(use text.html-lite)
(use www.cgi)
(use www.cgi.dispatch-tir)
;(use www.cgi.dispatch-tir.alist-table) ; for get to debug info

(define cgi-params (make-parameter #f))


;;; ----
;;; defines

(define *title* "www.cgi.dispatch-tir sample")
(define *css-url* #f)
;(define *css-url* "http://css.tir.jp/tir.css")


;;; ----
;;; common functions


(define (redirect . opt-url)
  (cgi-header
    :location (get-optional opt-url (or
                                      (cgi-get-metavariable "SCRIPT_NAME")
                                      "/"))))

(define (entry->redirect entry . opt-params)
  (redirect (cgi-entry->url entry :params (get-optional opt-params '()))))

(define-syntax and/null
  (syntax-rules ()
    ((_ . tests)
     (or (and . tests) '()))))

(define (make-cgi-response . html-bodies)
  (list
    (cgi-header)
    (html-doctype)
    (html:html
      (html:head
        (html:meta :http-equiv "Content-Type"
                   :content (string-append
                              "text/html; charset="
                              (x->string (gauche-character-encoding))))
        (and/null
          *title*
          (html:title (html-escape-string *title*)))
        (and/null
          *css-url*
          (html:link :rel "Stylesheet" :type "text/css" :href *css-url*))
        )
      (html:body
        html-bodies))))

(define (entry&label->a-tag entry label . opt-params)
  (html:a
    :href (cgi-entry->url entry :params (get-optional opt-params '()))
    (html-escape-string label)))

;;; ----
;;; define cgi entries


(define-cgi-entry (cgi:a . keywords) '(("cmd" "a"))
  (make-cgi-response
    (html:h1 "��ǽA")
    (html:p "(�ۤ��ۤ�)")
    (html:hr)
    (entry&label->a-tag 'cgi:menu "���")
    ))


(define-cgi-entry (cgi:b . keywords) '(("cmd" "b"))
  (make-cgi-response
    (html:h1 "���ص�ǽB")
    (html:ul
      (html:li
        "���ص�ǽB" "(���߰���)"
        (html:ul
          (html:li
            (entry&label->a-tag 'cgi:b:1 "���ص�ǽB:1�˿ʤ�")))))
    (html:hr)
    (html:p "(���ˤ礴�ˤ�)")
    (html:hr)
    (entry&label->a-tag 'cgi:menu "���")
    ))


(define-cgi-entry (cgi:b:1 . keywords) '(("cmd" "b") ("cmd-b" "1"))
  (make-cgi-response
    (html:h1 "���ص�ǽB:1")
    (html:ul
      (html:li
        (entry&label->a-tag 'cgi:b "���ص�ǽB�����")
        (html:ul
          (html:li
            "���ص�ǽB:1" "(���߰���)"
            (html:ul
              (html:li
                (entry&label->a-tag 'cgi:b:1:2 "���ص�ǽB:1:2�˿ʤ�")))))))
    (html:hr)
    (html:p "(�ޤ��ޤ�)")
    (html:hr)
    (entry&label->a-tag 'cgi:menu "�ᥤ���˥塼�����")
    ))


(define-cgi-entry (cgi:b:1:2 . keywords) '(("cmd" "b")
                                           ("cmd-b" "1")
                                           ("cmd-b-1" "2"))
  (make-cgi-response
    (html:h1 "���ص�ǽB:1:2")
    (html:ul
      (html:li
        (entry&label->a-tag 'cgi:b "���ص�ǽB�����")
        (html:ul
          (html:li
            (entry&label->a-tag 'cgi:b:1 "���ص�ǽB:1�����")
            (html:ul
              (html:li
                "���ص�ǽB:1:2" "(���߰���)"
                ))))))
    (html:hr)
    (html:p "(���⤰��)")
    (html:hr)
    (entry&label->a-tag 'cgi:menu "�ᥤ���˥塼�����")
    ))


(define-cgi-entry (cgi:param-view . keywords) '(("cmd" "param-view"))
  (make-cgi-response
    (html:h1 "CGI�ѥ�᡼����ǧ")
    (html:dl
      (map
        (lambda (key&vals)
          (list
            (html:dt (html-escape-string (car key&vals)))
            (map
              (lambda (val)
                (html:dd (html-escape-string val)))
              (cdr key&vals))))
        (sort
          (cgi-params)
          (lambda (x y)
            (string<=? (car x) (car y))))))
    (html:hr)
    (entry&label->a-tag 'cgi:menu "���")
    ))


;; check�������ˤ�#f���֤�
;; check���Ի��ˤ�error-msg���֤�
(define (check-error-params-for-cgi:reg)
  (let* ((text (or (cgi-get-parameter "text" (cgi-params)) ""))
         (text-length (string-length text))
         )
    (cond
      ((zero? text-length) "ʸ���󤬶��Ǥ�")
      ((< 8 text-length) "Ȭʸ������ˤ��Ʋ�����")
      (else #f))))

(define-cgi-entry (cgi:reg . keywords) '(("cmd" "reg"))
  (let-keywords* keywords ((error-msg #f)
                           )
    (define (make-internal-html)
      (list
        (html:p
          "��Ͽ������ʸ�����Ȭʸ����������Ϥ��Ʋ�������"
          (html:br)
          "(�����ޤ���Ȭʸ����ۤ���ȥ��顼���Фޤ�)")
        (html:input
          :type "text"
          :name "text"
          :value (or (cgi-get-parameter "text" (cgi-params)) ""))
        (html:input
          :type "submit"
          :value "��Ͽ")))

    (make-cgi-response
      (html:h1 "��Ͽ")
      (html:p "(��Ͽ���̤�ɤ��Ǥ����ºݤˤϲ�����Ͽ�Ǥ��ޤ���)")
      (and/null
        error-msg
        (html:p (html:strong (html-escape-string error-msg))))
      (cgi-entry->form
        'cgi:reg:confirm
        :internal-html (make-internal-html))
      (html:hr)
      (entry&label->a-tag 'cgi:menu "���")
      )))


(define-cgi-entry (cgi:reg:confirm . keywords) '(("cmd" "reg")
                                                 ("confirm" :*)
                                                 )
  (or
    (and-let* ((error-msg (check-error-params-for-cgi:reg)))
      (cgi:reg :error-msg error-msg))
    (let1 text (cgi-get-parameter "text" (cgi-params))
      (make-cgi-response
        (html:p
          "�ʲ���ʸ�������Ͽ���ޤ���"
          "�����ˤ������С���Ͽ�ܥ���򲡤��Ʋ�������")
        (html:blockquote
          :style "border:1px solid"
          (html:tt
            (html-escape-string text)))
        (cgi-entry->form
          'cgi:reg:submit
          :params `(("text" ,text))
          :submit-label "��Ͽ")
        (html:hr)
        (entry&label->a-tag 'cgi:menu "���")
        ))))


(define-cgi-entry (cgi:reg:submit . keywords) '(("cmd" "reg")
                                                ("submit" :*)
                                                )
  ;; �����դǡ����˥祴�˥�ȡ���Ͽ�¹Ԥ�Ԥ�
  (let1 text (cgi-get-parameter "text" (cgi-params))
    (entry->redirect 'cgi:reg:done `(("text" ,text)))))


(define-cgi-entry (cgi:reg:done . keywords) '(("cmd" "reg")
                                              ("done" :*)
                                              )
  (let1 text (cgi-get-parameter "text" (cgi-params))
    (make-cgi-response
      (html:p
        "��"
        (html-escape-string text)
        "��"
        "����Ͽ���ޤ���(�ºݤˤϡ�������Ͽ���Ƥ��ޤ���)��"
        )
      (html:hr)
      (entry&label->a-tag 'cgi:menu "���")
      )))


(define-cgi-entry (cgi:outer . keywords) '(("hoge" :*))
  (make-cgi-response
    (html:h1 "�㳰Ū�ʥѥ�᡼������ˤ��б�")
    (html:hr)
    (entry&label->a-tag 'cgi:menu "���")
    ))


(define-cgi-entry (cgi:menu . keywords) '(("cmd" "menu"))
  (make-cgi-response
    (html:h1 "�ᥤ���˥塼")
    (html:ul
      (map
        (lambda (l)
          (html:li
            (apply entry&label->a-tag l)))
        '(
          (cgi:a "��ǽA")
          (cgi:b "���ص�ǽB")
          (cgi:param-view "CGI�ѥ�᡼����ǧ" (("aa" "bb") ("cc" "dd" "ee")))
          (cgi:reg "��Ͽ")
          (cgi:outer "�㳰Ū�ʥѥ�᡼������ˤ��б�")
          )))
    ))


(define-cgi-entry cgi:fallback '() cgi:menu)


;;; ----
;;; define main

(define (main args)
  (cgi-main
    (lambda (params)
      (parameterize ((cgi-params params))
        ((cgi-params-dispatch params) :and 'other :args 'etc...)))))

;(use www.fastcgi)
;(define (main args)
;  ;; rewrite to cgi-metavariables (hack for reverse proxy)
;  (with-fastcgi
;    (lambda ()
;      (parameterize ((cgi-metavariables
;                       (list*
;                         '("SERVER_NAME" "e.tir.jp")
;                         '("SERVER_PORT" "80")
;                         (remove
;                           (lambda (key+val)
;                             (let1 key (car key+val)
;                               (or
;                                 (string=? key "SERVER_NAME")
;                                 (string=? key "SERVER_PORT")
;                                 )))
;                           (or (cgi-metavariables) '())))))
;        (cgi-main
;          (lambda (params)
;            (parameterize ((cgi-params params))
;              ((cgi-params-dispatch params) :and 'other :args 'etc...))))))))


