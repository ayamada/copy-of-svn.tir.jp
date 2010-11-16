#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; note : ���Υ⥸�塼��ϡ���ĤΥ⥸�塼��ˤޤȤ�����Ρ�
;;;        ���Ū�ʾ��֤δؿ�������¸����⥸�塼��Ȥ��롣


(define-module tir.misc
  ;(use srfi-2) ; and-let*

  (export
    hes
    html-tree-make
    make-webpage-main
    cgi-metavariables->html
    make-view-cgi-metavariables-thunk

    number->string-japanese-weekday
    number->string-japanese-wday
    time->japanese-date-string
    ))
(select-module tir.misc)


;;; ----
;;; tir.cgi?


(use srfi-2)
(use gauche.parameter)
(use text.html-lite)
(use text.tree)
(use www.cgi)

(define hes html-escape-string)

(define-syntax when/null
  (syntax-rules ()
    ((_ pred body ...) (if pred
                         (begin body ...)
                         '()))))


;; ToDo : frame�б�
(define (html-tree-make . keywords)
  (let-keywords* keywords ((encoding #f)
                           (css-url #f)
                           (css-body #f)
                           (js-url #f)
                           (js-body #f)
                           (robots #f)
                           (title #f)
                           (body #f)
                           (body-class #f)
                           (frame #f) ; ToDo : �ޤ�̤����
                           )
    (list
      (if encoding
        #`"<?xml version=\"1.0\" encoding=\",|encoding|\"?>"
        "<?xml version=\"1.0\"?>")
      "\n"
      (html-doctype :type :xhtml-1.0-transitional)
      (html:html
        :lang "ja-JP"
        :xml:lang "ja-JP"
        :xmlns "http://www.w3.org/1999/xhtml"
        (html:head
          (when/null encoding
            (html:meta :http-equiv "Content-Type"
                       :content #`"text/html; charset=,|encoding|"))
          (when/null (or css-url css-body)
            (html:meta :http-equiv "Content-Style-Type"
                       :content "text/css"))
          (when/null (or js-url js-body)
            (html:meta :http-equiv "Content-Script-Type"
                       :content "text/javascript"))
          (when/null robots
            (html:meta :name "ROBOTS"
                       :content robots))
          (when/null title
            (html:title (hes title)))
          (when/null css-url
            (html:link :rel "Stylesheet"
                       :type "text/css"
                       :href css-url))
          ;; ToDo : css-body, js-url, js-body
          )
        (when/null body
          (if body-class
            (html:body :class body-class body)
            (html:body body)))))))

;; cgi-main������˻Ȥ���webpage-main�ؿ�����������ⳬ�ؿ���
;; ������ɰ����Ȥ���encoding��on-error����ꤹ�����
(define (make-webpage-main . k)
  (let-keywords* k ((encoding #f)
                    (on-error #f)
                    (default-content-type #f)
                    )
    (let1 default-content-type (or
                                 default-content-type
                                 (if encoding
                                   #`"text/html; charset=,|encoding|"
                                   "text/html"))
      ;; proc�ϡ��̾��text.tree������ˡ��ʲ��Τ褦��list���֤�����
      ;; '(
      ;;   :ignore-encoding #f ; ���ϻ���ư���ܸ쥳�����Ѵ���Ԥ����ɤ���
      ;;   :http-header (:cache-control "private" :pragma "no-cache")
      ;;   :http-body #f ; ���줬#f�Ǥʤ��ʤ顢html���ϤϹԤ鷺�ˡ���������
      ;;   ... �� ����¾��html-tree-make��ɬ�פȤ���keywords
      ;;   )
      (lambda (proc)
        (parameterize ((cgi-output-character-encoding
                         (gauche-character-encoding))) ; �ǥե���Ȥ��Ѵ����ʤ�
          (begin0
            (apply
              cgi-main
              (lambda (params)
                ;; ToDo : form-parameter�μ�ư���ܸ쥳�����Ѵ���ǽ
                ;;        �ʥХ��ʥ�ե����륢�åץ����б��ޤ��
                (let* ((keywords (proc params)) ; proc��¹Ԥ�����̤�������

                       (ignore-encoding
                         (get-keyword :ignore-encoding keywords #f))
                       (http-header
                         (get-keyword :http-header     keywords '()))
                       (http-body
                         (get-keyword :http-body       keywords #f))

                       (content-type
                         (get-keyword :content-type http-header #f))
                       (location
                         (get-keyword :location     http-header #f))
                       )
                  (unless ignore-encoding
                    (cgi-output-character-encoding encoding)) ; encoding���Ѵ�
                  ;; ��̤�text.tree�Ȥ����֤�
                  (cons
                    (apply
                      cgi-header
                      (if (or content-type location)
                        http-header
                        (list*
                          :content-type default-content-type
                          http-header)))
                    (cond
                      (location '())
                      (http-body (list http-body))
                      (else (apply
                              html-tree-make
                              keywords))))))
              (if on-error
                `(:on-error ,on-error)
                '()))))))))


(define (cgi-metavariables->html mv)
  (html:dl
    (map (lambda (x)
           (list
             (html:dt (hes (car x)))
             (html:dd (hes (cadr x)))))
         (sort
           (or mv '())
           (lambda (x y)
             (string<? (car x) (car y)))))))


;; ������ɰ�����Ϳ���ơ�CGI�᥿�ѿ���html�Ȥ���ɽ�����������
;; CGI������ץ�thunk����������ⳬ�ؿ���
;; �̾��:css-url :robots :title :back-url��Ϳ����н�ʬ��
;; �ʢ�:title�����ܸ��Ȥ����ϡ�:encoding��ɬ�ס�
;;     â�������ߤϤޤ�form���Ϥμ�ư���ܸ쥳�����Ѵ����б����Ƥ��ʤ��Τǡ�
;;     :encoding�ϻȤ�ʤ������ɤ���
;; �Ķ��ѿ���ɽ�����ʤ���
(define (make-view-cgi-metavariables-thunk . keywords)
  (lambda ()
    (let1 webpage-main (apply
                         make-webpage-main
                         (or
                           (and-let* ((encoding
                                        (get-keyword :encoding keywords #f)))
                             (list :encoding encoding))
                           '()))
      (webpage-main
        (lambda (params)
          (define back-url-html
            (or
              (and-let* ((back-url (get-keyword :back-url keywords #f)))
                (html:ul
                  (html:li
                    (html:a
                      :href back-url
                      "back"))))
              '()))
          (define back-url-html-separator
            (if (null? back-url-html)
              '()
              (html:hr)))
          (define (make-html-body)
            (list
              back-url-html
              back-url-html-separator
              (html:h1
                :class "inline_centering"
                (hes (get-keyword :title keywords "cgi-metavariables"))
                )
              (cgi-metavariables->html
                (cgi-metavariables))
              back-url-html-separator
              back-url-html
              ))

          (list*
            :body (make-html-body)
            keywords
            ))))))



;;; ----
;;; tir.japanese?


(define (number->string-japanese-count num)
  ;; ToDo : ���Ȥ�
  num)


(define *japanese-weekday-vector*
  #("������" "������" "������" "������" "������" "������" "������"))
(define *japanese-wday-vector*
  #("��" "��" "��" "��" "��" "��" "��"))

(define (number->string-japanese-weekday num)
  (or
    (vector-ref *japanese-weekday-vector* num #f)
    (error "invalid number")))
(define (number->string-japanese-wday num)
  (or
    (vector-ref *japanese-wday-vector* num #f)
    (error "invalid number")))

(define (time->japanese-date-string time)
  (let1 tm (sys-localtime time)
    (format
      "~aǯ~a��~a��(~a)"
      (+ 1900 (number->string-japanese-count (ref tm 'year)))
      (+ 1 (number->string-japanese-count (ref tm 'mon)))
      (number->string-japanese-count (ref tm 'mday))
      (number->string-japanese-wday (ref tm 'wday))
      )))


;;; ----


(provide "tir/misc")

