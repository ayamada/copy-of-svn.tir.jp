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
    (html:h1 "機能A")
    (html:p "(ほげほげ)")
    (html:hr)
    (entry&label->a-tag 'cgi:menu "戻る")
    ))


(define-cgi-entry (cgi:b . keywords) '(("cmd" "b"))
  (make-cgi-response
    (html:h1 "階層機能B")
    (html:ul
      (html:li
        "階層機能B" "(現在位置)"
        (html:ul
          (html:li
            (entry&label->a-tag 'cgi:b:1 "階層機能B:1に進む")))))
    (html:hr)
    (html:p "(ごにょごにょ)")
    (html:hr)
    (entry&label->a-tag 'cgi:menu "戻る")
    ))


(define-cgi-entry (cgi:b:1 . keywords) '(("cmd" "b") ("cmd-b" "1"))
  (make-cgi-response
    (html:h1 "階層機能B:1")
    (html:ul
      (html:li
        (entry&label->a-tag 'cgi:b "階層機能Bに戻る")
        (html:ul
          (html:li
            "階層機能B:1" "(現在位置)"
            (html:ul
              (html:li
                (entry&label->a-tag 'cgi:b:1:2 "階層機能B:1:2に進む")))))))
    (html:hr)
    (html:p "(まげまげ)")
    (html:hr)
    (entry&label->a-tag 'cgi:menu "メインメニューに戻る")
    ))


(define-cgi-entry (cgi:b:1:2 . keywords) '(("cmd" "b")
                                           ("cmd-b" "1")
                                           ("cmd-b-1" "2"))
  (make-cgi-response
    (html:h1 "階層機能B:1:2")
    (html:ul
      (html:li
        (entry&label->a-tag 'cgi:b "階層機能Bに戻る")
        (html:ul
          (html:li
            (entry&label->a-tag 'cgi:b:1 "階層機能B:1に戻る")
            (html:ul
              (html:li
                "階層機能B:1:2" "(現在位置)"
                ))))))
    (html:hr)
    (html:p "(ぐもぐも)")
    (html:hr)
    (entry&label->a-tag 'cgi:menu "メインメニューに戻る")
    ))


(define-cgi-entry (cgi:param-view . keywords) '(("cmd" "param-view"))
  (make-cgi-response
    (html:h1 "CGIパラメータ確認")
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
    (entry&label->a-tag 'cgi:menu "戻る")
    ))


;; check成功時には#fを返す
;; check失敗時にはerror-msgを返す
(define (check-error-params-for-cgi:reg)
  (let* ((text (or (cgi-get-parameter "text" (cgi-params)) ""))
         (text-length (string-length text))
         )
    (cond
      ((zero? text-length) "文字列が空です")
      ((< 8 text-length) "八文字以内にして下さい")
      (else #f))))

(define-cgi-entry (cgi:reg . keywords) '(("cmd" "reg"))
  (let-keywords* keywords ((error-msg #f)
                           )
    (define (make-internal-html)
      (list
        (html:p
          "登録したい文字列を八文字以内で入力して下さい。"
          (html:br)
          "(空、または八文字を越えるとエラーが出ます)")
        (html:input
          :type "text"
          :name "text"
          :value (or (cgi-get-parameter "text" (cgi-params)) ""))
        (html:input
          :type "submit"
          :value "登録")))

    (make-cgi-response
      (html:h1 "登録")
      (html:p "(登録画面もどきです。実際には何も登録できません)")
      (and/null
        error-msg
        (html:p (html:strong (html-escape-string error-msg))))
      (cgi-entry->form
        'cgi:reg:confirm
        :internal-html (make-internal-html))
      (html:hr)
      (entry&label->a-tag 'cgi:menu "戻る")
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
          "以下の文字列を登録します。"
          "本当によろしければ、登録ボタンを押して下さい。")
        (html:blockquote
          :style "border:1px solid"
          (html:tt
            (html-escape-string text)))
        (cgi-entry->form
          'cgi:reg:submit
          :params `(("text" ,text))
          :submit-label "登録")
        (html:hr)
        (entry&label->a-tag 'cgi:menu "戻る")
        ))))


(define-cgi-entry (cgi:reg:submit . keywords) '(("cmd" "reg")
                                                ("submit" :*)
                                                )
  ;; この辺で、ゴニョゴニョと、登録実行を行う
  (let1 text (cgi-get-parameter "text" (cgi-params))
    (entry->redirect 'cgi:reg:done `(("text" ,text)))))


(define-cgi-entry (cgi:reg:done . keywords) '(("cmd" "reg")
                                              ("done" :*)
                                              )
  (let1 text (cgi-get-parameter "text" (cgi-params))
    (make-cgi-response
      (html:p
        "「"
        (html-escape-string text)
        "」"
        "を、登録しました(実際には、何も登録していませんが)。"
        )
      (html:hr)
      (entry&label->a-tag 'cgi:menu "戻る")
      )))


(define-cgi-entry (cgi:outer . keywords) '(("hoge" :*))
  (make-cgi-response
    (html:h1 "例外的なパラメータ指定にも対応")
    (html:hr)
    (entry&label->a-tag 'cgi:menu "戻る")
    ))


(define-cgi-entry (cgi:menu . keywords) '(("cmd" "menu"))
  (make-cgi-response
    (html:h1 "メインメニュー")
    (html:ul
      (map
        (lambda (l)
          (html:li
            (apply entry&label->a-tag l)))
        '(
          (cgi:a "機能A")
          (cgi:b "階層機能B")
          (cgi:param-view "CGIパラメータ確認" (("aa" "bb") ("cc" "dd" "ee")))
          (cgi:reg "登録")
          (cgi:outer "例外的なパラメータ指定にも対応")
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


