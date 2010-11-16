#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(use gauche.parameter)

(define-module tir.cgi-framework-lite
  (use srfi-2) ; and-let*
  (use rfc.uri)
  (use text.html-lite)
  (use text.tree)
  (use util.list)
  (use www.cgi)

  (use tir.cgi)

  (export
    <cgi-framework-lite>
    cgi-framework-lite-main
    ))
(select-module tir.cgi-framework-lite)


(define-class <cgi-framework-lite> ()
  (
   (title
     :accessor title-of
     :init-keyword :title
     :init-value "(untitled)")
   (css-url
     :accessor css-url-of
     :init-keyword :css-url
     :init-value "http://css.tir.jp/tir.css")
   (body-class
     :accessor body-class-of
     :init-keyword :body-class
     :init-value #f)
   (mail-address
     :accessor mail-address-of
     :init-keyword :mail-address
     :init-value #f)

   (initialize-thunk
     :accessor initialize-thunk-of
     :init-keyword :initialize-thunk
     :init-value #f)
   (main-proc
     :accessor main-proc-of
     :init-keyword :main-proc
     :init-value #f)
   (error-page-thunk
     :accessor error-page-thunk-of
     :init-keyword :error-page-thunk
     :init-value #f)
   ))

(define-method initialize ((self <cgi-framework-lite>) initargs)
  (next-method)
  (unless (main-proc-of self)
    (error "this class must be need to :main-proc"))
  )


;; ----


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





(define (get-default-error-page self)
  (make-page
    self
    (list
      (html:p
        (hes "sorry, this service was temporarily unavailable now.")
        (html:br)
        (hes "please access later."))
      (html:p
        (hes "申し訳ないが、現在、一時的にサービスを停止している。")
        (html:br)
        (hes "また後でアクセスしてくれ。"))
      (let1 e (mail-address-of self)
        (if e
          (list
            (html:hr)
            (html:p
              (html:small
                (hes "(")
                (hes "もし、充分に長い間サービスが停止しているのであれば、")
                (hes "下のボタンを押す事によって、")
                (hes "サービス管理者に催促する事が可能")
                (hes ")")))
            (html:form
              :action (self-url)
              :method "post"
              :target "_blank"
              :style "text-align:center"
              (html:input
                :type "hidden"
                :name "error_c"
                :value "send")
              ;(hes "連絡メッセージ : ")
              ;(html:input
              ;  :type "input"
              ;  :name "error_msg"
              ;  :value "")
              (html:input
                :type "submit"
                :value "催促する")
              ))
          '())))))

(define (get-sended-page self bool)
  (make-page
    self
    (html:h1
      (hes
        (if bool
          "* 催促しました *"
          "* 催促不可能 *")))))

(define (send-mail to header-alist body)
  ;; あとで
  #f)

(define-method cgi-framework-lite-main ((self <cgi-framework-lite>))
  (define (do-initialize)
    (let1 initializer (initialize-thunk-of self)
      (when initializer
        (with-error-handler
          (lambda (e)
            (report-error e)
            #f)
          (lambda ()
            (initializer)
            #t)))))
  (define (do-main)
    (cgi-main
      (lambda (params)
        ((main-proc-of self) params))))
  (define (do-error)
    (cgi-main
      (lambda (params)
        (define (r key)
          (cgi-get-parameter key params))

        (let (
              (error_c (r "error_c"))
              (error_msg (r "error_msg"))
              )
          (if (equal? error_c "send")
            (let1 email (mail-address-of self)
              (get-sended-page
                self
                (and
                  email
                  (send-mail
                    email
                    '()
                    (or error_msg "エラー通知/催促が行われました")))))
            (let1 ept (error-page-thunk-of self)
              (if ept
                (list
                  (cgi-header :pragma "no-cache")
                  (ept))
                (get-default-error-page self))))))))

  (if (do-initialize)
    (do-main)
    (do-error)))


(provide "tir/cgi-framework-lite")


