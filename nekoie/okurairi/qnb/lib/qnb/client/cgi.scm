;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(define-module qnb.client.cgi
  ;(use gauche.charconv)
  (use gauche.parameter)

  (use srfi-1)

  (use file.util)
  (use util.list)

  (use text.html-lite)
  ;(use text.tree)
  (use www.cgi)
  ;(use www.cgi.dispatch-tir)

  ;(use rfc.uri)

  (use tir03.cgi)
  ;(use tir03.cgi.auth)
  ;(autoload tir03.cgi.auth.typekey <cgi-auth-typekey>)

  ;(use tir03.sdss.client)

  (export
    <client-cgi>
    qnb-main
    ))
(select-module qnb.client.cgi)


;;; --------


(define-class <client-cgi> ()
  (
   ;; settings
   (html-render-keywords
     :accessor html-render-keywords-of
     :init-keyword :html-render-keywords
     :init-value '())
   (data-dir
     :accessor data-dir-of
     :init-keyword :data-dir
     :init-value "/home/nekoie/tmp/qnb/var")
   (cgi-auth-type
     :accessor cgi-auth-type-of
     :init-keyword :cgi-auth-type
     :init-value 'http401)

   ;; internal slots
   ))


(define-method initialize ((self <client-cgi>) initargs)
  (next-method)
  ;; prepare :data-dir
  (unless (file-exists? (data-dir-of self))
    (make-directory* (data-dir-of self)))
  )


;;; --------


(define (delete-keywords delete-key-list target-kv-list)
  (let loop ((left target-kv-list)
             (result '()))
    (if (null? left)
      result
      (let ((now-keyword (car left))
            (now-keyword-value (cadr left))
            (next (cddr left)))
        (loop
          next
          (if (memq now-keyword delete-key-list)
            result
            (list* now-keyword now-keyword-value result)))))))




(define (get-path-info-list)
  ;; ToDo: PATH_INFOではなく、REQUEST_URIとSCRIPT_NAMEから取得するようにする
  ;; ToDo: 日本語コード変換等
  (let1 path-info (cgi-get-metavariable "PATH_INFO")
    (if path-info
      (delete "" (string-split path-info #\/)) ; 空文字列を除去する
      '())))


;;; --------


(define (go-dispatch self c path-info-list)
  #f)


(define (view-page self path-info-list)
  (list
    (cgi-header
      :pragma "no-cache"
      :content-type (string-append
                      "text/html; charset="
                      (symbol->string (gauche-character-encoding)))
      )
    (html:p
      (if (null? path-info-list)
        "path-info not found"
        (hes path-info-list))))
  )


(define-method qnb-main ((self <client-cgi>))
  ;; ToDo: 認証情報の取得
  (cgi-main
    (lambda (params)
      (let ((path-info-list (get-path-info-list))
            (c (cgi-get-parameter "c" params)))
        (if c
          (go-dispatch self c path-info-list)
          (view-page self path-info-list))))
    :on-error cgi-on-error/stack-trace
    ))


;;; --------


(provide "qnb/client/cgi")


