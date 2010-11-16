#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(use gauche.test)

;;; ----

(test-start "tcpcgi.request")

(use tcpcgi.request)

(test-module 'tcpcgi.request)

;;; ----

;; keywords :
;; :convert-incomplete-string-uri #t
;; :request-body-caching #t
;; :temporary-file-prefix "hoge."
;; :request-body-on-memory-limit-size 1024

(define (write-result-keywords keywords)
  (display "(")
  (for-each
    (lambda (x)
      (write
        (if (port? x)
          (list x (port->string x))
          x))
      (display " "))
    keywords)
  (display ") "))
(define (t input-target . keywords)
  (with-input-from-string
    (if (list? input-target)
      (string-join
        input-target
        "\r\n"
        'suffix)
      input-target)
    (lambda ()
      (receive (result-keywords message)
        (apply
          stdin->http-request
          keywords)
        ;(write-result-keywords result-keywords)
        message))))

;; result-keywords :
;; :request-body-port input-port
;; :request-header (("host" "hoge.com")
;;                  ("connection" "Keep-Alive")
;;                  ) ; rfc.822
;; :request-line "GET / HTTP/1.1"
;; :request-method "GET"
;; :request-uri "/"
;; :parsed-uri-list (#f #f #f #f "/" #f #f) ; rfc.uri
;; :request-protocol "HTTP/1.1"

;;; ----

(test-section "empty_request")

(test* "case" 'empty_request
  (t ""))

(test* "case" 'empty_request
  (t "\r\n"))


(test-section "HTTP/0.9")

(test* "case" #f
  (t '("GET")))

(test* "case" #f
  (t '("GET /path")))

(test* "case" #f
  (t '("GET /path HTTP/0.9")))


(test-section "HTTP/1.0")

(test* "case" #f
  (t '("GET / HTTP/1.0" "")))


(test-section "HTTP/1.1")

(test* "case" #f
  (t '("GET / HTTP/1.1"
       "Host: hoge.com"
       "")))

(test* "case" #f
  (t '("GET http://hoge.com/abc HTTP/1.1" "")))


(test-section "POST")

(test* "case" #f
  (t '("POST / HTTP/1.0"
       "Content-Type: application/x-www-form-urlencoded"
       "Content-Length: 4"
       ""
       "abcd")))


(test-section "ces convertion")

(use gauche.charconv)
(define *request-string*
  "GET /日本語の文字列 HTTP/1.0\r\n\r\n")
(define (test*-ces enc expected . keywords)
  (test* enc (and
               (not (ces-equivalent? enc (gauche-character-encoding)))
               expected)
    (apply
      t
      (ces-convert *request-string* (gauche-character-encoding) enc)
      keywords)))

(test*-ces "EUC-JP" #f
           :convert-incomplete-string-uri #t)
(test*-ces "Shift_JIS" #f
           :convert-incomplete-string-uri #t)
(test*-ces "iso-2022-jp" #f
           :convert-incomplete-string-uri #t)
(test*-ces "utf-8" #f
           :convert-incomplete-string-uri #t)
(test* "bin" 'abnormal_request_line
  (t '(#*"GET /\x80\x80\x80\x80 HTTP/1.0" "")
     :convert-incomplete-string-uri #t))
(test*-ces "EUC-JP" 'abnormal_request_line
           :convert-incomplete-string-uri #f)
(test*-ces "Shift_JIS" 'abnormal_request_line
           :convert-incomplete-string-uri #f)
(test*-ces "iso-2022-jp" #f ; jisは決して不完全文字列を構築しない
           :convert-incomplete-string-uri #f)
(test*-ces "utf-8" 'abnormal_request_line
           :convert-incomplete-string-uri #f)
(test* "bin" 'abnormal_request_line
  (t '(#*"GET /\x80\x80\x80\x80 HTTP/1.0" "")
     :convert-incomplete-string-uri #f))


(test-section "400 Bad Request")

(test* "case" 'too_many_request_line_parameter
  (t '("GET / HTTP/1.0 hoge" "")))

(test* "case" 'bad_protocol
  (t '("GET / ABC/0.0" "")))

(test* "case" 'bad_path
  (t '("GET hoge HTTP/1.0" "")))

(test* "bad_uri_scheme" 'bad_uri_scheme
  (t '("GET ftp://hoge.com/abc HTTP/1.0" "")))

(test* "no bad_uri_scheme" #f
  (t '("GET http://hoge.com/abc HTTP/1.0" "")))

(test* "no bad_uri_scheme" #f
  (t '("GET https://hoge.com/abc HTTP/1.0" "")))


(test-section "request-body-caching")

(define *request-body-caching-request*
  (string-append
    "POST / HTTP/1.0" "\r\n"
    "Content-Type: application/x-www-form-urlencoded" "\r\n"
    "Content-Length: 4" "\r\n"
    "" "\r\n"
    "abcd" "\r\n"))
(test* "case" #f
  (t *request-body-caching-request*
     :request-body-caching #t))
(test* "case" #f
  (t *request-body-caching-request*
     :request-body-caching #f))



(test-section "request-body-on-file")

(test* "case" #f
  (t '("POST / HTTP/1.0"
       "Content-Type: application/x-www-form-urlencoded"
       "Content-Length: 4"
       ""
       "abcd")
     :temporary-file-prefix "request_test."
     :request-body-on-memory-limit-size 3))
(test* "case" #f
  (t '("POST / HTTP/1.0"
       "Content-Type: application/x-www-form-urlencoded"
       "Content-Length: 4"
       ""
       "abcd")
     :temporary-file-prefix "request_test."
     :request-body-on-memory-limit-size 5))




;;; ----

(test-end)
