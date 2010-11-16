#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(use gauche.test)

;;; ----

(test-start "tcpcgi.metavariables")

(use tcpcgi.metavariables)

(test-module 'tcpcgi.metavariables)

;;; ----

(define (alist-equal? expected result)
  (define (alist-sort alist)
    (sort
      alist
      (lambda (x y)
        (if (equal? (car x) (car y))
          (compare (cadr x) (cadr y))
          (string<? (car x) (car y))))))
  (equal?
    (alist-sort expected)
    (alist-sort result)))

(define (alist-include? expected result)
  (let/cc return
    (for-each
      (lambda (key&val)
        (let* ((key (car key&val))
               (val (cadr key&val))
               (target (assoc key result))
               )
          (unless (and
                    target
                    (equal? key&val target))
            (return #f))))
      expected)
    (return #t)))

(define (alist-exclude? expected result)
  (not (alist-include? result expected)))

;;; ----

(test-section "make metavariables")

(test* "case"
  '(("GATEWAY_INTERFACE" "CGI/1.1")
    ("SERVER_SOFTWARE" "hoge a")
    ("QUERY_STRING" "")
    )
  (request->metavariables-alist
    :server-software "hoge a"
    )
  alist-equal?)

(test* "case"
  '(
    ("REMOTE_ADDR" "111.222.33.44")
    ("REMOTE_PORT" "12345")
    ("REMOTE_HOST" "hoge.host")
    ("SERVER_ADDR" "55.66.77.88")
    ("SERVER_PORT" "8888")
    )
  (request->metavariables-alist
    :remote-addr "111.222.33.44"
    :remote-port 12345
    :remote-host "hoge.host"
    :server-addr "55.66.77.88"
    :server-port 8888
    )
  alist-include?)

(use rfc.uri)
(test* "case"
  '(
    ("REQUEST_METHOD" "HOGE")
    ("REQUEST_URI" "/hoge?aaa=bbb&ccc=ddd")
    ("SERVER_PROTOCOL" "HTTP/1.2")
    ("QUERY_STRING" "aaa=bbb&ccc=ddd")
    )
  (request->metavariables-alist
    :request-method "HOGE"
    :request-uri "/hoge?aaa=bbb&ccc=ddd"
    :request-protocol "HTTP/1.2"
    :parsed-uri-list (call-with-values
                       (lambda ()
                         (uri-parse "/hoge?aaa=bbb&ccc=ddd"))
                       list)
    )
  alist-include?)

(test* "case"
  '(
    ("HTTPS" "on")
    )
  (request->metavariables-alist
    :https #t
    )
  alist-include?)

(test* "case"
  '(
    ("CONTENT_TYPE" "application/x-hoge")
    ("CONTENT_LENGTH" "1234")
    )
  (request->metavariables-alist
    :request-header '(
                      ("content-type" "application/x-hoge")
                      ("content-length" "1234")
                      )
    )
  alist-include?)

(test* "case"
  '(
    ("HTTP_CONTENT_TYPE" "application/x-hoge")
    ("HTTP_CONTENT_LENGTH" "1234")
    ("HTTP_AUTHORIZATION" "pass")
    ("HTTP_PROXY_AUTHORIZATION" "pass2")
    )
  (request->metavariables-alist
    :request-header '(
                      ("content-type" "application/x-hoge")
                      ("content-length" "1234")
                      ("authorization" "pass")
                      ("proxy-authorization" "pass2")
                      )
    )
  alist-exclude?)

(test* "case"
  '(
    ("HTTP_HOST" "hoge.com")
    ("HTTP_CONNECTION" "Keep-Alive")
    ("HTTP_X_HOGE" "hoge_1")
    )
  (request->metavariables-alist
    :request-header '(
                      ("content-type" "application/x-hoge")
                      ("content-length" "1234")
                      ("host" "hoge.com")
                      ("connection" "Keep-Alive")
                      ("x-hoge" "hoge_1")
                      )
    )
  alist-include?)

(test* "case"
  '(
    ("SERVER_NAME" "hoge.server")
    ("AUTH_TYPE" "digest")
    ("REMOTE_USER" "hogehoge")
    ("PATH_INFO" "/path_hoge")
    ("PATH_TRANSLATED" "/path/to/path_hoge")
    ("SCRIPT_NAME" "/script_hoge")
    )
  (request->metavariables-alist
    :server-name "hoge.server"
    :auth-type "digest"
    :remote-user "hogehoge"
    :path-info "/path_hoge"
    :path-translated "/path/to/path_hoge"
    :script-name "/script_hoge"
    )
  alist-include?)

;;; ----

(test-end)
