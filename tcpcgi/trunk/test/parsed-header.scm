#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(use gauche.test)

;;; ----

(test-start "tcpcgi.parsed-header")

(use tcpcgi.parsed-header)

(test-module 'tcpcgi.parsed-header)

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

;(test-section "check CGI/1.1->HTTP/1.1")
;
;(test* "case" #f
;  (receive r
;    (cgi/1.1->http/1.1
;      '()
;      (open-input-string "hello\n")
;      )
;    r))
;
;(test-section "check output HTTP/1.1")
;
;(test* "case" #f
;  (receive (response-code status-line response-header body-port)
;    (cgi/1.1->http/1.1
;      '()
;      (open-input-string "hello\n")
;      )
;    (display-http/1.1-header status-line response-header)
;    (copy-port body-port (current-output-port))
;    #f))


;;; ----

(test-end)
