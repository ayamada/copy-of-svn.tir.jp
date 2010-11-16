#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ��ʬ�ǽ񤤤�squid�ѤΥ�����쥯��������ץȡ�

;;; usage :
;;; squid.conf�˰ʲ��Τ褦�����ꤹ�롣
;;;
;;; redirect_program /path/to/gosh /path/to/goshredir.scm /path/to/configfile
;;;
;;; configfile�ν񼰤ϼ��Τ褦��S���򲿹Ԥ�񤯡�
;;;   ("http://gs-alpha.tir.jp" "http://127.0.0.1:40000")
;;;   ("http://gs-beta.tir.jp" "http://127.0.0.1:40001")
;;; configfile�ΤɤιԤˤ�ޥå����ʤ��ä���硢http://noexists ���֤���
;;; �����ʥꥯ�����Ȼ��ˤ⡢http://noexists ���֤���
;;;
;;; ��ħ :
;;; - configfile�����������ȡ���ưŪ�˺��ɤ߹��ߤ�Ԥ���
;;; - configfile�Υѡ����˼��Ԥ����顢���Τޤ�ľ����������֤�����Ѥ��롣
;;;   (squid��cache.log�˥ѡ������Ԥ���Ͽ�����)

;;; note : ������쥯������ξܺ٤ˤĤ��Ƥϡ��ʲ��Υ����Ȥ����ͤˤʤ�
;;;        - http://squid.robata.org/faq_15.html

;;; ToDo : �����ʥ������Ԥ�����
;;; ToDo : select��Ȥäơ��ˤˤʤä���ե�����ι��������å���Ԥ��褦�ˤ���


(use gauche.charconv)
(use gauche.regexp)

(use srfi-2)
(use file.util)
(use text.tree)
(use util.list)


(define *debug* #f)


(define (usage self)
  (format #t "usage : ~a configfile\n" self))


(define (ces-convert-*jp incomplete-string)
  (with-error-handler
    (lambda (e) #f)
    (lambda ()
      (ces-convert
        incomplete-string
        (ces-guess-from-string incomplete-string "*JP")))))


(define-syntax string-size-sort-maker
  (syntax-rules ()
    ((_ comparison)
     (lambda (target-list)
       (sort
         target-list
         (lambda (x y)
           (comparison (string-size x) (string-size y))))))))

(define string-size-sort
  (string-size-sort-maker <))
(define string-size-reverse-sort
  (string-size-sort-maker >))

;; �ʲ��Τ褦��regexp����������(���ºݤϤ�����regexp-quote����)
;; #/^(?i:(?:http://hoge.com/aaa)|(?:https://aaa.net:8443/)|(?:...))/
(define (list->re src-list)
  (string->regexp
    (tree->string
      (list
        "^(?i:(?:"
        (intersperse
          ")|(?:"
          (map
            regexp-quote
            (string-size-reverse-sort
              src-list)))
        "))"))))


(define (logging message . params)
  (format
    (current-error-port)
    "~a : "
    (sys-strftime "%Y/%m/%d %T %Z" (sys-localtime (sys-time))))
  (apply
    format
    (current-error-port)
    message
    params)
  (newline (current-error-port)))

;; proc���֤���
;; configfile�Υѡ����˼��Ԥ����饿���ॹ������դ��ǥ��顼����Ϥ���#f���֤�
(define (make-redirector configfile . first-try)
  (with-error-handler
    (lambda (e)
      (logging
        "cannot parsed configfile '~a' ~a\n(~a)"
        configfile
        (if (null? first-try)
          "(be used to old configuration)"
          "!")
        (ref e 'message))
      (unless (null? first-try)
        (exit 1))
      #f)
    (lambda ()
      (let* ((src-alist (file->sexp-list configfile))
             (re (list->re (map car src-alist)))
             (ht (alist->hash-table src-alist 'string=?))
             (proc (lambda (line)
                     (or
                       (and-let* ((str (if (string-incomplete? line)
                                         (ces-convert-*jp line)
                                         line))
                                  (matched (re str))
                                  (key (matched))
                                  (conversion-list (ref ht key #f))
                                  )
                         (string-append
                           (car conversion-list)
                           (matched 'after)))
                       ;; �ޥå����ʤ��ä�
                       "http://noexists"))) ; ����Ū��403��Ф�����
             )
        (when (null? first-try)
          (logging "configfile '~a' was updated. reload." configfile))
        ;; proc���֤�
        (if (not *debug*)
          proc
          (lambda (line)
            (logging "requested '~a'" line)
            (let1 result (proc line)
              (logging "responsed '~a'" result)
              result)))))))


(define (goshredir-main configfile)
  ;; �ǽ�ˡ�configfile��¸�ߥ����å������Ԥ�
  (unless (file-is-readable? configfile)
    (errorf "cannot read configfile '~a'" configfile))

  (let loop ((old-config-mtime (file-mtime configfile))
             (old-redirector (make-redirector configfile 'first-try)))
    (let* ((config-mtime (file-mtime configfile))
           (redirector (if (= config-mtime old-config-mtime)
                            old-redirector
                            (or
                              (make-redirector configfile)
                              old-redirector))))
      ;; �¹Գ���
      (let1 line (read-line (current-input-port) #t)
        (unless (eof-object? line) ; eof�ʤ齪λ
          (print
            (redirector line))
          (loop config-mtime redirector))))))


(define (main args)
  (if (not (= (length args) 2))
    (begin
      (usage (car args))
      1)
    (begin
      (set! (port-buffering (current-output-port)) :line)
      (set! (port-buffering (current-error-port)) :line)
      (goshredir-main (cadr args))
      0)))



