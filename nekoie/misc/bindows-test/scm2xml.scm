#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; sxml2xml.scm との違いは、file全体をschemeスクリプトとしてevalしてから、
;;; その一番最後の結果をsxmlのtreeとして扱い、
;;; それをxmlに整形して出力する、という点。

;;; note: evalするschemeスクリプトが出力するsxmlには、
;;;       *TOP*や*PI*は含めないようにする事。
;;;       これらはsxml2xml.scmが自動的に付加する。

;;; ToDo: encoding指定/変換


;(use gauche.charconv)
(use sxml.ssax)
(use sxml.tools)
(use file.util)
(use text.tree)


;;; ----


(define (usage&exit)
  (format (standard-error-port)
          "usage: ~a FILENAME\n"
          (sys-basename *program-name*))
  (sys-exit 2))


(define (write-xml-tree xml-tree)
  ;; 今のところ、encoding指定は無し
  (print "<?xml version=\"1.0\"?>")
  (write-tree xml-tree)
  (newline))


(define (main args)
  (with-error-handler
    (lambda (e)
      ;(report-error e)
      (format (standard-error-port) "~a\n" (ref e 'message))
      (usage&exit))
    (lambda ()
      (when (null? (cdr args))
        (usage&exit))
      (let* ((filename (cadr args))
             (sexp-list (file->sexp-list filename))
             )
        (let loop ((left sexp-list))
          (let ((result (eval (car left) (interaction-environment)))
                (next (cdr left)))
            (if (null? next)
              (write-xml-tree
                (sxml:sxml->xml result))
              (loop next)))))))
  0)


