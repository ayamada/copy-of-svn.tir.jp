#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; sxml2xml.scm �Ȥΰ㤤�ϡ�file���Τ�scheme������ץȤȤ���eval���Ƥ��顢
;;; ���ΰ��ֺǸ�η�̤�sxml��tree�Ȥ��ư�����
;;; �����xml���������ƽ��Ϥ��롢�Ȥ�������

;;; note: eval����scheme������ץȤ����Ϥ���sxml�ˤϡ�
;;;       *TOP*��*PI*�ϴޤ�ʤ��褦�ˤ������
;;;       ������sxml2xml.scm����ưŪ���ղä��롣

;;; ToDo: encoding����/�Ѵ�


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
  ;; ���ΤȤ���encoding�����̵��
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


