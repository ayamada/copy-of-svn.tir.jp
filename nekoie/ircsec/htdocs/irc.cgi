#!/usr/local/gauche-0.8.14/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ircsec��Ϣ�Ȥ��Ƶ�ǽ���롢irc��ɽ��cgi
;;; �ʲ��ε�ǽ���׵ᤵ���
;;; - ǧ�ڤ��줿�桼���Τߤ��оݤȤ���
;;; -- ǧ����ˡ�ϡ������å�����
;;; - irc�Υ���ɽ������
;;; -- ����ñ��ζ�Ĵ��ǽ�դ�
;;; -- ���������Ӥ��������ǽ�դ�
;;; - ����ǻȤ��Τǡ�����ޤ꿿���ܤ˺�����ɬ�פϤʤ�

;;; ircsec¦�ǤΥ��󥿡��ե������ϡ��ʲ��Τ褦�ˤʤ�ͽ��
;;; - ���Ȥ�

;;; TODO: ���󥿡��ե����������ɬ�פ�����
;;; ɬ�פˤʤ����ϰʲ����̤�
;;; (â�������Ƥ�url��ͳ���Ϥ�ɬ�פϤʤ������������åȷ�ͳ���Ϥ��Ƥ�褤)
;;; - ǧ�ھ���
;;; - ͭ�����¾���
;;; - ɽ���������
;;; -- �ϡ��ɥ�ߥåȤ�ǧ�ھ�������ꤷ��
;;;    ���եȥ�ߥåȤ�cgi�ѥ�᡼���ǻ��ꤹ��
;;;    (�������ɤ����˥��եȥ�ߥå��ѹ��Υե������ɽ��������)
;;; - ��Ĵʸ�������

;;; TODO: file.atomic�⥸�塼�����
;;; (�����ͽ��Ǥ�wdsm�򥹥ȥ졼���ˤ���ͽ����ä������ɤ���
;;;  �������Ӥˤϸ����ʤ�����Ƚ�������Τǡ�
;;;  ����ˡ�file.atomic��Ȥ����������queue��Ͽ���ơ�
;;;  ���ѤȤ�����ˤ���)


(use gauche.parameter)
(use www.cgi)

(define *base-dir* "/home/nekoie/ircsec")
(define *log-dir* "logs")


;;; ----


;;; ----


(define (get-paths)
  (cgi-get-metavariable "PATH_INFO")
  ...)


(define (main args)
  (parameterize ((cgi-output-character-encoding "Shift_JIS"))
    (cgi-main
      (lambda (params)
        (let1 paths (get-paths)
          ...)))))



