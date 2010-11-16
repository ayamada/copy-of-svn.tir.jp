;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme sw=2 ts=2 et:
;;; $Id$

(define-module tcpcgi.state
  (use gauche.interactive)
  (export
    <tcpcgi.state>
    copy-all-slot!
    clone
    dump-string
    dump-to-error-port
    ))
(select-module tcpcgi.state)


(define-class <tcpcgi.state> ()
  (
   ;; �ǽ餫��ʬ���äƤ�����
   (first-transaction :init-keyword :first-transaction
                      :init-value #t)
   (server-addr :init-keyword :server-addr
                :init-value #f)
   (server-port :init-keyword :server-port
                :init-value #f)
   (server-name :init-keyword :server-name
                :init-value #f)
   (remote-addr :init-keyword :remote-addr
                :init-value #f)
   (remote-port :init-keyword :remote-port
                :init-value #f)
   (remote-host :init-keyword :remote-host
                :init-value #f)
   (https       :init-keyword :https
                :init-value #f)

   ;; ��̾���
   (response-code :init-value #f) ; 200, 302, 500 ���ο��͡�
   (internal-description :init-value #f) ; ������nph�䥨�顼��ͳ���򵭽�
   (connection-close :init-value #f) ; ����쥹�ݥ󥹤��֤��ƤϤ����ʤ�

   ;; ���饤����Ȥ����HTTP�ꥯ�����Ȥξ���
   (request-line :init-value #f) ; "GET / HTTP/1.0" ��
   (request-method :init-value #f)
   (request-uri :init-value #f)
   (request-protocol :init-value #f)
   (parsed-uri-list :init-value #f) ; uri-parse���֤��ͤΥꥹ��
   (http-0.9-flag :init-value #f) ; HTTP/0.9�ʤ鿿
   (request-header :init-value #f) ; request-line��³��rfc822�����Υإå�
   (content-type :init-value #f)
   (content-length :init-value #f)
   (request-body :init-value #f) ; �����ʳ��ǰ�ö�����ɤ߼�äƤ���

   ;; cgi������ץȼ¹Ի���ɬ�פˤʤ����
   (dispatched-server-name :init-value #f) ; HTTP_HOST������vhost̾������
   (dispatched-script-name :init-value #f) ; dispatch��Υ�����ץȤ�path
   (dispatched-path-info :init-value #f) ; dispatch���decode����path-info
   (dispatched-cgi-thunk :init-value #f) ; �¹Ԥ��٤�cgi������ץ�����
   (nph :init-value #f) ; cgi������ץȤ�nphư���Ԥ����ɤ���
   (use-persistent-connection :init-value #f) ; ��³Ū��³��Ԥ����ɤ���
   (auth-type :init-value #f) ; ǧ�ڼ�ˡ��Basic, Digest��
   (auth-name :init-value #f) ; ǧ�ڤ�̾��
   (auth-hash :init-value #f) ; ǧ�ڤ˻Ȥ�hash-table
   (remote-user :init-value #f) ; ǧ�ڻ��Υ桼��̾
   (plain-cgi-metavariables :init-value #f) ; �������줿�᥿�ѿ�
   (merged-cgi-metavariables :init-value #f) ; ����(cgi-metavariables)�ȥޡ���

   ;; cgi������ץȤ���Υ쥹�ݥ󥹤ξ���
   (cgi-response :init-value #f) ; cgi�����Ϥ������Ƥ��Τ��(string)
   (cgi-error-instance :init-value #f) ; ��ª�������顼�㳰���֥�������
   (cgi-response-header :init-value #f) ; cgi���֤�rfc822�����Υإå�
   (cgi-response-body :init-value #f) ; ��Υإå����ä��Ĥ�

   ;; ���饤����Ȥ��֤�HTTP�쥹�ݥ󥹤ξ���
   (response-status-line :init-value #f) ; "HTTP/1.1 200 OK" ��
   (response-header :init-value #f) ; status-line��³��rfc822�����Υإå�
   (response-body :init-value #f) ; ��Υإå����ä��Ĥ�
   ))


(define-method copy-all-slot! ((src <tcpcgi.state>) (dst <tcpcgi.state>))
  (for-each
    (lambda (slot-name)
      (set!
        (ref dst slot-name)
        (ref src slot-name)))
    (map slot-definition-name (class-slots <tcpcgi.state>))))


(define-method clone ((self <tcpcgi.state>))
  (let1 new (make <tcpcgi.state>)
    (copy-all-slot! self new)
    new))


(define-method dump-string ((self <tcpcgi.state>))
  (with-output-to-string
    (lambda ()
      (describe self))))

(define-method dump-to-error-port ((self <tcpcgi.state>))
  (with-output-to-port
    (current-error-port)
    (lambda ()
      (describe self))))


(provide "tcpcgi/state")

