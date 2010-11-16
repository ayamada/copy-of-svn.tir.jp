;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
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
   ;; �ǽ�˥��åȤ������
   (counter     :init-keyword :counter
                :init-value 0) ; persistent-connection�β����ܤν�����
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

   ;; ��̾���ʥ��󥰵ڤ�ư�������ѡ�
   (response-code :init-value #f) ; 200, 302, 500 ���ο��͡�
   (internal-description :init-value #f) ; ������nph�䥨�顼��ͳ���򵭽�
   (error-instance :init-value #f) ; ��ª�������顼�㳰���󥹥���
   (connection-close :init-value #f) ; ���˱�³Ū��³��Ԥ�ʤ�/�Ԥ��ʤ�

   ;; ���饤����Ȥ����HTTP�ꥯ�����Ȥξ���
   (request-keywords :init-value #f) ; see tcpcgi.request

   ;; �ǥ����ѥå���̾���
   (dispatched-server-name :init-value #f)
   (dispatched-plain-path-info :init-value #f)
   (dispatched-script-name :init-value #f)
   (dispatched-executee :init-value #f)

   ;; cgi�᥿�ѿ�����
   (cgi-metavariables :init-value #f) ; cgi-thunk�¹Ի�����parameterize����

   ;; executee��­����
   (lazy-execute :init-value #f) ; �ٱ�¹�/nphư���Ԥ����ɤ���

   ;; cgi������ץȤ���Υ쥹�ݥ󥹤ξ���
   ;; execute�塢cgi/1.1-response �ޤ��ϡ�
   ;; (cons response-code response-keywords) ���֤롣
   (cgi/1.1-response :init-value #f) ; cgi�����Ϥ���CGI/1.1���������Ƥ�ʸ����
   (response-keywords :init-value #f) ; cgi�����Ϥ���response-keywords

   ;; �쥹�ݥ󥹤�connection�إå�
   (response-connection :init-value #f) ; #f or "close" or "Keep-Alive"

   ;; cgi/1.1-response����Ϥ������
   (cgi-response-header :init-value #f) ; rfc822�����Υإå���ʬ
   (cgi-response-body :init-value #f) ; �إå����ä��Ĥ��body��ʬ
   (http/1.1-status-line :init-value #f) ; HTTP/1.1 status-line
   (http/1.1-header :init-value #f) ; HTTP/1.1 response-header(alist)
   (http/1.1-body-port :init-value #f) ; HTTP/1.1 response-body(input-port)
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

