;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; setp�����Ф��̿���Ԥ����饤����ȡ�
;;; ���ߤΤȤ������Ʊ���̿��ˤ��б����Ƥ��ʤ���

;;; ToDo: �Ȥ�����񤤤Ƥ�����

;;; ToDo: connect���˥ץ�ȥ�������å���Ԥ������ɤ��ɲä����

;;; ToDo: �̿����ǻ��ν�����ɤ����뤫�ͤ���ɬ�פ�����

;;; ToDo: request->response���μ¹�����connect�����ꤹ�뤫��
;;;       ���ʤ��Ȥ⡢������connect���Ƥ��뤫�Υ����å��ϹԤ���
;;;       Ŭ���ʥ��顼��å�������Ф��褦�ˤϤ�������������


(define-module tir03.setp.client
  (use gauche.net)

  (use srfi-1)

  (use util.list)

  (use tir03.setp.protocol)

  (export
    <setp-client>
    setp-client-connect
    setp-client-disconnect
    setp-client-connected?
    setp-client-connection-is-alive?
    setp-client-reconnect
    request->response
    requests->responses

    ;; came from gauche.net
    <sockaddr>
    <sockaddr-in>
    <sockaddr-un>
    sockaddr-family
    sockaddr-name
    sockaddr-addr
    sockaddr-port
    ))
(select-module tir03.setp.client)


;;; --------
;;; came from gauche.net


(define <sockaddr> <sockaddr>)
(define <sockaddr-in> <sockaddr-in>)
(define <sockaddr-un> <sockaddr-un>)
(define sockaddr-family sockaddr-family)
(define sockaddr-name sockaddr-name)
(define sockaddr-addr sockaddr-addr)
(define sockaddr-port sockaddr-port)


;;; --------


(define-class <setp-client> ()
  (
   ;; settings
   (sockaddr
     :accessor sockaddr-of
     :init-keyword :sockaddr
     :init-value #f)

   ;; internal slots
   (client-socket
     :accessor client-socket-of
     :init-value #f)
   ))


(define-method initialize ((self <setp-client>) initargs)
  (next-method)
  ;; check slot
  (unless (sockaddr-of self)
    (errorf "~a must be need to :sockaddr" (class-name (class-of self))))
  )


;;; --------


(define-method setp-client-connect ((self <setp-client>))
  (when (client-socket-of self)
    (error "already opened" self))
  (set!
    (client-socket-of self)
    (make-client-socket (sockaddr-of self))))


(define-method setp-client-disconnect ((self <setp-client>))
  (unless (client-socket-of self)
    (error "not opened" self))
  ;; ToDo: ��ǽ�ʤ顢flush���Ƥ����������ɤ������Τ�ʤ���
  (guard (e (else #f))
    (socket-shutdown (client-socket-of self) 2))
  (guard (e (else #f))
    (socket-close (client-socket-of self)))
  #t)


(define-method setp-client-connected? ((self <setp-client>))
  (not (not (client-socket-of self))))


(define-method setp-client-connection-is-alive? ((self <setp-client>))
  (and
    (client-socket-of self)
    (with-error-handler
      (lambda (e)
        ;; ToDo:
        ;; ���顼���ˤϡ�close�������������ɤ�������
        ;; ����Ȥ⡢����³������Ԥ��٤�������
        #f)
      (lambda ()
        (let1 identifier (list (sys-time) (sys-getpid))
          (equal? identifier (with-setp-client
                               self
                               (lambda ()
                                 (setp:echo identifier)))))))))


(define-method setp-client-reconnect ((self <setp-client>))
  (guard (e (else #f))
    (setp-client-disconnect self))
  (setp-client-connect self))


;;; --------


(define-method requests->responses ((self <setp-client>) request
                                                         . other-requests)
  ;; ¿�ͤȤ���requests�������ꡢ¿�ͤȤ���responses���֤���
  ;; â����requests������ǥ��顼�����ä����ϡ�
  ;; ����ʹߤ�request��ɾ���ϹԤ鷺���֤äƤ���responses�ο��⸺��١�
  ;; responses��Ĺ��������Ȥʤ롣
  ;; (â�������顼���֤�Τǡ�����Ǥ��Ĥ����Ǥϴޤޤ�Ƥ���)
  ;; ToDo: ��Ŭ����Ԥ�;�Ϥ�����
  (apply
    values
    (let loop ((requests (cons request other-requests)))
      (if (null? requests)
        '()
        (let1 response (request->response self (car requests))
          (if (condition? response)
            (list response)
            (cons
              response
              (loop (cdr requests)))))))))




(define-method request->response ((self <setp-client>) request)
  ;; ToDo: �̿������б���ɬ��
  (guard (e (else e)) ; �����̿������к�
    (let ((in (socket-input-port (client-socket-of self)))
          (out (socket-output-port (client-socket-of self))))
      ;; write/ss���˥��顼���Ф�=���ǡ���Ȧ�ʤΤǡ����顼�㳰�Ϥ��Τޤ��ꤲ��
      ;; ToDo: �̿����顼���ϡ����餫�θ������ԤäƤ���
      ;;       �㳰���ꤲ�������ɤ��ʤ���������
      (write/ss request out)
      (newline out)
      (flush out)
      ;; read-line���˥��顼���Ф�=���ǡ���Ȧ�ʤΤǡ����顼�㳰�Ϥ��Τޤ��ꤲ��
      (let1 line (read-line in)
        (if (eof-object? line)
          (error "disconnected from server")
          (receive (response headers) (guard (e (else (error "read error" e)))
                                        (call-with-input-string
                                          line
                                          (lambda (p)
                                            (let* ((v1 (read p))
                                                   (v2 (read p)))
                                              (values v1 v2)))))
            (cond
              ((eof-object? response)
               (values)) ; ToDo: ¾���֤����ͤ�̵���Τ�������
              ((and
                 (pair? headers)
                 (assoc-ref headers "error"))
               => (cut error <>)) ; ToDo: ��äȤ����ȥ��顼���ۤ��롩
              (else
                response))))))))



;;; --------


(provide "tir03/setp/client")


