;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; sdss���饤����ȥ⥸�塼��

;;; ToDo: �ɤΤ褦�ˤ��롩����
;;; - �Ȥꤢ������dbm�����ǹԤ����ˤ��Ƥߤ롣�����Ѳ�ǽ������
;;; - �����ϡ������³���������ˤ��롩

;;; note:
;;; - ���Υ⥸�塼��ϡ�sdss:table-get���δؿ����󶡤��롩����
;;; -- �Ȥꤢ������Ʃ��Ū�˻Ȥ���褦�ˤ������������Ǥ���С������������ʰ١�


(define-module tir03.sdss.client
  (use gauche.parameter)
  (use gauche.net)

  (use srfi-1)

  (use tir03.sdss.command)

  (export
    <sdss-client>
    sdss-client-connect
    sdss-client-disconnect
    sdss-client-connected?
    sdss-client-connection-is-alive?
    sdss-client-reconnect
    with-sdss-client

    ;; came from gauche.net
    <sockaddr>
    <sockaddr-in>
    <sockaddr-un>
    sockaddr-family
    sockaddr-name
    sockaddr-addr
    sockaddr-port
    ))
(select-module tir03.sdss.client)


;;; --------


(define sdss-client (make-parameter #f))


;;; --------


(define-class <sdss-client> ()
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


(define-method initialize ((self <sdss-client>) initargs)
  (next-method)
  ;; check slot
  (unless (sockaddr-of self)
    (errorf "~a must be need to :sockaddr" (class-name (class-of self))))
  )


;;; --------


(define-method sdss-client-connect ((self <sdss-client>))
  (when (client-socket-of self)
    (error "already opened" self))
  (set!
    (client-socket-of self)
    (make-client-socket (sockaddr-of self))))


(define-method sdss-client-disconnect ((self <sdss-client>))
  (unless (client-socket-of self)
    (error "not opened" self))
  ;; ToDo: ��ǽ�ʤ顢flush���Ƥ����������ɤ������Τ�ʤ���
  (guard (e (else #f))
    (socket-shutdown (client-socket-of self) 2))
  (guard (e (else #f))
    (socket-close (client-socket-of self)))
  #t)


(define-method with-sdss-client ((self <sdss-client>) thunk)
  (parameterize ((sdss-client self))
    (thunk)))


(define-method sdss-client-connected? ((self <sdss-client>))
  (not
    (not
      (client-socket-of self))))


(define-method sdss-client-connection-is-alive? ((self <sdss-client>))
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
          (equal? identifier (with-sdss-client
                               self
                               (lambda ()
                                 (sdss:echo identifier)))))))))


(define-method sdss-client-reconnect ((self <sdss-client>))
  (guard (e (else #f))
    (sdss-client-disconnect self))
  (sdss-client-connect self))


;;; --------


;; list��Ĺ�����֤����Դ���list�ξ��ϡ���������ʬ�Τߤ�Ĺ�����֤���
(define (length* target)
  (let loop ((result 0)
             (left target))
    (if (pair? left)
      (loop
        (+ result 1)
        (cdr left))
      result)))


;;; �ºݤΡ����ޥ�ɼ¹Դؿ�(��sdss���ޥ�ɴؿ��ϡ�����Υ�åѡ�)
(define (command-execute command-symbol args)
  (let1 sdss-args (hash-table-get sdss-args-table command-symbol)
    (define (args-is-valid?)
      (if (proper-list? sdss-args)
        (= (length sdss-args) (length args))
        (<= (length* sdss-args) (length args))))
    ;; �ޤ������������å���Ԥ�
    (unless (args-is-valid?)
      (errorf
        "wrong number of arguments for ~s (required ~d, got ~d)"
        command-symbol
        (length* sdss-args)
        (length args)))
    ;; with-sdss-client�⤫�ɤ���������å�
    (unless (sdss-client)
      (errorf "~s must be use into with-sdss-client" command-symbol))
    (let ((in (socket-input-port (client-socket-of (sdss-client))))
          (out (socket-output-port (client-socket-of (sdss-client)))))
      ;; ���ΤȤ���Ʊ����ʣ���Υ��ޥ�ɤ�¹Ԥ��뵡ǽ��̵��
      ;; write/ss���˥��顼���Ф�=���ǡ���Ȧ�ʤΤǡ����顼�㳰�Ϥ��Τޤ��ꤲ��
      ;; ToDo: �̿����顼���ϡ����餫�θ������ԤäƤ���
      ;;       �㳰���ꤲ�������ɤ��ʤ���������
      (write/ss (cons command-symbol args) out)
      (newline out)
      (flush out)
      ;; read-line���˥��顼���Ф�=���ǡ���Ȧ�ʤΤǡ����顼�㳰�Ϥ��Τޤ��ꤲ��
      (let1 line (read-line in)
        (if (eof-object? line)
          (error "disconnected from server")
          (let1 result (with-error-handler
                         (lambda (e) e)
                         (lambda ()
                           ;; note: ���ΤȤ���ñ�ȥ��ޥ�ɼ¹ԡ��Ȥ�������
                           (read-from-string line)))
            (cond
              ((is-a? result <error>)
               (error "read error" result))
              ((not (pair? result))
               (error "invalid response accepted" result))
              ((eq? 'error (car result))
               (error (cdr result)))
              (else
                (apply values (cdr result))))))))))


;;; sdss-command-table��sdss-args-table���顢��åѡ��ؿ�����������export����
(for-each
  (lambda (command-symbol)
    (eval
      ;; ToDo: eval���ʤ�٤�����������(�ºݤν�����eval�γ����ɤ��Ф�)
      `(begin
         (define (,command-symbol . args)
           ;; ���������ä��Ȥ������ϡ������ο��Ϲ�äƤ���Τǡ��¹�
           ;; (�����ο������꤬������ϡ��̾�δؿ���Ʊ�ͤˡ�
           ;;  ���顼�㳰���ꤲ������)
           (,command-execute ',command-symbol args))
         ;; ����塢export
         (export ,command-symbol))
      (current-module)))
  (hash-table-keys sdss-command-table))


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


(provide "tir03/sdss/client")


