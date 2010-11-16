#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ToDo: ��³�κǽ�ˡ���Ԥ�sexp�ץ�ȥ����ǧ���̿���Ԥ��褦�ˤ����

;;; S��client/server�饤�֥��

;;; �ץ�ȥ���:
;;; - HTTP�Τ褦�ˡ�1�ꥯ�����ȤˤĤ�1�쥹�ݥ󥹤��֤���
;;; - �ꥯ�����Ȥ�쥹�ݥ󥹤⡢ñ���(���Ԥ������Τ�)��S����ġ�
;;; - �ꥯ�����Ȥ��嵭�Υե����ޥåȤ������Ƥ��ʤ�������³�����Ǥ��롣
;;; - ʸ�����󥳡��ǥ��󥰤�ɤΤ褦�˰�������̤�ꡣ
;;; - �Х��ʥ�ǡ������������ˤ��б������������ġš�

;;; ����:

;;; - ����S�������Ф���³�Ǥ��륯�饤����Ȥϡ�����Ū�ˡ�
;;;   ���ѤǤ��륯�饤����ȤΤߤ˴������¤���Ƥ��롢�Ȥ�������Ȥ��롣
;;; -- tcp�ξ���iptables�������¤�������ޤ���unix domain socket��Ȥ�����
;;; - ����S�������Ф���³�Ǥ��륯�饤����ȤȤ��̿��ϡ�����Ū�ˡ�
;;;   ��ʬ�˹�®�Ǥ��롢�Ȥ�������Ȥ��롣
;;; -- ���饤����Ȥ������ʥꥯ�����Ȥ����äƤ������ˡ���ñ��
;;;    �����å����Ƥ��ޤ��Τ��򤱤�٤ˡ����饤����Ȥ���ꥯ�����Ȥ�
;;;    �������ݤˡ������ॢ���Ȥ����ꤹ��ɬ�פ�����١�


(define-module tir.sexp-cs
  (use gauche.parameter)
  (use gauche.net)
  (use gauche.selector)
  (use srfi-1) ; delete
  (use srfi-2) ; and-let*

  (use tir.socket-cs)

  (export
    <sexp-server>
    sexp-server-start
    sexp-server-shutdown

    <sexp-client>
    sexp-client-connect
    sexp-client-disconnect
    request->response
    sexp-connection-live?
    ))
(select-module tir.sexp-cs)


(define-class <sexp-cs> ()
  (
   ;; �ʲ��Τ褦�ʥꥹ�Ȥ���ꤹ�롣
   ;; '(unix "/tmp/hoge.sock" [:reuse-addr? flag])
   ;; '(inet port-number [:reuse-addr? flag])
   ;; '(tcp server-addr port-number [:reuse-addr? flag]) ; �ȼ���ĥ��
   (socket-spec
     :accessor socket-spec-of
     :init-keyword :socket-spec
     :init-value #f) ; ����ɬ��

   ;; �����ѿ��ѥ���å�
   (socket-cs
     :accessor socket-cs-of
     :init-value #f)
   ))

(define-class <sexp-server> (<sexp-cs>)
  (
   (proc
     :accessor proc-of
     :init-keyword :proc
     :init-value #f) ; ����ɬ��
   ;; proc�ϡ������Ȥ��ơ�S��obj������Ϥ����Τǡ�
   ;; �֤��ͤȤ��ơ��ʲ��Τɤ��餫���֤�����
   ;; - (values S��obj bool)
   ;; -- �����ܤ��ͤ����饤����Ȥ˥쥹�ݥ󥹤Ȥ����Ϥ롣
   ;; -- �����ܤ��ͤ�#t�ʤ顢��³�ϰݻ�����롣
   ;; -- �����ܤ��ͤ�#f�ʤ顢��³�����Ǥ���롣
   ;; - S��obj
   ;; -- (values S��obj #t)�ξ�ά����
   (timeout
     :accessor timeout-of
     :init-keyword :timeout
     :init-value 2)

   ;; �����ѿ��ѥ���å�
   (exit-flag ; ��λ�ե饰
     :accessor exit-flag-of
     :init-value #f)
   ))

(define-class <sexp-client> (<sexp-cs>)
  (
   ))



(define-method initialize ((self <sexp-cs>) initargs)
  (next-method)
  (unless (socket-spec-of self)
    (error "this class must be need to :socket-spec"))
  )

(define-method initialize ((self <sexp-server>) initargs)
  (next-method)
  (unless (proc-of self)
    (error "this class must be need to :proc"))
  )

;(define-method initialize ((self <sexp-client>) initargs)
;  (next-method)
;  )


;;; ----


;; ToDo: timeout
;; ToDo: guarder
(define-method sexp-server-start ((self <sexp-server>))
  (letrec (
           (original-current-input-port (current-input-port))
           (original-current-output-port (current-output-port))
           (proc (proc-of self))
           (thunk (lambda ()
                    ;; ToDo: with-error-handler��read-from-string�ˤ���
                    ;;       ������褦�ˤ��٤�����
                    (with-error-handler
                      (lambda (e)
                        ;; ToDo: �����ȥ��顼�����򤹤��
                        (report-error e)
                        #f)
                      (lambda ()
                        ;; ToDo: request��<eof>�Ǥʤ������ǧ�����
                        (let* ((l (read-line))
                               (request (read-from-string l))
                               )
                          ;; l��""�ξ��(���Ԥ������Ƥ���)��
                          ;; proc��¹Ԥ����ˡ����������Ԥ������֤�
                          ;; (ping�����dirty hack)
                          (if (string=? l "")
                            (begin
                              (newline)
                              (flush)
                              (display "debug: ping\n" (current-error-port))
                              #t)
                            (receive r
                              (with-output-to-port
                                original-current-output-port
                                (lambda ()
                                  (with-input-from-port
                                    original-current-input-port
                                    (lambda ()
                                      (proc request)))))
                              (let* (
                                     (single? (= 1 (length r)))
                                     (response (car r))
                                     (keep-connect? (unless single?
                                                      (cadr r)))
                                     )
                                (write response)
                                (newline)
                                (flush)
                                (if (exit-flag-of self)
                                  (begin
                                    ;; ��λ������Ԥ�
                                    ;; ToDo: ¾��ɬ�פʽ�λ������̵������
                                    (socket-server-shutdown
                                      (force d-socket-server))
                                    #f)
                                  keep-connect?)))))))))
           (d-socket-server (delay
                              (make
                                <socket-server>
                                :socket-spec (socket-spec-of self)
                                :thunk thunk)))
           )
    (set! (socket-cs-of self) (force d-socket-server))
    (socket-server-start (force d-socket-server))
    ;; ��λ�����򥳥��˽񤯻�
    (set! (socket-cs-of self) #f)
    ))


(define-method sexp-server-shutdown ((self <sexp-server>))
  ;; �ºݤ�shutdown�ϡ�sexp-server-start����ǹԤ��롣
  (set! (exit-flag-of self) #t))


;; ----


(define-method sexp-client-connect ((self <sexp-client>))
  ;; ���˳����Ƥ�����ϡ���ö�Ĥ��Ƥ��鳫��ľ��
  (when (socket-cs-of self)
    (sexp-client-close self))
  ;; ToDo: connect���ԥ��顼�к�
  (let1 socket-client (make
                        <socket-client>
                        :socket-spec (socket-spec-of self))
    (socket-client-open socket-client)
    (set! (socket-cs-of self) socket-client)))


(define-method sexp-client-disconnect ((self <sexp-client>))
  ;; ToDo: disconnect���ԥ��顼�к�
  (and-let* ((socket-client (socket-cs-of self)))
    (set! (socket-cs-of self) #f)
    (socket-client-close socket-client)
    ))


;; ToDo: ���顼����
(define-method request->response ((self <sexp-client>) request)
  (or
    (and-let* ((socket-client (socket-cs-of self)))
      (with-signal-handlers ((SIGPIPE => #f))
        (lambda ()
          (with-socket-client-io
            socket-client
            (lambda ()
              (let/cc return
                (with-error-handler
                  (lambda (e)
                    (return e))
                  (lambda ()
                    (write/ss request)
                    (newline)
                    (flush)))
                (let1 line (read-line)
                  (with-error-handler
                    (lambda (e)
                      ;; ToDo: ���ȤǤ����Ⱥ���
                      (display "returned: " (current-error-port))
                      (write line (current-error-port))
                      (newline (current-error-port))
                      (report-error e)
                      (error e))
                    (lambda ()
                      (read-from-string line))))))))))
    (error "be connect, first")))


;; ToDo: �̿������˥����åȤξ��֤�Ĵ�٤�����ˤ��롩
(define-method sexp-connection-live? ((self <sexp-client>))
  (and-let* ((socket-client (socket-cs-of self)))
    (with-error-handler
      (lambda (e) #f)
      (lambda ()
        (with-socket-client-io
          socket-client
          (lambda ()
            (newline)
            (flush)
            (equal? "" (read-line))))))))


(provide "tir/sexp-cs")

