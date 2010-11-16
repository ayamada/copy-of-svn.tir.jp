#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ��Ĵ����åɷ���client/server�����åȥ饤�֥��

;;; ���Ѿ�����:
;;; - �����å��̿��λ��;塢�����륿���ߥ󥰤��̿������ڤ���ǽ�������롣
;;;   ����Ū�ˤϡ�sigpipe��ή�줿�ꡢread������̤�<eof>���ä��ꡢ
;;;   write����error�㳰��ȯ�������ǽ�������롣
;;;   thunk�ˤϡ�ɬ���������ʥ��к���error�㳰�к���
;;;   �����Υȥ�󥶥������ΰ�������ݤ������

;;; �Ѹ�:
;;; socket-server: ���Υ⥸�塼��ˤ�äƼ¸�����롢tcp-server���λ���
;;; socket-client: ���Υ⥸�塼��ˤ�äƼ¸�����롢tcp-client���λ���
;;; server-socket: tcp��port����listen���Ƥ���socket��
;;; client-socket: ��³��ˤĤʤ��äƤ���socket��
;;;                socket-server��socket-client�⥳����̤��������̿����롣

;;; ToDo: ����꡼�����Ƥ��ʤ����ɤ��������å��������
;;; ToDo: ��³�褫�����Ǥ��졢<eof>���ɤ߽Ф��줿���˼�ưclose������ʤ�̵����


(define-module tir.socket-cs
  (use gauche.parameter)
  (use gauche.net)
  (use gauche.selector)
  (use srfi-1) ; delete
  (use srfi-2) ; and-let*

  (export
    <socket-server>
    socket-server-start
    socket-server-shutdown

    <socket-client>
    socket-client-open
    socket-client-close
    get-socket-client-io
    with-socket-client-io
    ))
(select-module tir.socket-cs)


(define-class <socket-cs> ()
  (
   ;; �ʲ��Τ褦�ʥꥹ�Ȥ���ꤹ�롣
   ;; '(unix "/tmp/hoge.sock" [:reuse-addr? flag])
   ;; '(inet port-number [:reuse-addr? flag])
   ;; '(tcp server-addr port-number [:reuse-addr? flag]) ; �ȼ���ĥ��
   (socket-spec
     :accessor socket-spec-of
     :init-keyword :socket-spec
     :init-value #f) ; ����ɬ��
   ))

(define-class <socket-server> (<socket-cs>)
  (
   (thunk ; �̿����˼¹Ԥ���/�����thunk��
     :accessor thunk-of
     :init-keyword :thunk
     :init-value #f) ; ����ɬ��
   ;; thunk�ϡ�bool�ͤ��֤�����
   ;; #t�ʤ顢���ͥ������ϰݻ����롣
   ;; #f�ʤ顢��³�����Ǥ��롣

   ;; �����ѿ��ѥ���å�
   (exit-flag ; socket-server�ν�λ�ե饰
     :accessor exit-flag-of
     :init-value #f)
   (server-socket
     :accessor server-socket-of
     :init-value #f)
   (client-sockets
     :accessor client-sockets-of
     :init-value '())
   ))

(define-class <socket-client> (<socket-cs>)
  (
   ;; �����ѿ��ѥ���å�
   (client-socket
     :accessor client-socket-of
     :init-value #f)
   ))


(define-method initialize ((self <socket-cs>) initargs)
  (next-method)
  (unless (socket-spec-of self)
    (error "this class must be need to :socket-spec"))
  )
(define-method initialize ((self <socket-server>) initargs)
  (next-method)
  (unless (thunk-of self)
    (error "this class must be need to :thunk"))
  )
;(define-method initialize ((self <socket-client>) initargs)
;  (next-method)
;  )


(define (apply-make-socket proc socket-spec)
  (apply
    proc
    (case (car socket-spec)
      ('tcp
       (cons
         (make
           <sockaddr-in>
           :host (cadr socket-spec)
           :port (caddr socket-spec))
         (cdddr socket-spec)))
      ('unix
       ;; ����socket�ե����뤬¸�ߤ����硢�ä��ʤ���listen�Ǥ��ʤ�
       (sys-unlink (cadr socket-spec))
       socket-spec)
      (else socket-spec))))


;;;;;;;;;

;; ToDo:
;; �Ƽ���ݸ�ϥ�ɥ�
;; fallback-thunk�Ȥ��ơ�client-socket����thunk��������
;; ToDo: �����ʥ��ݸ�
;;       �ɤΥ����ʥ���ݸ�롩
;;       �ݸ��ñ��̵��������������ɤ��Τ���
;;       �����ʥ뤬ή�줿��fallback-thunk��¹Ԥ��٤��ʤΤ���
(define (guarder fallback-thunk thunk)
  (with-error-handler
    (fallback-thunk
      (lambda (e)
        (fallback-thunk))
      (lambda ()
        ;;;;
        (thunk)
        #f))))


;; ----


;;; ToDo: www.cgi���ˡ���³ip���ξ����parameter���󶡤��뤳�ȡ�
;;;       - server-addr
;;;       - server-port
;;;       - client-addr
;;;       - client-port
;;;       unix-domain-socket�λ��ϡ�����
;; ToDo: ����ֳ֤ǡ��̿���̵��client-socket����³�Ǥ�Ƚ�ꤹ��褦�ˤ����
;; ToDo: �����ʥ뤫����ݸ�
(define-method socket-server-start ((self <socket-server>))
  (let ((server-socket
          ;; ToDo: listen�˼��Ԥ������ν�����ͤ���
          (apply-make-socket make-server-socket (socket-spec-of self)))
        (selector (make <selector>))
        )
    ;; server-socket�����ϥ�ɥ�
    (define (accept-handler sock flag)
      (let* ((client-socket (socket-accept server-socket))
             (output (socket-output-port client-socket :buffering :none))
             )
        (set!
          (client-sockets-of self)
          (cons client-socket (client-sockets-of self)))
        ;; client-socket�����ϥ�ɥ����Ͽ
        (selector-add!
          selector
          (socket-input-port client-socket :buffering :none)
          (lambda (input flag)
            ;; ToDo: ���顼�㳰��ª�ΰ٤ˡ�����ϥ�ɥ�ǰϤ�ɬ�פ�����
            ;;       ���㳰��ª���ɬ��client-socket��close�������١�
            (unless (with-output-to-port
                      output
                      (lambda ()
                        (begin0
                          (with-input-from-port
                            input
                            (thunk-of self))
                          (flush))))
              ;; client-socket��shutdown��close����
              (socket-shutdown client-socket 2)
              (socket-close client-socket)
              ;; client-socket�����ϥ�ɥ����
              (selector-delete! selector input #f #f)
              ;; (client-sockets-of self)��������
              (set!
                (client-sockets-of self)
                (delete client-socket (client-sockets-of self)))
              #f))
          '(r))))

    ;; ��������åȤ�server-socket����¸
    (set! (server-socket-of self) server-socket)

    ;; server-socket�����ϥ�ɥ����Ͽ
    (selector-add!
      selector
      (socket-fd server-socket)
      accept-handler
      '(r))

    ;; exit-flag��#t�Ǥʤ���С������֤�
    ;; �����֤���������
    (let loop ()
      (selector-select selector)
      (unless (exit-flag-of self)
        (loop)))

    ;; shutdown��close����
    (socket-shutdown server-socket 2)
    (socket-close server-socket)
    (set! (server-socket-of self) #f)
    ))


(define-method socket-server-shutdown ((self <socket-server>))
  ;; thunk���椫��ƤФ���Ȧ�ˡ�
  ;; thunk�����server-socket���Ĥ��Ƥ��ޤ��ΤϤޤ�����������Τǡ�
  ;; thunk����λ���Ƥ����Ĥ���褦�ˤ���٤ˡ������Ǥ�flag���Τ߹Ԥ���
  ;; �ºݤ�shutdown�ϡ�socket-server-start����ǹԤ��롣
  (set! (exit-flag-of self) #t))


;; ----


(define-method socket-client-open ((self <socket-client>))
  ;; ���˳����Ƥ�����ϡ���ö�Ĥ��Ƥ��鳫��ľ��
  (when (client-socket-of self)
    (socket-client-close self))
  (set!
    (client-socket-of self)
    ;; ToDo: listen�˼��Ԥ��������н�
    (apply-make-socket make-client-socket (socket-spec-of self))))


(define-method socket-client-close ((self <socket-client>))
  ;; ToDo: close�˼��Ԥ��������н�
  ;;       (�����Ф�����³���ڤ�줿������ˡ�socket-shutdown��
  ;;        ���顼�㳰��ȯ�������������)
  (and-let* ((client-socket (client-socket-of self)))
    (set! (client-socket-of self) #f)
    (socket-shutdown client-socket 2)
    (socket-close client-socket)
    ))


(define-method get-socket-client-io ((self <socket-client>))
  (let ((client-socket (client-socket-of self)))
    (if client-socket
      (values
        (socket-input-port client-socket :buffering :none)
        (socket-output-port client-socket :buffering :none))
      (error "must be open client socket, first"))))


(define (with-socket-client-io self thunk)
  (receive (in out)
    (get-socket-client-io self)
    (with-input-from-port
      in
      (lambda ()
        (with-output-to-port
          out
          thunk)))))


(provide "tir/socket-cs")

