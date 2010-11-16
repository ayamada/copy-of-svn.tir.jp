;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; �Ȥ���:
;;; :responder�˰ʲ��Τ褦�ʼ�³���򥻥åȤ���setp-server:start���������
;;; - �����Ȥ��ơ�request��request-headers���롣
;;; - �֤��ͤȤ��ơ��ʲ��Τɤ��餫���֤���
;;; -- request���б�����response
;;; -- request���б�����response��response-headers������

;;; ToDo:
;;; :responder��ǥ��顼��ȯ���������ϡ����ΤȤ���
;;; ���ͥ�������ݻ������ޤޡ����顼�إå��դ��ǥ��饤����Ȥ˱�����
;;; ��ưŪ���֤��������λ��ͤ���̯���Ȼפ���
;;; ���ȤǤ�äȥޥ��ʻ��ͤ�ͤ��ơ�ľ������

;;; ToDo: client-disconnect-flag������method���󶡤������
;;;       (socket.server�򸫤��)
;;; ���Ρ��ʲ���method���Ѱդ��������פ�������
;;; - setp-server:shutdown (��)
;;; - setp-server:client-disconnect
;;; - setp-server:pause

;;; note: :verbose-mode��#t�λ��ϡ�STDERR�˾ܺ٥�����Ϥ��롣

;;; ToDo: ��å��ե�����(pid�ե�����)����������


(define-module tir03.setp.server
  (use gauche.parameter)

  (use srfi-1)
  (use srfi-2) ; and-let*

  ;(use file.util)
  (use util.list)

  (use tir03.socket.server)
  (use tir03.setp.protocol)

  (export
    <setp-server>
    setp-server:start
    setp-server:shutdown

    ;;setp-server:client-disconnect
    ;;setp-server:pause

    ;; came from tir03.socket.server
    <sockaddr>
    <sockaddr-in>
    <sockaddr-un>
    sockaddr-family
    sockaddr-name
    sockaddr-addr
    sockaddr-port
    ))
(select-module tir03.setp.server)


;;; --------
;;; came from tir03.socket.server


(define <sockaddr> <sockaddr>)
(define <sockaddr-in> <sockaddr-in>)
(define <sockaddr-un> <sockaddr-un>)
(define sockaddr-family sockaddr-family)
(define sockaddr-name sockaddr-name)
(define sockaddr-addr sockaddr-addr)
(define sockaddr-port sockaddr-port)


;;; --------


(define-macro (string-append-constant . strings)
  (apply string-append strings))


;;; --------


(define-class <setp-server> ()
  (
   ;; settings
   (responder
     :accessor responder-of
     :init-keyword :responder
     :init-value #f) ; ����1��optional����1�����³�������ꤹ�����
   (sockaddr
     :accessor sockaddr-of
     :init-keyword :sockaddr
     :init-value #f)
   (verbose-mode
     :accessor verbose-mode-of
     :init-keyword :verbose-mode
     :init-value #t)

   ;; internal slots
   (socket-server
     :accessor socket-server-of
     :init-value #f)
   ))


(define-method initialize ((self <setp-server>) initargs)
  (next-method)
  ;; check slot
  (unless (responder-of self)
    (errorf "~a must be need to :responder" (class-name (class-of self))))
  (unless (sockaddr-of self)
    (errorf "~a must be need to :sockaddr" (class-name (class-of self))))
  ;; make :socket-server
  (set!
    (socket-server-of self)
    (make
      <socket-server>
      :sockaddr (sockaddr-of self)
      :verbose-mode (verbose-mode-of self)
      :client-handler (lambda (ccid sockaddr reader writer)
                        (socket-server-client-handler self
                                                      ccid
                                                      sockaddr
                                                      reader
                                                      writer))
      ))
  )



;;; parameterize��ϡ�'connect�ޤ���'disconnect�Τɤ��餫�Ȥʤ�
(define client-disconnect-flag (make-parameter #f))




(define (make-error-response template . format-args)
  `(#f
    (("error" ,(if (null? format-args)
                 template
                 (apply format template format-args))))))


(define (socket-server-client-handler self ccid sockaddr reader writer)
  (define (initialize!)
    (setp-logging ccid 'system "client accepted")
    ;; ��³��Ω���ν����������Ԥ�
    #t)
  (define (finalize!)
    (setp-logging ccid 'system "client finished")
    ;; ��³��λ����terminate������Ԥ�
    #t)
  (define (line->requests line)
    ;; line��ѡ������Ƹ��Υꥯ�����Ȥ�list�Ȥ����֤�
    ;; �ꥯ�����Ȥ��������ä����ϡ�<condition>���֤�
    (guard (e (else e))
      (call-with-input-string
        line
        (lambda (p)
          (let next ()
            (let1 r (read p)
              (if (eof-object? r)
                '()
                (cons r (next)))))))))
  (define (requests->response requests)
    ;; note: �������褿�ʳ��ǡ�requests��'()�ǤϤʤ������ݾڤ���Ƥ���(��)
    ;; ���ͤ����list���֤���
    ;; - car�ϡ��׵ᤵ�줿response���Τ�Ρ�
    ;; - cadr�ϡ�rfc.822������alist(�᥿�ǡ���������ݤ˻Ȥ�)
    (let ((request (car requests))
          (request-headers (if (= 1 (length requests))
                             '()
                             (cadr requests))))
      (receive response+alist ((responder-of self) request request-headers)
        (define (get-alist)
          ;; ToDo: ���ȤǤ�������������褦�ˤ����
          (if (= 1 (length response+alist))
                 '()
                 (cadr response+alist)))
        (let ((response (car response+alist))
              (alist (get-alist)))
          (list response alist)))))

  (parameterize ((client-disconnect-flag 'connect))
    ;; �ޤ��ǽ�ˡ�SETP�ץ�ȥ���Ȥ��ΥС��������ǧ����(handshake)
    ;;;; ToDo: �ޤ�̤����
    (initialize!)
    (let/cc quit&disconnect
      (define (respond-to-client responses)
        (with-error-handler
          (lambda (e)
            ;; ��������� = write���� = �̿���ǽ���ʤΤǡ�disconnect���롣
            ;; ToDo: condition�򸫤�褦�ˤ���
            (setp-logging ccid 'respond-error (ref e 'message))
            (quit&disconnect))
          (lambda ()
            (if (null? responses)
              #t ; responses��'()�ʤ顢���Ԥ��֤�������Ȥ⥨�顼���֤���
              (let next ((current (car responses))
                         (left (cdr responses)))
                (writer 'write/ss current)
                (unless (null? left)
                  (writer 'display " ")
                  (next (car left) (cdr left)))))
            (writer 'newline)
            (writer 'flush))))

      ;; request�������ꡢ��Ԥ��Ľ������Ƥ���
      (let next-line ()
        (when (eq? 'disconnect (client-disconnect-flag))
          (quit&disconnect))
        ;; ��Ԥ����
        (let1 line (reader 'read-line #t) ; �Х��ʥ�ǡ����β�ǽ��ͭ�ꡩ
          (when (eof-object? line)
            (setp-logging ccid 'system "caught #<eof>")
            (quit&disconnect))
          (let1 requests (line->requests line)
            (cond
              ((condition? requests)
               ;; �����ꥯ�����Ȥ��ä�
               ;; ��³�ϰݻ������ޤޡ����顼���֤�
               ;; ToDo: ���Ǥ��������ɤ����ɤ����ͤ���
               (setp-logging ccid 'invalid-request-line line)
               (respond-to-client
                 (make-error-response (ref requests 'message))))
              (else
                (setp-logging ccid 'request-accepted requests)
                (guard (e (else
                            (setp-logging ccid
                                          'internal-error
                                          (ref e 'message))
                            (respond-to-client
                              (make-error-response (ref e 'message)))))
                  (respond-to-client (requests->response requests)))))))
        (next-line)))
    (finalize!)))



;;; --------


(define (setp-logging ccid log-type loggee)
  ;; ToDo: �������ѹ�
  ;; ToDo: ���ͤθ�ľ���Ƚ��Ϥμ���
  #f)



(define (setp-server:open self)
  (setp-logging #f 'system "booted")
  ;; (ɬ�פʤ�)�����������Ԥ�
  #t)




(define (setp-server:close self)
  (setp-logging #f 'system "shutdowned")
  ;; (ɬ�פʤ�)terminate������Ԥ�
  #t)



(define-method setp-server:start ((self <setp-server>))
  ;; ToDo: pid�ե������Ȥäơ���å�����褦�ˤ���
  (dynamic-wind
    (cut setp-server:open self)
    (cut socket-server:start (socket-server-of self))
    (cut setp-server:close self)))


(define-method setp-server:shutdown ((self <setp-server>))
  (socket-server:shutdown (socket-server-of self)))

(provide "tir03/setp/server")


