;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; Simple (or S-exp) Data Storage Service server module

;;; ���Υ⥸�塼��ϡ�S������¸����٤Υ⥸�塼��ˡ�
;;; ���ޥ���ǽ�Ȥ��ơ�schemeɾ���郎�Ĥ�����ΤǤ���

;;; note: sdss���饤����Ȥϡ�read�Ǥ��ʤ������θ�������

;;; ToDo: table̾�ϡ����ΤȤ���symbol����ǡ�����򤽤Τޤ�string�ˤ���
;;;       �ȤäƤ��뤬��fsdbm�Τ褦�ˡ����󥳡��ǥ��󥰤�Ԥä������褯�ʤ�����

;;; ToDo: ���ΤȤ���sdss-logging�μ��̤���í��(��٥�Ȥ��ƻȤ��Ƥ���)��
;;;       �����ȥ�����������줹�����

;;; note: ��Ԥ˽񤫤줿��Τϡ�atomic�˼¹Ԥ���롢�Ȥ�����«�Ȥ��롣

;;; ����:
;;; - dbm̾�ϡ�symbol��Ϳ�������ΤȤ��롣
;;; -- �ºݤ�dbm��������ˡ�symbol->string���롣

;;; ToDo: ��å��ե�����(pid�ե�����)����������


(define-module tir03.sdss.server
  (use gauche.parameter)
  (use gauche.net)

  (use srfi-1)
  (use srfi-2) ; and-let*

  (use dbm)
  (use dbm.fsdbm.extend)
  (use dbm.extend)
  (use dbm.queue)
  (use dbm.qdbm)

  (use file.util)
  (use util.list)

  (use tir02.translate) ; �����tir03�˰�ư�����롩

  (use tir03.sandbox)
  (use tir03.socket.server)
  (use tir03.sdss.command)

  (export
    <sdss-server>
    sdss-server:start

    ;; came from gauche.net
    <sockaddr>
    <sockaddr-in>
    <sockaddr-un>
    sockaddr-family
    sockaddr-name
    sockaddr-addr
    sockaddr-port
    ))
(select-module tir03.sdss.server)


;;; --------


(define-macro (string-append-constant . strings)
  (apply string-append strings))


;;; --------


;;; �Х��ʥ�ǡ������������ΰ٤ˡ�ľ��socket���ɤ߽񤭤���ݤ˻��Ѥ���ͽ��
(define socket:reader (make-parameter #f))
(define socket:writer (make-parameter #f))
;;; parameterize��ϡ�'connect�ޤ���'disconnect�Τɤ��餫�Ȥʤ�
(define client-disconnect-flag (make-parameter #f))


;;; --------


(define-class <sdss-server> ()
  (
   ;; settings
   (storage-dir
     :accessor storage-dir-of
     :init-keyword :storage-dir
     :init-value #f)
   (sockaddr
     :accessor sockaddr-of
     :init-keyword :sockaddr
     :init-value #f)
   (dbm-type
     :accessor dbm-type-of
     :init-keyword :dbm-type
     :init-value <qdbm>)
   (verbose-mode
     :accessor verbose-mode-of
     :init-keyword :verbose-mode
     :init-value #t)

   ;; internal slots
   (socket-server
     :accessor socket-server-of
     :init-value #f)
   (ccid->module-table
     :accessor ccid->module-table-of
     :init-form (make-hash-table 'eqv?))
   (dbm-table
     :accessor dbm-table-of
     :init-form (make-hash-table 'eq?))
   ;; dbm-table�ϡ��ʲ��Τ褦��key��value����ġ�
   ;; - key�ϡ�dbm̾��symbol(dbm-open����ݤ�symbol->string�����)
   ;; - value�ϡ�open���줿dbm
   ;; -- dbm-close�����顢����table�����ä�ɬ�פ����������դ���
   ))


(define-method initialize ((self <sdss-server>) initargs)
  (next-method)
  ;; check slot
  (unless (storage-dir-of self)
    (errorf "~a must be need to :storage-dir" (class-name (class-of self))))
  (unless (sockaddr-of self)
    (errorf "~a must be need to :sockaddr" (class-name (class-of self))))
  ;; prepare :storage-dir
  (unless (file-exists? (storage-dir-of self))
    (make-directory* (storage-dir-of self)))
  ;; make :socket-server
  (set!
    (socket-server-of self)
    (make
      <socket-server>
      :sockaddr (sockaddr-of self)
      :verbose-mode (verbose-mode-of self)
      :client-handler socket-server-client-handler
      ))
  )


;;; --------
;;; --------
;;; --------
;;; --------
;;; --------
;;; --------
;;; --------
;;; --------


(define (socket-server-client-handler ccid sockaddr reader writer)
  (define (initialize!)
    ;; ToDo: �����ɤ����뤫�ϡ��ޤ�̤��
    ;;       �Ȥꤢ������tir03.sandbox��Ȥ�
    ;(hash-table-put! (ccid->module-table-of (sdss-server))
    ;                 ccid
    ;                 (make-sandbox-module))
    (sdss-logging ccid 'system "client accepted"))
  (define (finalize!)
    (hash-table-delete! (ccid->module-table-of (sdss-server)) ccid)
    (sdss-logging ccid 'system "client finished"))
  (parameterize ((client-disconnect-flag 'connect)
                 (socket:reader reader)
                 (socket:writer writer))
    ;; �ޤ��ǽ�ˡ�SETP�ץ�ȥ�����ǧ����(handshake)
    ;;;; ToDo: �ޤ�̤����
    ;; �Ȥꤢ�����������å����̤ä��Τǡ���������Ԥ�
    (initialize!)
    (let/cc quit&disconnect
      ;; ư��:
      ;; - ����ư����ˤϡ�(����Ū�ˤ�)�ʲ��Τ褦��response���֤����ˤʤ롣
      ;;   '(���ޥ��̾ ��� . ...)
      ;; - ���顼���ˤϡ��ʲ��Τ褦��response���֤����ˤʤ롣
      ;;   '(error ���顼���� . ...)
      ;; - request����Ĥ�̵��(���Ԥ������Ƥ���)���ϡ����顼���֤���
      ;; - ʣ����request�ν���������ǥ��顼���Ф����ϡ�
      ;;   ����ʹߤ�request��ɾ�����ʤ���
      ;; ToDo: ���̲���ǽ����ʬ�ϡ��ʤ�٤����̲�����(���顼���ɤ�������)
      ;;       (���顼�Ͼ���㳰�֤����ơ��ϥ�ɥ����list�����롢�Ȥ�)
      (define (respond-to-client . responses)
        (with-error-handler
          (lambda (e)
            ;; ��������� = write���� = �̿���ǽ���ʤΤǡ�disconnect���롣
            ;; ToDo: condition�򸫤�褦�ˤ���
            (sdss-logging ccid 'respond-error (ref e 'message))
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
        (let1 line (reader 'read-line #t)
          (when (eof-object? line)
            (sdss-logging ccid 'system "caught #<eof>")
            (quit&disconnect))
          (let1 requests (with-error-handler
                           (lambda (e) e)
                           (lambda ()
                             (call-with-input-string
                               line
                               (lambda (p)
                                 (let next ()
                                   (let1 r (read p)
                                     (if (eof-object? r)
                                       '()
                                       (cons r (next)))))))))
            (cond
              ((is-a? requests <error>)
               (sdss-logging ccid 'invalid-request-line line)
               (respond-to-client
                 (make-error-response (ref requests 'message))))
              ((null? requests)
               (sdss-logging ccid 'invalid-request-line line)
               (respond-to-client
                 (make-error-response "empty request")))
              (else
                (sdss-logging ccid 'request-accepted requests)
                (apply
                  respond-to-client
                  (let next ((left requests))
                    (if (null? left)
                      '()
                      (let1 response (with-error-handler
                                       (lambda (e) e)
                                       (lambda ()
                                         (request->response (car left))))
                        (if (is-a? response <error>)
                          (let1 message (ref response 'message)
                            (sdss-logging ccid 'error message)
                            ;; responses����ü���֤�
                            (list (make-error-response message)))
                          (cons response (next (cdr left))))))))))))
        (next-line)))
    (finalize!)))


;;; --------


(define (request->response request)
  (cond
    ((not (list? request))
     (error "request must be list."))
    ((null? request)
     (error "request must be not empty."))
    (else
      (let ((command (car request))
            (args (cdr request)))
        (let1 func (hash-table-get sdss-command-table command #f)
          (if (not func)
            (errorf
              (string-append-constant
                "command '~s' not found: "
                "you can send to command '(help)' or '(help [command])'.")
              command)
            (receive r (apply func args)
              (cons command r))))))))


;;; --------


(define (sdss-server:open self)
  ;;;; ToDo: ���ư��(�����ƥ�ơ��֥뤬¸�ߤ��ʤ���)�ˡ�
  ;;;;       �����ƥ�ơ��֥�˽���ͤ򥻥åȤ���
  ;;;; ToDo: ��ʬ���Ȥλ��Ȥߤ�Ȥ��褦��ľ��
  (sdss-logging #f 'system "booted")
  )




(define (sdss-server:close self)
  (sdss-logging #f 'system "shutdowned")
  ;; �ޤ������Ƥ���dbm������close���ʤ��ƤϤʤ�ʤ�
  ;; note: sdss-logging���⡢������dbm��ȤäƤ���١�
  ;;       flush�ϰ��ֺǸ�Ǥʤ��ƤϤʤ�ʤ�
  ((with-module tir03.sdss.command sdss:flush-data!)))


;;; --------


(define-method sdss-server:start ((self <sdss-server>))
  ;; ToDo: pid�ե������Ȥäơ���å�����褦�ˤ���
  (parameterize ((sdss-server self))
    (dynamic-wind
      (cut sdss-server:open self)
      (cut socket-server:start (socket-server-of self))
      (cut sdss-server:close self))))


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


;;; external parameter (for command)
(define sdss-client-terminate-request
  (with-module tir03.sdss.command sdss-client-terminate-request))
(define sdss-server
  (with-module tir03.sdss.command sdss-server))


;;; --------


(define sdss:log-append!
  (with-module tir03.sdss.command sdss:log-append!))

(define (sdss-logging ccid log-type loggee)
  (sdss:log-append!
    'sdss-system-log
    'current
    (list
      (tm->yyyymmddhhmmss-bignum (sys-localtime (sys-time)))
      log-type
      ccid
      loggee)))


(define (make-error-response template . format-args)
  ;; note:
  ;; - ���ͤϡ�response��list�κǽ餬����ܥ�'error�Ǥ�����򡢥��顼�Ȥ��롣
  ;; - requests�ν���������ǥ��顼��ȯ��������硢�Ĥ�Ͻ�������ʤ��Ȥ��롣
  (list
    'error
    (if (null? format-args)
      template
      (apply format template format-args))))


;;; --------


(provide "tir03/sdss/server")


