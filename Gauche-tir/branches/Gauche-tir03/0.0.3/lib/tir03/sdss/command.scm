;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; Simple (or S-exp) Data Storage Sevice command module

;;; ToDo: �Ȥꤢ���������ޥ�ɤ򽼼¤����ơ��ҤȤޤ��Ȥ�����֤ˤ��褦��

;;; ToDo: sdss-server��������ʬ�򡢤��ȤǴؿ����������

;;; note: �����ȹͤ�������dbm��Ʊ���������ޥ�ɤη�̤�
;;;       readable�Ǥ��뤫�ɤ����ϥ����å����ʤ����ˤ�������
;;; (table:put!�����֤��ͤȤ���#<undef>�����֤��ʤ��褦�ˡ���դ��ʤ�����)
;;; (��������eval���֤��ͤ�#<undef>���Ф뤫�ɤ����ϵ��ˤ��ʤ�)
;;; �ʤΤǡ�sdss���饤����Ȥϡ�read�Ǥ��ʤ������θ�������

;;; ToDo: dbm���Τ򥫥ץ��벽���ơ���������˥���å����ǽ�ˤ�����

;;; dbm�ϡ�����Ū��create��������ǽ������(�ޤ�dbm��¸�ߤ��ʤ�����)
;;; �񤭹��ޤ���ʳ��ǡ�Ŭ�ڤ�dbm���̤�dbm�������������ͤȤ��롣
;;; - â����dbm��¸�ߤ��ʤ�����fetch���褦�Ȥ������ϡ�dbm�Ϻ���ʤ�
;;;   (fallback��(̤��)���֤����)
;;; -- �����餯��'record-not-found����ܥ���֤����ˤʤ�Ȼפ�
;;; -- ��ɡ����ϡ�'error����ܥ���֤��Ƥ���

;;; ToDo: auto-increment�μ���

;;; ToDo: �ơ��֥���ʬ�ϡ����饤����Ȥ����Ȥ�����Ȥ���

;;; ��äѤꡢtable��log��̾�����Ѥ�������
;;; ������������������̾����פ��դ��ޤǤϡ����ΤޤޤȤ��롣

;;; �֤��ͤλ��ͤˤĤ���:
;;; - ���顼�㳰�����ä����ϡ��㳰�򥭥�å����ơ����饤����Ȥ�
;;;   '(error "���顼����")
;;;   ���֤���ΤȤ��롣
;;; - ���顼�㳰���ʤ��ä����ϡ��֤��ͤ�r�Ȥ���ʤ顢
;;;   `(,���ޥ��̾ r)
;;;   ���֤���ΤȤ��롣
;;;   r��¿�ͤξ��ˤ��б��������
;;; ToDo: �嵭�λ��ͤǡ����󥿡��ե���������ľ������

;;; ToDo: ccid�̥⥸�塼�����ʬ���б�

;;; ToDo: ��ͭ��ǽ�ʥ⥸�塼����֤⡢�̤��Ѱդ����������ɤ����롩����
;;;       �Ȥꤢ��������󤷤ǡ�

;;; ToDo: sandbox�⥸�塼������/������ʬ���̥⥸�塼�벽�������

;;; �ץ�ȥ��륳�ޥ��ɽ:
;;; * table
;;; - ͥ���̤ι⤤���
;;; - ���Ȥޤ路
;;; -- sdss:table-update!
;;; -- sdss:table-for-each
;;; -- sdss:table-keys
;;; -- sdss:table-values
;;; - �Ȥꤢ����ư���褦�ˤʤä����
;;; -- sdss:table-get
;;; -- sdss:table-put!
;;; -- sdss:table-delete!
;;; - �����������
;;; * log
;;; - ͥ���̤ι⤤���
;;; - ���Ȥޤ路
;;; -- sdss:log-get-to-list (̾����ͤ����)
;;; -- sdss:log-purge!
;;; -- sdss:log-delete!
;;; -- sdss:log-fold
;;; -- sdss:log-fold-right
;;; -- sdss:log-for-each
;;; -- sdss:log-map
;;; -- sdss:log-find
;;; -- sdss:log-any
;;; -- sdss:log-timestamp
;;; -- sdss:log-cp!
;;; -- sdss:log-mv!
;;; - �Ȥꤢ����ư���褦�ˤʤä����
;;; -- sdss:log-append!
;;; - �����������
;;; * meta
;;; - ͥ���̤ι⤤���
;;; -- sdss:eval
;;; - ���Ȥޤ路
;;; - �Ȥꤢ����ư���褦�ˤʤä����
;;; - �����������
;;; -- sdss:echo
;;; * system
;;; - ͥ���̤ι⤤���
;;; - ���Ȥޤ路
;;; -- sdss:quit
;;; -- sdss:shutdown
;;; -- sdss:server-version
;;; -- sdss:information
;;; -- sdss:system-log-rotate
;;; - �Ȥꤢ����ư���褦�ˤʤä����
;;; -- sdss:help
;;; -- sdss:protocol-version
;;; -- sdss:flush-data!
;;; - �����������


(define-module tir03.sdss.command
  (use gauche.parameter)

  (use srfi-1)
  (use srfi-2) ; and-let*

  (use dbm)
  (use dbm.fsdbm.extend)
  (use dbm.extend)
  (use dbm.queue)
  (use dbm.qdbm)

  (use file.util)
  (use util.list)

  (export
    sdss-protocol-version
    sdss-command-table
    sdss-args-table
    ))
(select-module tir03.sdss.command)


;;; --------


(define sdss-protocol-version "SDSSP/0.1")


;;; --------


(define sdss-client-terminate-request (make-parameter #f))
(define sdss-server (make-parameter #f))


;;; --------


(define sdss-command-table (make-hash-table 'eq?))
(define sdss-args-table (make-hash-table 'eq?))


;;; --------


(define (dbm-symbol-taint-check dbm-symbol)
  ;; ���ΤȤ����ʲ��ξ��ǥ����å����롣
  ;; - /��ޤޤʤ���
  ;; - ..��ޤޤʤ���
  ;; - \x00��ޤޤʤ���
  ;; ToDo: ¾�ˤ�����å����ܤ�ɬ�פ���
  (when (#/\/|\.\.|\x00/ (symbol->string dbm-symbol))
    ;; ToDo: ���顼��å�������ʬ����䤹������
    (error "tainted dbm-symbol" dbm-symbol)))


(define (dbm-symbol->dbm-path dbm-symbol)
  (dbm-symbol-taint-check dbm-symbol)
  (string-append (ref (sdss-server) 'storage-dir)
                 "/"
                 (symbol->string dbm-symbol)))

;;; note: open�˼��Ԥ������ϡ����顼�㳰���ꤲ��
(define (dbm-symbol&dbm-type->dbm dbm-symbol dbm-type)
  (define (get-new-dbm)
    (dbm-open
      dbm-type
      :path (dbm-symbol->dbm-path dbm-symbol)
      :rw-mode :write
      ;; �ʲ�����Ĥϡ��ѹ����뤫���Τ�ʤ�
      :key-convert #t
      :value-convert #t
      ))
  (let1 dbm (hash-table-get (ref (sdss-server) 'dbm-table) dbm-symbol #f)
    (or
      dbm
      (let1 new-dbm (get-new-dbm)
        ;; dbm-table����Ͽ����
        (hash-table-put! (ref (sdss-server) 'dbm-table) dbm-symbol new-dbm)
        new-dbm))))


(define (dbm-symbol&dbm-type-exists? dbm-symbol dbm-type)
  (with-error-handler
    (lambda (e) #f)
    (lambda ()
      (dbm-db-exists? dbm-type (dbm-symbol->dbm-path dbm-symbol)))))


;;; --------


(define-syntax define-sdss-command
  (syntax-rules ()
    ((define-sdss-command (name . args) . body)
     (begin
       (hash-table-put! sdss-args-table 'name 'args)
       (define-sdss-command name (lambda args . body))))
    ((define-sdss-command name expr)
     (begin
       (define name expr)
       (hash-table-put! sdss-command-table 'name name)
       ;; ToDo: �ʲ��ξ���sdss-args-table�ؤ���¸�⡢���Ȥ��ͤ����
       ;(unless (hash-table-exists? sdss-args-table 'name)
       ;  (hash-table-put! sdss-args-table 'name ???))
       ))))


;;; --------


;;; ---- table


(define-sdss-command (sdss:table-get table key . opt-default)
  (if (dbm-symbol&dbm-type-exists? table <qdbm>)
    (let1 dbm (dbm-symbol&dbm-type->dbm table <qdbm>)
      (apply dbm-get dbm key opt-default))
    (if (null? opt-default)
      (error "table not found" table)
      (car opt-default))))


(define-sdss-command (sdss:table-put! table key value)
  (let1 dbm (dbm-symbol&dbm-type->dbm table <qdbm>)
    (dbm-put! dbm key value)
    #t))


(define-sdss-command (sdss:table-delete! table key)
  (let1 dbm (dbm-symbol&dbm-type->dbm table <qdbm>)
    (dbm-delete! dbm key)
    #t))


;;; ---- log


(define-sdss-command (sdss:log-append! table key value)
  (let1 dbm (dbm-symbol&dbm-type->dbm table <fsdbm>)
    (fsdbm-append! dbm key value)
    #t))


;;; ---- meta


;;; ToDo: ccid��Υ⥸�塼����֤�eval����褦��ľ��
(define-sdss-command (sdss:eval sexp)
  (with-error-handler
    (lambda (e)
      (errorf "eval error: ~s" (ref e 'message)))
    (lambda ()
      (eval sexp (interaction-environment)))))


(define-sdss-command (sdss:echo . args)
  (apply values args))


;;; ---- system


(define-sdss-command (sdss:help . opt-command)
  ;; ToDo: ���ꥳ�ޥ�ɤ����ꤵ�줿���ϡ�sdss-args-table��Ȥäơ�
  ;;       ���Υ��ޥ�ɤΰ�����ɽ������
  (hash-table-keys sdss-command-table))
(define-sdss-command (help . opt-command)
  (apply sdss:help opt-command))


(define-sdss-command (sdss:quit)
  (sdss-client-terminate-request #t)
  ;; ToDo: ����Ϥޤ�ư���ʤ������Ȥ�ľ����
  (values))


(define-sdss-command (sdss:protocol-version . opt-version)
  ;; ToDo: opt-version�����ꤵ�졢���줬���ȤΥС������Ȱ��פ��ʤ��ʤ顢
  ;;       ��³�����Ǥ���褦�ˤ��٤�����
  sdss-protocol-version)


(define-sdss-command (sdss:flush-data!)
  ;; ToDo: sdss-server��������ʬ���̴ؿ�������
  (for-each
    (lambda (key)
      (dbm-close (hash-table-get (ref (sdss-server) 'dbm-table) key))
      (hash-table-delete! (ref (sdss-server) 'dbm-table) key))
    (hash-table-keys (ref (sdss-server) 'dbm-table))))


;;; --------


(provide "tir03/sdss/command")


