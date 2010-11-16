#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ToDo: �ޤ���꤫��
;;; ToDo: cmd:*�Ϥδؿ��ϡ��ޥ������ơ�
;;;       proc��helpʸ�Ϥ�ڤ�����Ǥ���褦�ˤ��Ƥ�������
;;;       (text.html-lite�Ǥ�äƤ���褦��)
;;;       hash-table�⡢�ǽ�˥ơ��֥����������ơ�
;;;       �ޥ����¹Ԥ��줿�������add�����褦�ˤ�����


(add-load-path "/home/nekoie/Gauche-tir/trunk")
(add-load-path ".")
(use tir.sexp-cs)
(use tir.dbmwl)
(use dbm.qdbm)
(use util.list)

(use srfi-2) ; and-let*

;;; ----

(define *socket-spec* '(unix "/tmp/gs.sock"))

;; ��������ȥ��� -> ���������id �Ѵ��ơ��֥�
(define *key->aid-dbm*
  (make
    <dbmwl>
    :dbm-type <qdbm>
    :dbm-path "tmp/key2aid"))

;; ��������Ⱦ���ơ��֥�
(define *account-dbm*
  (make
    <dbmwl>
    :dbm-type <qdbm>
    :dbm-path "tmp/account"))

;; ������¥ǡ����ơ��֥�
(define *gs-dbm*
  (make
    <dbmwl>
    :dbm-type <qdbm>
    :dbm-path "tmp/gs"))

(define *protocol-version* "gs-tsp/0.1")
(define *protocol-encoding* "EUC-JP")

;;; ----

(define (make-new-gs-creature)
  ;; alist���֤�
  #f)
(define (make-new-account-info gscid)
  ;; alist���֤�
  #f)
(define (make-new-account-key)
  ;; ʸ������֤�
  ;; Ŭ���ˡ��ޤ�¸�ߤ��Ƥ��ʤ������ͤ��֤�����
  #f)

;;; ----

(define (cmd:fallback account-info . request-params)
  (list 'invalid-cmd "you can get valid cmd from 'help' cmd."))

;; cmd-list�Ȥ����̤Υ��ޥ�ɤ��Ѱդ������������������Ƥ����ġ�
(define (cmd:help account-info . request-params)
  (if (null? request-params)
    (list
      'cmd-list
      (hash-table-keys
        (if account-info
          *civil-cmd-table*
          *common-cmd-table*)))
    (list 'help "this feature is unavailable yet.")))

(define (cmd:ping account-info . request-params)
  (cons 'pong request-params))

(define (cmd:version account-info . request-params)
  (list 'version *protocol-version*))

(define (cmd:encoding account-info . request-params)
  (list 'encoding *protocol-encoding*))

(define (cmd:is-key-valid? account-info . request-params)
  (list 'is-key-valid? (not (not account-info))))


(define (cmd:create-agent account-info . request-params)
  (let* ((gs-creature (make-new-gs-creature))
         (gscid (dbm-auto-increment-insert
                  *gs-dbm*
                  "gs-creature:~8,'0d"
                  gs-creature))
         (account-info (make-new-account-info gscid))
         (aid (dbm-auto-increment-insert
                *account-dbm*
                "account-info:~8,'0d"
                account-info))
         (account-key (make-new-account-key))
         )
    (dbm-put! *key->aid-dbm* account-key aid)
    (list 'created account-key)))

(define (cmd:view account-info . request-params)
  ;; �ޤ���äƤʤ�
  #f)

(define (cmd:action account-info . request-params)
  ;; �ޤ���äƤʤ�
  #f)

;;; ----

(define *common-cmd-table*
  (hash-table
    'eq?
    `(help . ,cmd:help)
    `(ping . ,cmd:ping)
    `(version . ,cmd:version)
    `(encoding . ,cmd:encoding)
    `(is-key-valid? . ,cmd:is-key-valid?)
    `(create-agent . ,cmd:create-agent) ; common�Τ�
    `(view . ,cmd:view)
    ))
(define *civil-cmd-table*
  (hash-table
    'eq?
    `(help . ,cmd:help)
    `(ping . ,cmd:ping)
    `(version . ,cmd:version)
    `(encoding . ,cmd:encoding)
    `(is-key-valid? . ,cmd:is-key-valid?)
    `(view . ,cmd:view)
    `(action . ,cmd:action)
    ))

;;; ----

(define (ss-proc request)
  (cond
    ((not (list? request))
     (ss-proc (list #f request))) ; �������Ƥ��ľ��
    ((eqv? 1 (length request))
     (ss-proc (cons #f request))) ; �������Ƥ��ľ��
    (else
      (let* ((account-key (car request))
             (request-cmd (cadr request))
             (request-params (cddr request))
             (aid (dbm-get *key->aid-dbm* account-key #f))
             (account-info (dbm-get *account-dbm* aid #f))
             )
        (values
          (apply
            (hash-table-get
              (if account-info
                *civil-cmd-table*
                *common-cmd-table*)
              request-cmd
              cmd:fallback)
            account-info
            request-params)
          #t)))))

;;; ----

(define *sexp-server*
  (make
    <sexp-server>
    :socket-spec *socket-spec*
    :proc ss-proc
    ))

(define (main args)
  (sexp-server-start *sexp-server*)
  0)


