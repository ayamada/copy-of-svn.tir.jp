#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ToDo: まだ作りかけ
;;; ToDo: cmd:*系の関数は、マクロ化して、
;;;       procとhelp文章を楽に定義できるようにしておきたい
;;;       (text.html-liteでやっているように)
;;;       hash-tableも、最初にテーブルだけ定義して、
;;;       マクロが実行された時に毎回addされるようにしたい


(add-load-path "/home/nekoie/Gauche-tir/trunk")
(add-load-path ".")
(use tir.sexp-cs)
(use tir.dbmwl)
(use dbm.qdbm)
(use util.list)

(use srfi-2) ; and-let*

;;; ----

(define *socket-spec* '(unix "/tmp/gs.sock"))

;; アカウントキー -> アカウントid 変換テーブル
(define *key->aid-dbm*
  (make
    <dbmwl>
    :dbm-type <qdbm>
    :dbm-path "tmp/key2aid"))

;; アカウント情報テーブル
(define *account-dbm*
  (make
    <dbmwl>
    :dbm-type <qdbm>
    :dbm-path "tmp/account"))

;; ゲーム実データテーブル
(define *gs-dbm*
  (make
    <dbmwl>
    :dbm-type <qdbm>
    :dbm-path "tmp/gs"))

(define *protocol-version* "gs-tsp/0.1")
(define *protocol-encoding* "EUC-JP")

;;; ----

(define (make-new-gs-creature)
  ;; alistを返す
  #f)
(define (make-new-account-info gscid)
  ;; alistを返す
  #f)
(define (make-new-account-key)
  ;; 文字列を返す
  ;; 適当に、まだ存在していないキー値を返すだけ
  #f)

;;; ----

(define (cmd:fallback account-info . request-params)
  (list 'invalid-cmd "you can get valid cmd from 'help' cmd."))

;; cmd-listという別のコマンドを用意した方がいい気がしてきた…‥
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
  ;; まだ作ってない
  #f)

(define (cmd:action account-info . request-params)
  ;; まだ作ってない
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
    `(create-agent . ,cmd:create-agent) ; commonのみ
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
     (ss-proc (list #f request))) ; 整形してやり直し
    ((eqv? 1 (length request))
     (ss-proc (cons #f request))) ; 整形してやり直し
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


