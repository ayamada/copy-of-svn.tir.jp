;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; Simple (or S-exp) Data Storage Service server module

;;; このモジュールは、S式を保存する為のモジュールに、
;;; オマケ機能として、scheme評価器がついたものです。

;;; note: sdssクライアントは、readできない事も考慮する事！

;;; ToDo: table名は、今のところsymbol固定で、それをそのままstringにして
;;;       使っているが、fsdbmのように、エンコーディングを行った方がよくないか？

;;; ToDo: 今のところ、sdss-loggingの種別が出鱈目(ラベルとして使われている)。
;;;       ちゃんとジャンル毎に統一する事！

;;; note: 一行に書かれたものは、atomicに実行される、という約束とする。

;;; 仕様:
;;; - dbm名は、symbolで与えられるものとする。
;;; -- 実際にdbmを引く時に、symbol->stringする。

;;; ToDo: ロックファイル(pidファイル)を生成する


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

  (use tir02.translate) ; 将来はtir03に移動させる？

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


;;; バイナリデータ送受信等の為に、直接socketを読み書きする際に使用する予定
(define socket:reader (make-parameter #f))
(define socket:writer (make-parameter #f))
;;; parameterize後は、'connectまたは'disconnectのどちらかとなる
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
   ;; dbm-tableは、以下のようなkeyとvalueを持つ。
   ;; - keyは、dbm名のsymbol(dbm-openする際にsymbol->stringされる)
   ;; - valueは、openされたdbm
   ;; -- dbm-closeしたら、このtableからも消す必要がある事に注意する
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
    ;; ToDo: コレをどうするかは、まだ未定
    ;;       とりあえず、tir03.sandboxを使う
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
    ;; まず最初に、SETPプロトコルを確認する(handshake)
    ;;;; ToDo: まだ未実装
    ;; とりあえず、チェックに通ったので、前準備を行う
    (initialize!)
    (let/cc quit&disconnect
      ;; 動作:
      ;; - 正常動作時には、(基本的には)以下のようなresponseを返す事になる。
      ;;   '(コマンド名 結果 . ...)
      ;; - エラー時には、以下のようなresponseを返す事になる。
      ;;   '(error エラー内容 . ...)
      ;; - requestが一つも無い(空行が送られてきた)場合は、エラーを返す。
      ;; - 複数のrequestの処理の途中でエラーが出た場合は、
      ;;   それ以降のrequestは評価しない。
      ;; ToDo: 共通化可能な部分は、なるべく共通化する(エラーかどうか、等)
      ;;       (エラーは常に例外返しして、ハンドラ内でlist化する、とか)
      (define (respond-to-client . responses)
        (with-error-handler
          (lambda (e)
            ;; ここに来る = write失敗 = 通信不能、なので、disconnectする。
            ;; ToDo: conditionを見るようにする
            (sdss-logging ccid 'respond-error (ref e 'message))
            (quit&disconnect))
          (lambda ()
            (if (null? responses)
              #t ; responsesが'()なら、空行を返す？それともエラーを返す？
              (let next ((current (car responses))
                         (left (cdr responses)))
                (writer 'write/ss current)
                (unless (null? left)
                  (writer 'display " ")
                  (next (car left) (cdr left)))))
            (writer 'newline)
            (writer 'flush))))
      ;; requestを受け取り、一行ずつ処理していく
      (let next-line ()
        (when (eq? 'disconnect (client-disconnect-flag))
          (quit&disconnect))
        ;; 一行を取得
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
                            ;; responsesの末端を返す
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
  ;;;; ToDo: 初回起動時(システムテーブルが存在しない時)に、
  ;;;;       システムテーブルに初期値をセットする
  ;;;; ToDo: 自分自身の仕組みを使うように直す
  (sdss-logging #f 'system "booted")
  )




(define (sdss-server:close self)
  (sdss-logging #f 'system "shutdowned")
  ;; まだ開いているdbmを全部closeしなくてはならない
  ;; note: sdss-logging等も、内部でdbmを使っている為、
  ;;       flushは一番最後でなくてはならない
  ((with-module tir03.sdss.command sdss:flush-data!)))


;;; --------


(define-method sdss-server:start ((self <sdss-server>))
  ;; ToDo: pidファイルを使って、ロックするようにする
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
  ;; - 仕様は、responseのlistの最初がシンボル'errorである場合を、エラーとする。
  ;; - requestsの処理の途中でエラーが発生した場合、残りは処理されないとする。
  (list
    'error
    (if (null? format-args)
      template
      (apply format template format-args))))


;;; --------


(provide "tir03/sdss/server")


