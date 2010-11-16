;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; Simple (or S-exp) Data Storage Sevice command module

;;; ToDo: とりあえず、コマンドを充実させて、ひとまず使える状態にしよう！

;;; ToDo: sdss-serverを操作する部分を、あとで関数化する事。

;;; note: 色々と考えたが、dbmと同じく、コマンドの結果が
;;;       readableであるかどうかはチェックしない事にしたい。
;;; (table:put!等の返り値として#<undef>等を返さないように、注意しながら作る)
;;; (しかし、evalの返り値に#<undef>が出るかどうかは気にしない)
;;; なので、sdssクライアントは、readできない事も考慮する事！

;;; ToDo: dbm自体をカプセル化して、将来メモリ上にキャッシュ可能にしたい

;;; dbmは、明示的にcreateする事も可能だが、(まだdbmが存在しない場合は)
;;; 書き込まれる段階で、適切なdbm種別でdbmが生成される仕様とする。
;;; - 但し、dbmが存在しない時にfetchしようとした場合は、dbmは作られない
;;;   (fallback値(未定)が返される)
;;; -- おそらく、'record-not-foundシンボルを返す事になると思う
;;; -- 結局、今は、'errorシンボルを返している

;;; ToDo: auto-incrementの実装

;;; ToDo: テーブル部分は、クライアントからも使う前提とする

;;; やっぱり、tableとlogの名前を変えたい？
;;; しかし、気の利いた名前を思い付くまでは、このままとする。

;;; 返り値の仕様について:
;;; - エラー例外があった場合は、例外をキャッチして、クライアントに
;;;   '(error "エラー内容")
;;;   を返すものとする。
;;; - エラー例外がなかった場合は、返り値をrとするなら、
;;;   `(,コマンド名 r)
;;;   を返すものとする。
;;;   rが多値の場合にも対応する事。
;;; ToDo: 上記の仕様で、インターフェースを作り直す事。

;;; ToDo: ccid別モジュール空間分割対応

;;; ToDo: 共有可能なモジュール空間も、別に用意したいが、どうする？？？
;;;       とりあえず、後回しで。

;;; ToDo: sandboxモジュール生成/管理部分は別モジュール化する事。

;;; プロトコルコマンド表:
;;; * table
;;; - 優先順位の高いもの
;;; - あとまわし
;;; -- sdss:table-update!
;;; -- sdss:table-for-each
;;; -- sdss:table-keys
;;; -- sdss:table-values
;;; - とりあえず動くようになったもの
;;; -- sdss:table-get
;;; -- sdss:table-put!
;;; -- sdss:table-delete!
;;; - 完成したもの
;;; * log
;;; - 優先順位の高いもの
;;; - あとまわし
;;; -- sdss:log-get-to-list (名前を考える事)
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
;;; - とりあえず動くようになったもの
;;; -- sdss:log-append!
;;; - 完成したもの
;;; * meta
;;; - 優先順位の高いもの
;;; -- sdss:eval
;;; - あとまわし
;;; - とりあえず動くようになったもの
;;; - 完成したもの
;;; -- sdss:echo
;;; * system
;;; - 優先順位の高いもの
;;; - あとまわし
;;; -- sdss:quit
;;; -- sdss:shutdown
;;; -- sdss:server-version
;;; -- sdss:information
;;; -- sdss:system-log-rotate
;;; - とりあえず動くようになったもの
;;; -- sdss:help
;;; -- sdss:protocol-version
;;; -- sdss:flush-data!
;;; - 完成したもの


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
  ;; 今のところ、以下の条件でチェックする。
  ;; - /を含まない事
  ;; - ..を含まない事
  ;; - \x00を含まない事
  ;; ToDo: 他にもチェック項目が必要か？
  (when (#/\/|\.\.|\x00/ (symbol->string dbm-symbol))
    ;; ToDo: エラーメッセージを分かりやすくする
    (error "tainted dbm-symbol" dbm-symbol)))


(define (dbm-symbol->dbm-path dbm-symbol)
  (dbm-symbol-taint-check dbm-symbol)
  (string-append (ref (sdss-server) 'storage-dir)
                 "/"
                 (symbol->string dbm-symbol)))

;;; note: openに失敗した場合は、エラー例外を投げる
(define (dbm-symbol&dbm-type->dbm dbm-symbol dbm-type)
  (define (get-new-dbm)
    (dbm-open
      dbm-type
      :path (dbm-symbol->dbm-path dbm-symbol)
      :rw-mode :write
      ;; 以下の二つは、変更するかも知れない
      :key-convert #t
      :value-convert #t
      ))
  (let1 dbm (hash-table-get (ref (sdss-server) 'dbm-table) dbm-symbol #f)
    (or
      dbm
      (let1 new-dbm (get-new-dbm)
        ;; dbm-tableに登録する
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
       ;; ToDo: 以下の場合のsdss-args-tableへの保存も、何とか考える事
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


;;; ToDo: ccid毎のモジュール空間でevalするように直す
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
  ;; ToDo: 特定コマンドが指定された場合は、sdss-args-tableを使って、
  ;;       そのコマンドの引数を表示する
  (hash-table-keys sdss-command-table))
(define-sdss-command (help . opt-command)
  (apply sdss:help opt-command))


(define-sdss-command (sdss:quit)
  (sdss-client-terminate-request #t)
  ;; ToDo: これはまだ動かない！あとで直す。
  (values))


(define-sdss-command (sdss:protocol-version . opt-version)
  ;; ToDo: opt-versionが指定され、それが自身のバージョンと一致しないなら、
  ;;       接続を切断するようにすべきか？
  sdss-protocol-version)


(define-sdss-command (sdss:flush-data!)
  ;; ToDo: sdss-serverを操作する部分は別関数化する
  (for-each
    (lambda (key)
      (dbm-close (hash-table-get (ref (sdss-server) 'dbm-table) key))
      (hash-table-delete! (ref (sdss-server) 'dbm-table) key))
    (hash-table-keys (ref (sdss-server) 'dbm-table))))


;;; --------


(provide "tir03/sdss/command")


