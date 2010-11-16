#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ToDo: 接続の最初に、一行でsexpプロトコル確認の通信を行うようにする事

;;; S式client/serverライブラリ

;;; プロトコル:
;;; - HTTPのように、1リクエストにつき1レスポンスを返す。
;;; - リクエストもレスポンスも、単一行(改行は末尾のみ)のS式一つ。
;;; - リクエストが上記のフォーマットを構成していない場合は接続を切断する。
;;; - 文字エンコーディングをどのように扱うかは未定。
;;; - バイナリデータの送受信にも対応させたいが…‥。

;;; 仕様:

;;; - このS式サーバに接続できるクライアントは、基本的に、
;;;   信用できるクライアントのみに既に制限されている、という前提とする。
;;; -- tcpの場合はiptables等で制限する事。またはunix domain socketを使う事。
;;; - このS式サーバに接続できるクライアントとの通信は、基本的に、
;;;   充分に高速である、という前提とする。
;;; -- クライアントが不正なリクエストを送ってきた場合に、簡単に
;;;    スタックしてしまうのを避ける為に、クライアントからリクエストを
;;;    受け取る際に、タイムアウトを設定する必要がある為。


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
   ;; 以下のようなリストを指定する。
   ;; '(unix "/tmp/hoge.sock" [:reuse-addr? flag])
   ;; '(inet port-number [:reuse-addr? flag])
   ;; '(tcp server-addr port-number [:reuse-addr? flag]) ; 独自拡張。
   (socket-spec
     :accessor socket-spec-of
     :init-keyword :socket-spec
     :init-value #f) ; 指定必須

   ;; 内部変数用スロット
   (socket-cs
     :accessor socket-cs-of
     :init-value #f)
   ))

(define-class <sexp-server> (<sexp-cs>)
  (
   (proc
     :accessor proc-of
     :init-keyword :proc
     :init-value #f) ; 指定必須
   ;; procは、引数として、S式objが一つ渡されるので、
   ;; 返り値として、以下のどちらかを返す事。
   ;; - (values S式obj bool)
   ;; -- 一番目の値がクライアントにレスポンスとして渡る。
   ;; -- 二番目の値が#tなら、接続は維持される。
   ;; -- 二番目の値が#fなら、接続は切断される。
   ;; - S式obj
   ;; -- (values S式obj #t)の省略形。
   (timeout
     :accessor timeout-of
     :init-keyword :timeout
     :init-value 2)

   ;; 内部変数用スロット
   (exit-flag ; 終了フラグ
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
                    ;; ToDo: with-error-handlerはread-from-stringにだけ
                    ;;       かかるようにすべきか？
                    (with-error-handler
                      (lambda (e)
                        ;; ToDo: ちゃんとエラー処理をする事
                        (report-error e)
                        #f)
                      (lambda ()
                        ;; ToDo: requestが<eof>でない事を確認する事
                        (let* ((l (read-line))
                               (request (read-from-string l))
                               )
                          ;; lが""の場合(空行が送られてきた)は
                          ;; procを実行せずに、こちらも空行を送り返す
                          ;; (ping代わりのdirty hack)
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
                                    ;; 終了処理を行う
                                    ;; ToDo: 他に必要な終了処理は無いか？
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
    ;; 終了処理をココに書く事
    (set! (socket-cs-of self) #f)
    ))


(define-method sexp-server-shutdown ((self <sexp-server>))
  ;; 実際のshutdownは、sexp-server-startの中で行われる。
  (set! (exit-flag-of self) #t))


;; ----


(define-method sexp-client-connect ((self <sexp-client>))
  ;; 既に開いている場合は、一旦閉じてから開き直す
  (when (socket-cs-of self)
    (sexp-client-close self))
  ;; ToDo: connect失敗エラー対策
  (let1 socket-client (make
                        <socket-client>
                        :socket-spec (socket-spec-of self))
    (socket-client-open socket-client)
    (set! (socket-cs-of self) socket-client)))


(define-method sexp-client-disconnect ((self <sexp-client>))
  ;; ToDo: disconnect失敗エラー対策
  (and-let* ((socket-client (socket-cs-of self)))
    (set! (socket-cs-of self) #f)
    (socket-client-close socket-client)
    ))


;; ToDo: エラー処理
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
                      ;; ToDo: あとでちゃんと作る事
                      (display "returned: " (current-error-port))
                      (write line (current-error-port))
                      (newline (current-error-port))
                      (report-error e)
                      (error e))
                    (lambda ()
                      (read-from-string line))))))))))
    (error "be connect, first")))


;; ToDo: 通信せずにソケットの状態を調べるだけにする？
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

