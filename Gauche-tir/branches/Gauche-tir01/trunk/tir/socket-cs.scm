#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; 協調スレッド型のclient/serverソケットライブラリ

;;; 使用上の注意:
;;; - ソケット通信の仕様上、あらゆるタイミングで通信が途切れる可能性がある。
;;;   具体的には、sigpipeが流れたり、readした結果が<eof>だったり、
;;;   write時にerror例外が発生する可能性がある。
;;;   thunkには、必ず、シグナル対策、error例外対策、
;;;   処理のトランザクションの安全を確保する事。

;;; 用語:
;;; socket-server: このモジュールによって実現される、tcp-server等の事。
;;; socket-client: このモジュールによって実現される、tcp-client等の事。
;;; server-socket: tcpのport等をlistenしているsocket。
;;; client-socket: 接続先につながっているsocket。
;;;                socket-serverもsocket-clientもコレを通して相手と通信する。

;;; ToDo: メモリリークしていないかどうかチェックする事。
;;; ToDo: 接続先から切断され、<eof>が読み出された時に自動closeする手段が無い？


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
   ;; 以下のようなリストを指定する。
   ;; '(unix "/tmp/hoge.sock" [:reuse-addr? flag])
   ;; '(inet port-number [:reuse-addr? flag])
   ;; '(tcp server-addr port-number [:reuse-addr? flag]) ; 独自拡張。
   (socket-spec
     :accessor socket-spec-of
     :init-keyword :socket-spec
     :init-value #f) ; 指定必須
   ))

(define-class <socket-server> (<socket-cs>)
  (
   (thunk ; 通信時に実行する/されるthunk。
     :accessor thunk-of
     :init-keyword :thunk
     :init-value #f) ; 指定必須
   ;; thunkは、bool値を返す事。
   ;; #tなら、コネクションは維持する。
   ;; #fなら、接続は切断する。

   ;; 内部変数用スロット
   (exit-flag ; socket-serverの終了フラグ
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
   ;; 内部変数用スロット
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
       ;; 既にsocketファイルが存在する場合、消さないとlistenできない
       (sys-unlink (cadr socket-spec))
       socket-spec)
      (else socket-spec))))


;;;;;;;;;

;; ToDo:
;; 各種の保護ハンドラ
;; fallback-thunkとして、client-socket切断thunkを受け取る
;; ToDo: シグナル保護
;;       どのシグナルを保護する？
;;       保護は単に無効化するだけで良いのか？
;;       シグナルが流れたらfallback-thunkを実行すべきなのか？
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


;;; ToDo: www.cgi風に、接続ip等の情報をparameterで提供すること。
;;;       - server-addr
;;;       - server-port
;;;       - client-addr
;;;       - client-port
;;;       unix-domain-socketの時は？？？
;; ToDo: 一定間隔で、通信の無いclient-socketの接続断を判定するようにする事
;; ToDo: シグナルからの保護
(define-method socket-server-start ((self <socket-server>))
  (let ((server-socket
          ;; ToDo: listenに失敗した時の処理を考える
          (apply-make-socket make-server-socket (socket-spec-of self)))
        (selector (make <selector>))
        )
    ;; server-socket処理ハンドラ
    (define (accept-handler sock flag)
      (let* ((client-socket (socket-accept server-socket))
             (output (socket-output-port client-socket :buffering :none))
             )
        (set!
          (client-sockets-of self)
          (cons client-socket (client-sockets-of self)))
        ;; client-socket処理ハンドラを登録
        (selector-add!
          selector
          (socket-input-port client-socket :buffering :none)
          (lambda (input flag)
            ;; ToDo: エラー例外捕捉の為に、↓をハンドラで囲む必要がある
            ;;       （例外捕捉後は必ずclient-socketをcloseしたい為）
            (unless (with-output-to-port
                      output
                      (lambda ()
                        (begin0
                          (with-input-from-port
                            input
                            (thunk-of self))
                          (flush))))
              ;; client-socketをshutdownしcloseする
              (socket-shutdown client-socket 2)
              (socket-close client-socket)
              ;; client-socket処理ハンドラを削除
              (selector-delete! selector input #f #f)
              ;; (client-sockets-of self)から除去する
              (set!
                (client-sockets-of self)
                (delete client-socket (client-sockets-of self)))
              #f))
          '(r))))

    ;; 内部スロットにserver-socketを保存
    (set! (server-socket-of self) server-socket)

    ;; server-socket処理ハンドラを登録
    (selector-add!
      selector
      (socket-fd server-socket)
      accept-handler
      '(r))

    ;; exit-flagが#tでなければ、繰り返す
    ;; 繰り返し処理する
    (let loop ()
      (selector-select selector)
      (unless (exit-flag-of self)
        (loop)))

    ;; shutdownしcloseする
    (socket-shutdown server-socket 2)
    (socket-close server-socket)
    (set! (server-socket-of self) #f)
    ))


(define-method socket-server-shutdown ((self <socket-server>))
  ;; thunkの中から呼ばれる（筈）。
  ;; thunkの中でserver-socketを閉じてしまうのはまずい気がするので、
  ;; thunkが終了してから閉じるようにする為に、ここではflag操作のみ行う。
  ;; 実際のshutdownは、socket-server-startの中で行われる。
  (set! (exit-flag-of self) #t))


;; ----


(define-method socket-client-open ((self <socket-client>))
  ;; 既に開いている場合は、一旦閉じてから開き直す
  (when (client-socket-of self)
    (socket-client-close self))
  (set!
    (client-socket-of self)
    ;; ToDo: listenに失敗した時の対処
    (apply-make-socket make-client-socket (socket-spec-of self))))


(define-method socket-client-close ((self <socket-client>))
  ;; ToDo: closeに失敗した時の対処
  ;;       (サーバから接続を切られた場合等に、socket-shutdownで
  ;;        エラー例外が発生する事がある)
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

