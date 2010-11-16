;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(define-module tcpcgi.state
  (use gauche.interactive)
  (export
    <tcpcgi.state>
    copy-all-slot!
    clone
    dump-string
    dump-to-error-port
    ))
(select-module tcpcgi.state)


(define-class <tcpcgi.state> ()
  (
   ;; 最初にセットされる値
   (counter     :init-keyword :counter
                :init-value 0) ; persistent-connectionの何回目の処理か
   (server-addr :init-keyword :server-addr
                :init-value #f)
   (server-port :init-keyword :server-port
                :init-value #f)
   (server-name :init-keyword :server-name
                :init-value #f)
   (remote-addr :init-keyword :remote-addr
                :init-value #f)
   (remote-port :init-keyword :remote-port
                :init-value #f)
   (remote-host :init-keyword :remote-host
                :init-value #f)
   (https       :init-keyword :https
                :init-value #f)

   ;; 結果情報（ロギング及び動作制御用）
   (response-code :init-value #f) ; 200, 302, 500 等の数値。
   (internal-description :init-value #f) ; 備考欄。nphやエラー理由等を記述
   (error-instance :init-value #f) ; 捕捉したエラー例外インスタンス
   (connection-close :init-value #f) ; 次に永続的接続を行わない/行えない

   ;; クライアントからのHTTPリクエストの情報
   (request-keywords :init-value #f) ; see tcpcgi.request

   ;; ディスパッチ結果情報
   (dispatched-server-name :init-value #f)
   (dispatched-plain-path-info :init-value #f)
   (dispatched-script-name :init-value #f)
   (dispatched-executee :init-value #f)

   ;; cgiメタ変数情報
   (cgi-metavariables :init-value #f) ; cgi-thunk実行時等にparameterizeする

   ;; executee補足情報
   (lazy-execute :init-value #f) ; 遅延実行/nph動作を行うかどうか

   ;; cgiスクリプトからのレスポンスの情報
   ;; execute後、cgi/1.1-response または、
   ;; (cons response-code response-keywords) が返る。
   (cgi/1.1-response :init-value #f) ; cgiが出力したCGI/1.1形式の内容の文字列
   (response-keywords :init-value #f) ; cgiが出力したresponse-keywords

   ;; レスポンスのconnectionヘッダ
   (response-connection :init-value #f) ; #f or "close" or "Keep-Alive"

   ;; cgi/1.1-responseを解析した結果
   (cgi-response-header :init-value #f) ; rfc822形式のヘッダ部分
   (cgi-response-body :init-value #f) ; ヘッダを取った残りのbody部分
   (http/1.1-status-line :init-value #f) ; HTTP/1.1 status-line
   (http/1.1-header :init-value #f) ; HTTP/1.1 response-header(alist)
   (http/1.1-body-port :init-value #f) ; HTTP/1.1 response-body(input-port)
   ))


(define-method copy-all-slot! ((src <tcpcgi.state>) (dst <tcpcgi.state>))
  (for-each
    (lambda (slot-name)
      (set!
        (ref dst slot-name)
        (ref src slot-name)))
    (map slot-definition-name (class-slots <tcpcgi.state>))))


(define-method clone ((self <tcpcgi.state>))
  (let1 new (make <tcpcgi.state>)
    (copy-all-slot! self new)
    new))


(define-method dump-string ((self <tcpcgi.state>))
  (with-output-to-string
    (lambda ()
      (describe self))))

(define-method dump-to-error-port ((self <tcpcgi.state>))
  (with-output-to-port
    (current-error-port)
    (lambda ()
      (describe self))))


(provide "tcpcgi/state")

