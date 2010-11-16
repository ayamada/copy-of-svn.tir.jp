;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme sw=2 ts=2 et:
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
   ;; 最初から分かっている値
   (first-transaction :init-keyword :first-transaction
                      :init-value #t)
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

   ;; 結果情報
   (response-code :init-value #f) ; 200, 302, 500 等の数値。
   (internal-description :init-value #f) ; 備考欄。nphやエラー理由等を記述
   (connection-close :init-value #f) ; 今後レスポンスを返してはいけない

   ;; クライアントからのHTTPリクエストの情報
   (request-line :init-value #f) ; "GET / HTTP/1.0" 等
   (request-method :init-value #f)
   (request-uri :init-value #f)
   (request-protocol :init-value #f)
   (parsed-uri-list :init-value #f) ; uri-parseの返り値のリスト
   (http-0.9-flag :init-value #f) ; HTTP/0.9なら真
   (request-header :init-value #f) ; request-lineに続くrfc822形式のヘッダ
   (content-type :init-value #f)
   (content-length :init-value #f)
   (request-body :init-value #f) ; この段階で一旦全部読み取っておく

   ;; cgiスクリプト実行時に必要になる情報
   (dispatched-server-name :init-value #f) ; HTTP_HOST等からvhost名を生成
   (dispatched-script-name :init-value #f) ; dispatch後のスクリプトのpath
   (dispatched-path-info :init-value #f) ; dispatch後のdecode前のpath-info
   (dispatched-cgi-thunk :init-value #f) ; 実行すべきcgiスクリプト本体
   (nph :init-value #f) ; cgiスクリプトがnph動作を行うかどうか
   (use-persistent-connection :init-value #f) ; 永続的接続を行うかどうか
   (auth-type :init-value #f) ; 認証手法（Basic, Digest）
   (auth-name :init-value #f) ; 認証の名前
   (auth-hash :init-value #f) ; 認証に使うhash-table
   (remote-user :init-value #f) ; 認証時のユーザ名
   (plain-cgi-metavariables :init-value #f) ; 生成されたメタ変数
   (merged-cgi-metavariables :init-value #f) ; 元の(cgi-metavariables)とマージ

   ;; cgiスクリプトからのレスポンスの情報
   (cgi-response :init-value #f) ; cgiが出力した内容そのもの(string)
   (cgi-error-instance :init-value #f) ; 捕捉したエラー例外オブジェクト
   (cgi-response-header :init-value #f) ; cgiの返すrfc822形式のヘッダ
   (cgi-response-body :init-value #f) ; 上のヘッダを取った残り

   ;; クライアントに返すHTTPレスポンスの情報
   (response-status-line :init-value #f) ; "HTTP/1.1 200 OK" 等
   (response-header :init-value #f) ; status-lineに続くrfc822形式のヘッダ
   (response-body :init-value #f) ; 上のヘッダを取った残り
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

