#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ToDo: スロットを見て、ログイン機能を全く使わないなら、
;;;       上部のログインリンクは出さないようにする



(define-module cgi-client
  (use gauche.parameter)
  (use srfi-2) ; and-let*
  (use rfc.uri)
  (use text.html-lite)
  (use text.tree)
  (use util.list)
  (use www.cgi)
  (use qdbm)

  (use tir.cgi)
  (use tir.cgi-framework-lite)
  (use tir.sexp-cs)
  (use tir.dbmwl)

  (export
    <cgi-client>
    cgi-client-main
    ))
(select-module cgi-client)


(define-class <cgi-client> ()
  (
   (title
     :accessor title-of
     :init-keyword :title
     :init-value "cgi client")
   (css-url
     :accessor css-url-of
     :init-keyword :css-url
     :init-value "http://css.tir.jp/tir.css")
   (body-class
     :accessor body-class-of
     :init-keyword :body-class
     :init-value "wireframe01")
   (mail-address
     :accessor mail-address-of
     :init-keyword :mail-address
     :init-value "nekoie-cgi-client@tir.jp")

   (socket-spec
     :accessor socket-spec-of
     :init-keyword :socket-spec
     ;:init-value '()
     :init-value '(tcp "127.0.0.1" 12345 :reuse-addr? #t)
     )
   (session-cookie-key
     :accessor session-cookie-key-of
     :init-keyword :session-cookie-key
     :init-value "gs-common")
   (loginid-cookie-key
     :accessor loginid-cookie-key-of
     :init-keyword :loginid-cookie-key
     :init-value "gs-loginid")
   (description-text
     :accessor description-text-of
     :init-keyword :description-text
     :init-value #f)
   (session-dbm-path
     :accessor session-dbm-path-of
     :init-keyword :session-dbm-path
     :init-value "./session-dbm")
   (account-dbm-path
     :accessor account-dbm-path-of
     :init-keyword :account-dbm-path
     :init-value "./account-dbm")

   ;; プライベートスロット
   (sexp-client
     :accessor sexp-client-of
     :init-value #f)
   (cgi-framework
     :accessor cgi-framework-of
     :init-value #f)
   (session-dbm
     :accessor session-dbm-of
     :init-value #f)
   (account-dbm
     :accessor account-dbm-of
     :init-value #f)
   ))

(define-method initialize ((self <cgi-client>) initargs)
  (next-method)
  (unless (session-dbm-path-of self)
    (error "this class must be need to :session-dbm-path"))
  (unless (account-dbm-path-of self)
    (error "this class must be need to :account-dbm-path"))
  ;; dbmを用意する
  (set!
    (session-dbm-of self)
    (make
      <dbmwl>
      ;;
      :dbm-path (session-dbm-path-of self)
      ))
  (set!
    (account-dbm-of self)
    (make
      <dbmwl>
      ;;
      :dbm-path (account-dbm-path-of self)
      ))
  )


;; ----


(define now-url self-url/path)

(define (self-url/param param-alist)
  (make-url-from-form-params (self-url) param-alist))

(define (now-url/param param-alist)
  (make-url-from-form-params (now-url) param-alist))


;; ----


(define (ss-ping self)
  #t)


(define (ss-connect self)
  (let1 sexp-client (make
                      <sexp-client>
                      :socket-spec (socket-spec-of self)
                      )
    (set! (sexp-client-of self) sexp-client)
    ;; ココで、何らかの通信テストを行う必要があるかも知れない
    #t))


(define (ss-version-check self)
  #t)


(define *default-description-text*
  (string-append
    "あとで、説明文を書きます"
    ""
    ""
    ""
    ""
    ))


(define (get-session-obj self)
  (if (not (session-dbm-path-of self))
    #f
    (begin
      ;; まず、cookieから割符文字列を取得する
      (and-let* ((cookie-str (cgi-get-parameter
                               (session-cookie-key-of self)
                               (cgi-parse-parameters
                                 :query-string ""
                                 :merge-cookies #t))))
        ;; まだ作成途中
        #f
        ))))

(define (make-page self body)
  (list
    (cgi-header)
    (html-tree-make
      :encoding "EUC-JP"
      :title (title-of self)
      :css-url (css-url-of self)
      :body-class (body-class-of self)
      :body (html:div
              body
              (html:hr)
              (html:address (hes (title-of self)))
              ))))

(define (html-href label param)
  (html:a
    :href (now-url/param param)
    :target "_self"
    (hes label)))


(define (response:main self p c session-obj)
  (define (link-login/out)
    (apply
      html-href
      (if session-obj
        (list "logout" '(("cc" "logout")))
        (list "login (このサイトの説明)" '(("cc" "login_form"))))))

  (make-page
    self
    (list
      (if (session-dbm-path-of self)
        (list
          (html:address (link-login/out))
          (html:hr))
        '())
      ;; ココに本体を書く
      (html:p (hes "まだ作成中です"))
      (if p
        (html:div
          (html:p
            (hes
              "現在、「"
              p
              "」にいます")))
        '())
      ;; まだ
      ;; - GET/POSTによるフォームパラメータとPATH_INFOの二つの情報を同時に指定
      ;; -- GETは、副作用の無い操作のみにする事(情報閲覧系など)
      ;; -- POSTは、副作用の有る操作のみにする事(実際の操作を行う行動系等)
      ;; --- 但し、副作用の有る操作でも、間に確認画面を挟める場合は、
      ;;     最後のsubmitのみPOST固定とし、それまではGET/POSTを柔軟に使い分ける
      ;; -- PATH_INFOでは、where(位置情報的なもの)を指定する
      ;; 
      ;; - PATH_INFOが無い場合は、セッションの有無で挙動を変更する
      ;; -- セッション無しの時は、タイトルを表示
      ;; -- セッション有りの時は、現在位置へとリダイレクト
      ;; --- または「○○は××に居ます」といったメッセージとhref
      ;; 概要:
      ;; - アクションがあるなら、アクションを実行
      ;; -- sexpで通信を行い、結果を受け取る
      ;; -- 
      ;; - 現在位置の情報をsexpで取得
      ;; -- 
      ;; - 現在の自分自身の情報をsexpで取得
      ;; -- 
      ;; - 得られた結果をレンダリングする
      ;; -- 適当に
      ;; その他:
      ;; - 非ログイン時は表示のみ可能（アクション不可）とするが、
      ;;   実装は後回しにする事
      ;; - シンボリックリンク的なものを用意する
      ;; -- /self/は、自分自身を差す。装備等はココで行う
      ;; --- /self/と直接差さずに、procみたいな中に入れる？
      ;; --- この辺はplan9を見て確認したいところ
      ;; - アクション実行後、毎回、自分自身のステータスも取得して、
      ;;   片隅に表示させるようにしたい

      ;; 以下は、本来なら、ログイン後にのみ表示する
      (html:form
        :action (now-url)
        :method "post"
        :target "_self"
        :style "text-align:center"
        "# "
        (html:input
          :type "hidden"
          :name "c"
          :value "console_cmd")
        (html:input
          :style "width:75%"
          :type "text"
          :name "console_cmd"
          :value "")
        )
      )))


(define (make-form url hidden-alist . htmls)
  (html:form
    :action url
    :method "post"
    :target "_self"
    (map
      (lambda (x)
        (html:input
          :type "hidden"
          :name (car x)
          :value (cdr x)))
      hidden-alist)
    htmls))


(define (response:login-form self params . errors)
  ;; 以下の三つを表示する
  ;; - ログインフォーム
  ;; - ログインエラーメッセージ(エラー時のみ)
  ;; - 説明文
  ;; - 登録フォームへのリンク
  ;; クッキーから最後のログインidを取得し、含める
  ;; 但し、ログインidは入力値があるなら、そちらを優先する事
  (define (html:error-message)
    (if (null? errors)
      '()
      (html:div
        :class "error"
        (html:ul
          (map
            (lambda (x)
              (html:li
                (hes x)))
            errors)))))
  (define (html:login-form)
    (make-form
      (now-url)
      '(("cc" . "login"))
      (hes "login id : ")
      (html:input
        :type "text"
        :name "login_id"
        :value "") ;; ToDo: あとで現在の値を反映する事
      (html:br)
      (hes "login password : ")
      (html:input
        :type "password"
        :name "password"
        :value "")
      (html:br)
      (html:input
        :type "submit"
        :value "ログイン")
      ))
  (define (html:description)
    ;; ToDo: 簡単な、テキスト→html変換関数を用意し、それに掛ける事
    (let1 description (or
                        (description-text-of self)
                        *default-description-text*)
      (html:pre
        (hes description))))
  (define (html:link-regist)
    (html:ul
      (html:li
        (html-href "新規登録" '(("cc" "register"))))))

  (make-page
    self
    (list
      (html:error-message)
      (html:login-form)
      (html:hr)
      (html:description)
      (html:hr)
      (html:link-regist)
      )))

(define (response:login self params)
  ;; パラメータを検証し、正しければ以下の挙動を行う
  ;; - ログイン状態とする
  ;; - セッションクッキーとログインid記憶用クッキーの二種を発行
  ;; - response:loginedへとリダイレクト
  ;; 正しくなければ、「どのように問題があるのか」のメッセージと共に、
  ;; response:login-formに差し戻す
  (let* (
         (login_id (cgi-get-parameter "login_id" params))
         (password (cgi-get-parameter "password" params))
         ;;;;
         )
    ;; ログイン処理を行う
    ;;;; まだ
    ;; (認証に成功したなら)
    ;; セッションクッキーを返しつつ、ログイン完了画面へとリダイレクト
    ;;;; 専用モジュールを作るようにする事
    (cgi-header
      :location (now-url/param '(("cc" "logined"))))))

(define (response:logined self params)
  ;; まだ作ってない
  (make-page
    self
    (list
      (html:p (hes "まだ作成中です"))
      (html:ul
        (html:li
          (html:a
            :href (now-url)
            (hes "戻る")))
        ))))

(define (response:logout self params)
  ;; まだ作ってない
  (make-page
    self
    (html:p (hes "まだ作成中です"))))

(define (response:logouted self params)
  ;; まだ作ってない
  (make-page
    self
    (list
      (html:p (hes "まだ作成中です"))
      (html:ul
        (html:li
          (html:a
            :href (now-url)
            (hes "戻る")))
        ))))

(define (response:register self params)
  ;; まだ作ってない
  (make-page
    self
    (list
      (html:p (hes "今は、使われていないlogin idとパスワードを入力する事で"
                   "登録可能です"))
      (html:ul
        (html:li
          (html:a
            :href (now-url/param '(("cc" "login_form")))
            (hes "戻る")))
        ))))


(define (ss-main self params)
  (let (
        (p (cgi-get-metavariable "PATH_INFO")) ; 表示ターゲット指定子
        (c (cgi-get-parameter "c" params)) ; 本体の指示用
        (cc (cgi-get-parameter "cc" params)) ; ログイン/ログアウト用
        (session-obj (get-session-obj self))
        )
    (define (go)
      (response:main self p c session-obj))

    ;; ToDo: あとでtable化する
    ;; ToDo: table化したものを、更に別関数化し、他でも使えるようにする
    (if (not cc)
      (go)
      (if session-obj
        (cond
          ((string=? "logined" cc) (response:logined self params))
          ((string=? "logout" cc) (response:logout self params))
          (else (go)))
        (cond
          ((string=? "login_form" cc) (response:login-form self params))
          ((string=? "login" cc) (response:login self params))
          ((string=? "logouted" cc) (response:logouted self params))
          ((string=? "register" cc) (response:register self params))
          (else (go)))))))


(define-method cgi-client-main ((self <cgi-client>))
  (let1 cgi-framework (cgi-framework-of self)
    (if (not cgi-framework)
      (begin
        (set!
          (cgi-framework-of self)
          (make
            <cgi-framework-lite>
            :title (title-of self)
            :css-url (css-url-of self)
            :body-class (body-class-of self)
            :mail-address (mail-address-of self)
            :initialize-thunk (lambda ()
                                (if (sexp-client-of self)
                                  (or
                                    (ss-ping self)
                                    (error "connection disconnected"))
                                  (and
                                    (or
                                      (ss-connect self)
                                      (error "cannot connect to server"))
                                    (or
                                      (ss-version-check self)
                                      (error "mismatch protocol version"))
                                    )))
            :main-proc (lambda (params)
                         (ss-main self params))
            ))
        (cgi-client-main self))
      (cgi-framework-lite-main cgi-framework))))


(provide "cgi-client")


