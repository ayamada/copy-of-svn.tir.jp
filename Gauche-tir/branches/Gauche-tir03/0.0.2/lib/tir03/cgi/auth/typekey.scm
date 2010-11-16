;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(define-module tir03.cgi.auth.typekey
  (extend tir03.cgi.auth)
  (use gauche.charconv)
  (use gauche.parameter)

  (use srfi-13)

  (use file.util)

  (use text.html-lite)
  (use text.tree)
  (use www.cgi)
  (use util.digest)

  (use tir03.cgi)

  (use typekey)

  (export
    <cgi-auth-typekey>
    get-description-html
    cgi-auth:dispatch-login
    cgi-auth:dispatch-logout
    cgi-auth:dispatch-logined
    cgi-auth:dispatch-logouted
    ))
(select-module tir03.cgi.auth.typekey)


;;; --------


(define-class <cgi-auth-typekey> (<cgi-auth>)
  (
   ;; typekey設定
   (typekey-token
     :accessor typekey-token-of
     :init-keyword :typekey-token
     :init-value #f)

   ;; internal slot
   (typekey-manager
     :accessor typekey-manager-of
     :init-value #f)
   ))


(define-method initialize ((self <cgi-auth-typekey>) initargs)
  (next-method)
  ;; check :typekey-token
  (unless (typekey-token-of self)
    (errorf "~a must be need to :typekey-token" (class-name (class-of self))))
  ;; make typekey-manager
  (set!
    (typekey-manager-of self)
    (make
      <typekey>
      :token (typekey-token-of self)
      :key-cache-path (string-append (storage-dir-of self) "/regkeys.txt")
      ))
  )


;;; --------


(define-method get-description-html ((self <cgi-auth-typekey>))
  (list
    (html:h1 "TypeKey認証")
    (html:p
      "このサービスを利用するには、TypeKey認証が必要です。"
      (html:br)
      "ログインボタンを押し、TypeKey認証を行って下さい。"
      (html:br)
      "(ログイン状態は、ブラウザを閉じると解除されます)"
      )
    (html:p
      "TypeKeyについては、下記のサイトを確認して下さい。"
      (html:br)
      "また、TypeKeyアカウントの取得や設定変更等も、"
      "下記サイトから行えます。")
    (html:ul
      (html:li
        (html:a
          :href "http://www.sixapart.jp/typekey/"
          :target "_blank"
          "http://www.sixapart.jp/typekey/")))
    (html:hr)
    (html:p
      "※ログインには、"
      "cookieが有効になっている必要があります。"
      (html:br)
      "ログインボタンを押してもログインできない時は、"
      "ブラウザのcookie設定を見直して下さい。"
      (html:br)
      "また、一定期間、アクセスが無かった場合も、自動的に"
      "ログアウトされますので、再ログインして下さい。"
      )))


;;; --------


(define (redirect url)
  (write-tree
    (cgi-header
      :pragma "no-cache"
      :cache-control "no-cache"
      :location url)))

(define-method cgi-auth:dispatch-login ((self <cgi-auth-typekey>) params)
  ;; typekeyへとリダイレクトを行う。
  (redirect
    (get-login-url
      (typekey-manager-of self)
      (string-append
        (self-url/path-info)
        "?"
        (cgi-auth-dispatch-key) "=" "logined"
        ))))

(define-method cgi-auth:dispatch-logined ((self <cgi-auth-typekey>) params)
  ;; typekeyパラメータのverifyを行い、sessionに保存し、
  ;; cookieを発行しつつredirectする。
  (define (get-auth-info)
    (define (reader key)
      (cgi-get-parameter key params))
    (define (utf-8->native str)
      (with-error-handler
        (lambda (e) #f)
        (lambda ()
          (ces-convert str "utf-8"))))
    (and-let* (
               (email (reader "email"))
               (name (reader "name"))
               (nick (reader "nick"))
               (ts (reader "ts"))
               (sig (reader "sig"))
               )
      (and
        (verify (typekey-manager-of self) email name nick ts sig)
        (list
          :unique-id (string-append "typekey:" (digest-hexify name))
          :auth-type 'typekey
          :email (utf-8->native email)
          :name (utf-8->native name)
          :nick (utf-8->native nick)
          :ts (x->number ts)
          :sig (string-map
                 (lambda (c)
                   (if (eqv? c #\space)
                     #\+
                     c))
                 sig)))))
  (let1 typekey-auth-info (get-auth-info)
    (if (not typekey-auth-info)
      (cgi-auth:dispatch-login self params) ; パラメータ異常。再度認証させる
      (let1 sid (cgi-auth-info-save! self typekey-auth-info)
        ;;;; ToDo: 古いsidを保持しているなら、先に消去を行うべき？
        (redirect/cookie self sid)))))

(define-method cgi-auth:dispatch-logout ((self <cgi-auth-typekey>) params)
  ;; typekeyへとリダイレクトを行う。
  (redirect
    (get-logout-url
      (typekey-manager-of self)
      (string-append
        (self-url/path-info)
        "?"
        (cgi-auth-dispatch-key) "=" "logouted"
        ))))

(define-method cgi-auth:dispatch-logouted ((self <cgi-auth-typekey>) params)
  ;; typekeyのログアウト処理が終わったものとして、
  ;; sessionを削除し、削除cookieを発行しつつredirectする。
  ;;;; ToDo: あとで、cookieがsidを持っているなら、sessionから削除するように直す
  ;;;;       (おそらく、セキュリティ上、重要？)
  ;(cgi-auth-info-delete! self sid)
  (redirect/cookie self "logouted"))


;;; --------


(provide "tir03/cgi/auth/typekey")

