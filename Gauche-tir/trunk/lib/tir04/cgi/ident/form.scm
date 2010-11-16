;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; htmlのformでログインし、dbmにアカウント情報を保持する、
;;; 簡易登録/認証を行うクラス

;;; TODO: あとでemail等、その他の情報を同時に登録/保持できるように
;;;       拡張する事を考慮して作る事

;;; TODO: パスワード変更機能、アカウント削除機能が必要。
;;;       尚、アカウント削除機能は、単に削除するのではなく、
;;;       削除フラグを立てるようにして、
;;;       今後、同じidのアカウントが作られないようにする必要がある。

;;; TODO: ログイン後、クッキーでしばらく値を憶えさせるラッパーを別に作る
;;; TODO: ログイン画面にJavaScriptを追加

;;; 概要:
;;; このモジュールは、formによるユーザ識別を行う為のクラスを提供する。
;;; ユーザ識別の為の情報は指定されたdbmファイルに記録される。
;;; このクラスは、ユーザの識別機能のみを提供する為、別途、
;;; セッションクラスを用意する必要がある(tir04.cgi.session)。

#|
(define *cgi-ident*
  (make <cgi-ident-form>
        :internal-key-prefix "cgi-ident"
        :error-html-proc ...
        :error-html-keywords '(...)
        :dbm-type <qdbm>
        :dbm-path "/path/to/dbm"
        :rw-mode :write
        :html-keywords '(...)
        ))

(cgi-main
  (lambda (orig-params)
    (with-cgi-ident
      *cgi-ident*
      orig-params ; この値によって、with-cgi-identが処理を横取りするかが決まる
      (lambda (true-params ident-info)
        ;; true-paramsには、:internal-key-prefixで始まるkeyが除去された
        ;; paramsが入る。
        ;; 認証が行われ、それが成功した場合のみ、ident-infoに値が入る。
        ;; ident-infoが#f以外だった場合、ユーザがログインボタンを押し、
        ;; with-cgi-identが横取りした処理内で認証処理が行われ、正しく認証された
        ;; という事なので、ident-infoがある場合はparams等のチェックよりも先に
        ;; ログイン後の処理へとディスパッチしなくてはならない。
        ;; ident-infoは以下のようなlistとなる。
        ;; '(
        ;;   :ident-type 'form ; 固定
        ;;   :ident-path "/path/to/dbm" ; :dbm-pathの値が入る
        ;;   :ident-id "..." ; 登録されたログインID(不完全文字列の可能性有り)
        ;;   :uid "..." ; "form:/path/to/dbm:ログインID"をsha1 digestしてhexifyした文字列
        ;;   :x-ident-info '() ; 今のところ未実装
        ;;   )
        ;; 認証ページへのurlを生成。
        (make-ident-url *cgi-ident* callback-params)
        ;; 認証ボタン生成
        (make-ident-form *cgi-ident* callback-params html-tree . keywords)
        ... ; 様々な処理を行う
        ))))
|#


(define-module tir04.cgi.ident.form
  (use dbm)
  (use text.tree)
  (use text.html-lite)
  (use www.cgi)
  (use tir04.cgi.util)
  (extend tir04.cgi.ident)
  (use rfc.sha1)
  (use util.digest)
  (use tir04.dbm.util)

  (export
    <cgi-ident-form>
    with-cgi-ident
    make-ident-url
    make-ident-form
    ))
(select-module tir04.cgi.ident.form)



(define-class <cgi-ident-form> (<cgi-ident>)
  (
   (dbm-type
     :accessor dbm-type-of
     :init-keyword :dbm-type
     :init-value #f)
   (dbm-path
     :accessor dbm-path-of
     :init-keyword :dbm-path
     :init-value #f)
   (rw-mode ; :read or :write
     :accessor rw-mode-of
     :init-keyword :rw-mode
     :init-value :write)
   (html-keywords
     :accessor html-keywords-of
     :init-keyword :html-keywords
     :init-value '())
   ))


(define-method initialize ((self <cgi-ident-form>) initargs)
  (next-method)
  (unless (dbm-type-of self)
    (error ":dbm-type must be required"))
  (unless (dbm-path-of self)
    (error ":dbm-path must be required"))
  )

(define-method get-html-keywords ((self <cgi-ident-form>)
                                  keyword . opt-fallback)
  (apply get-keyword keyword (html-keywords-of self) opt-fallback))

;; <cgi-ident>の子クラスが実装すべきmethods
(define-method dispatch-or-ident-info ((self <cgi-ident-form>) dispatch-val
                                                               cgi-params
                                                               true-params
                                                               ident-info-cont)
  ;; dispatch-valによってディスパッチを行う
  ;; 以下のような遷移になると思われる。
  ;; - "login"なら、ログインフォームと登録フォームへのリンクを出す
  ;; - "login-submit"なら、ログインパラメータを検証し、問題なければ
  ;;   dispatch-val無しurlへとリダイレクト戻し
  ;;   (マルチログイン対応の為に、戻りurlはカスタマイズ可能にする事)
  ;; - "register"なら、登録フォームを出す(:rw-modeが:write時のみ)
  ;; - "register-confirm"なら、登録フォームの確認画面を出す
  ;; - "register-submit"なら、登録パラメータを検証し、"registered"へ
  ;; - "registered"なら、登録完了メッセージとログインフォームへのリンクを出す
  ;; -- 直接ログイン状態にする事も考えたが、
  ;;    確認の意味も含めて、ログインフォームに戻す事にした
  ;; - それ以外なら、"login"と同じ扱いとする
  (let1 proc (cond
               ((equal? "login-submit" dispatch-val)
                form:login-submit)
               ((equal? "register" dispatch-val)
                form:register)
               ((equal? "register-confirm" dispatch-val)
                form:register-confirm)
               ((equal? "register-submit" dispatch-val)
                form:register-submit)
               ((equal? "registered" dispatch-val)
                form:registered)
               (else ; or "login"
                 form:login))
    (proc self cgi-params true-params ident-info-cont)))

(define (make-page self form-flag . body)
  ;; TODO: カスタマイズ可能にする事
  (cgi-tree-make
    :http-header '(:pragma "no-cache")
    :encoding (x->string (cgi-output-character-encoding))
    :css-url (get-html-keywords self :css-url #f)
    :robots "NOINDEX,NOFOLLOW"
    :js-body "function f(){document.f.l.focus();}"
    :title (get-html-keywords self :title #f)
    :body-header #f
    :body-footer (list
                   (html:hr)
                   (html:address
                     (html:a
                       :name "bottom"
                       (get-html-keywords self :title #f))))
    :body-attr (if form-flag
                 '(:onload "f()")
                 #f)
    :body body))

(define (make-error-page self . body)
  (make-page
    self
    #f
    (html:h1 "error")
    body))

(define (form:login self cgi-params true-params ident-info-cont)
  ;; TODO: カスタマイズ可能にする事
  ;; ログインフォームを出す
  ;; また、(rw-mode-of self)が:writeなら、登録リンクも出す
  (make-page
    self
    #t
    (make-form
      (self-path/path-info)
      `((,(dispatch-key-of self) "login-submit")
        )
      (list
        (html:div
          (text->inline-html
            (tree->string
              (get-keyword :login-message (html-keywords-of self) '()))))
        (html:div
          (html:tt
            "login-id: "
            (html:input
              :style "ime-mode:disabled"
              :type "text"
              :name "login-id"
              :id "l"
              :value "")))
        (html:div
          (html:tt
            "password: "
            (html:input
              :type "password"
              :name "password"
              :value "")))
        (html:div
          (html:tt
            (html:input
              :type "submit"
              :value "login")))
        (if (not (eq? (rw-mode-of self) :write))
          '()
          (list
            (html:hr)
            (html:p
              (html:a
                :target "_self"
                :href (append-params-to-url
                        (self-url/path-info)
                        `((,(dispatch-key-of self) "register")))
                "新規登録")))))
      :name "f")))

(define (form:login-submit self cgi-params true-params ident-info-cont)
  (let ((login-id (cgi-get-parameter "login-id" cgi-params :default ""))
        (password (cgi-get-parameter "password" cgi-params :default ""))
        )
    (guard (e (else
                ;; TODO: あとで、その他のエラーにも対応できるようにする事
                (make-error-page
                  self
                  (html:div
                    "ログインidかパスワードが間違っています"))))
      (let1 login-info (get-login-info self login-id) ; keywords or #f
        (unless login-info
          (error "login-info not found"))
        (let1 password-digest (password->digest password)
          (if (not
                (string=?
                  (get-keyword :password-digest login-info "")
                  password-digest))
            (error "password not matched")
            (ident-info-cont
              `(
                :ident-type 'form
                :ident-path (dbm-path-of self)
                :ident-id login-id
                :uid ,(digest-hexify
                        (sha1-digest-string
                          (string-join
                            (list
                              "form"
                              (dbm-path-of self)
                              login-id)
                            ":")))
                :x-ident-info '()))))))))

(define (form:register self cgi-params true-params ident-info-cont)
  (if (not (eq? (rw-mode-of self) :write))
    (make-error-page
      self
      (html:div "現在、登録は中止しています"))
    (make-page
      self
      #f
      (make-form
        (self-path/path-info)
        `((,(dispatch-key-of self) "register-confirm")
          )
        (list
          (html:div
            (het
              "login-idとpasswordを決めてください。"
              "(あとで詳しい説明を書く予定)"
              ))
          (html:div
            (html:div
              (html:tt
                "login-id: "
                (html:input
                  :style "ime-mode:disabled"
                  :type "text"
                  :name "login-id"
                  :value "")))
            (html:div
              (html:tt
                "password: "
                (html:input
                  :type "password"
                  :name "password"
                  :value "")))
            (html:div
              (html:tt
                "password: "
                (html:input
                  :type "password"
                  :name "password"
                  :value "")
                "(確認の為、同じ内容を二回入力してください)"))
            (html:div
              (html:tt
                (html:input
                  :type "submit"
                  :value "登録")))))))))
(define (form:register-confirm self cgi-params true-params ident-info-cont)
  ;; TODO: あとで実装する事にする
  (form:register-submit self cgi-params true-params ident-info-cont))
(define (form:register-submit self cgi-params true-params ident-info-cont)
  (define (error-html . msg)
    (make-error-page self
                     (html:div (apply het msg))))

  (let ((login-id (cgi-get-parameter "login-id" cgi-params))
        (passwords (cgi-get-parameter "password" cgi-params :list #t))
        )
    ;; パラメータチェック
    (cond
      ((or
         (not login-id)
         (zero? (string-length login-id)))
       (error-html "login-idを指定してください"))
      ((not (#/^\w+$/ login-id)) ; TODO: あとでカスタマイズ可能に
       (error-html
         "login-idには半角アルファベットと数字とアンダーバーしか使えません"))
      ((< 16 (string-size login-id)) ; TODO: あとでカスタマイズ可能に
       (error-html "login-idが長過ぎます"))
      ((not (= 2 (length passwords)))
       (error-html "パラメータに異常があります"))
      ((not (string=? (car passwords) (cadr passwords)))
       (error-html "二つのパスワードが一致しません"))
      ((zero? (string-length (car passwords)))
       (error-html "パスワードを指定してください"))
      (else
        (let1 password-digest (password->digest (car passwords))
          (guard (e (else
                      ;(report-error e)
                      (error-html
                        "このlogin-idは既に、他の方によって登録されています。"
                        "別のlogin-idを指定してください")))
            (set-login-info!
              self
              login-id
              `(
                :password-digest ,password-digest
                ;; TODO: フォームの他の値も記録したりできるようにする事
                ;;       (しかし、何らかのインターフェースを考える必要有り)
                ))
            (location
              (append-params-to-url
                (self-url/path-info)
                `((,(dispatch-key-of self) "registered"))))))))))
(define (form:registered self cgi-params true-params ident-info-cont)
  (make-page
    self
    #f
    (html:p
      "登録しました。")
    (html:ul
      (html:li
        (html:a
          :href (append-params-to-url
                  (self-url/path-info)
                  `((,(dispatch-key-of self) "login")))
          "login")))))

(define (get-login-info self login-id)
  ;; keywordsを返す
  ;; データが無い場合は#fを返す
  (guard (e (else #f))
    (with-dbm-open
      (dbm-type-of self)
      :path (dbm-path-of self)
      :rw-mode :read
      :key-convert #f
      :value-convert #t
      (lambda (dbm)
        (dbm-get dbm login-id)))))

(define (set-login-info! self login-id login-info)
  ;; 既に登録されていた場合はエラー例外を投げる事
  (with-dbm-open
    (dbm-type-of self)
    :path (dbm-path-of self)
    :rw-mode (rw-mode-of self)
    :key-convert #f
    :value-convert #t
    (lambda (dbm)
      (if (dbm-exists? dbm login-id)
        (error "login-id already exists")
        (dbm-put! dbm login-id login-info)))))


(define (password->digest password)
  (digest-hexify
    (sha1-digest-string password)))




(define-method get-remove-params-key-list ((self <cgi-ident-form>))
  '("login-id" "password"))

(define-method with-cgi-ident ((self <cgi-ident-form>) cgi-params proc)
  (next-method))

(define-method make-ident-url ((self <cgi-ident-form>) misc-params)
  (next-method))

(define-method make-ident-form ((self <cgi-ident-form>)
                               misc-params
                               html-tree . keywords)
  (next-method))


(provide "tir04/cgi/ident/form")
