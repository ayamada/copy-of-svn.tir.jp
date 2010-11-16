;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; cgi用の認証システムを提供

;;; ToDo: aclのチェックをlazyに行えるように、procを指定するように変更する事。
;;;       (現在の仕様では巨大なaclを管理できない為)

;;; note: このモジュールは、dbmと同様の抽象モジュールである。

;;; note: tir02とは違い、typekey等の外部認証を使う場合でも、
;;;       毎回リダイレクトでtypekeyに飛ばすようにする。
;;;       (つまり、typekey等を使っていても、ログイン用urlは
;;;        常にCGIスクリプト自身のものになる。)
;;;       これによって、ログインurl生成

;;; WARN: cgi-mainは、with-auth-infoの内側で呼ぶ事。

;;; note: このモジュールが横取りするパラメータ類は、GET時のみとする。
;;;       POST時には反応しない事に注意。
;;;       また、「methodがPOSTでない」事を前提に、
;;;       POST時には動作しないコードが書かれている事にも注意する。

;;; note:
;;; tir02では、認証が常に必要という前提でログイン時のみ機能するように作ったが、
;;; 今回は、cgi-auth-infoパラメータを常に提供するモジュールとして作る。

;;; 使い方:
;;; - まず、適切にインスタンスを生成しておく。
;;; - with-auth-info内では、cgi-auth-infoパラメータから、認証情報を取り出せる。
;;; -- cgi-auth-infoが#fの時は、未認証状態。
;;; - 便利に使う為のユーティリティ関数を提供する。
;;; -- 常に認証が必要なページの為の、簡易認証画面関数
;;; -- ログイン/ログアウト用url/href/form生成関数

;;; 仕様:
;;; cgi-auth-infoは、以下の要素を持つkeywordsとする。
;;; - :unique-id (ユーザ固有のid)
;;;              (これは、固有のバイナリデータをdigest-hexifyした文字列とする)
;;; - :auth-type (認証機構の種別。違う認証機構を複数利用可能にした際に、
;;;               万が一、:unique-idが同一になってしまう可能性があるので)
;;; - その他のキーワード (その他の、各認証機構独自の値)
;;; 尚、cgi-auth-infoがlistではなく#fなら、未ログイン状態であるとする。

;;; 構造:
;;; - 認証情報は、基本的に、このモジュールの外に持つ事とする
;;;   (このモジュールのstorage-dirを使ってもいいが)
;;; - 認証の有無は、(session)cookieを使って行う。
;;;   (通常cookieも可能)
;;; - cookieでは、sha1割符文字列をやりとりする。
;;; - 上記の割符文字列には、cgi-auth-infoそのもの(keywords)が対応する。
;;; - cgi-auth-infoのkeywordsを、session-liteに保存する。


(define-module tir03.cgi.auth
  (use gauche.parameter)

  (use file.util)

  (use rfc.cookie)
  (use text.tree)
  (use text.html-lite)
  (use www.cgi)

  (use tir03.cgi)
  (use tir02.session.lite)
  ;(autoload tir02.session.lite <session-lite>)

  (export
    cgi-auth-dispatch-key ; CGIパラメータのキー文字列(uniqueかつ被らない事)
    cgi-auth-info ; parameterizeされた認証情報(keywords)

    <cgi-auth>

    with-auth-info
    ;; usage: (with-auth-info <cgi-auth> thunk)
    ;; cgi応答を横取りし、必要なら、認証を行う。

    must-required-auth-with

    ;; 以下の関数は、オプショナル引数として'login 'logout 'flipのどれかを取る
    ;; (デフォルトでは'flip)
    ;; CGIパラメータとしてflipを投げる事はしない(ダブルクリック時に問題発生)
    cgi-auth:make-url
    cgi-auth:make-html:a
    cgi-auth:make-html:form
    ;; ※これらは、methodではなく関数だという事に注意。

    ;; 以下は、抽象メソッド。子クラスが実装しなくてはならない。
    get-description-html ; 認証説明の書かれたhtml-treeを返す関数
    cgi-auth:dispatch-login ; 実装必須。
    cgi-auth:dispatch-logout ; 実装必須。
    cgi-auth:dispatch-logined ; 使わないなら、実装しなくてもよい
    cgi-auth:dispatch-logouted ; 使わないなら、実装しなくてもよい

    ;; 以下は、子クラスが利用する為の内部用メソッド。
    cgi-auth-info-save!
    cgi-auth-info-delete!
    ;; cgi-auth-infoを新たに設定し、そのsidを返す。
    ;; 主に、子クラスが認証結果を保存する為に使う。
    redirect/cookie ; cookieを発行しつつredirectする。cgi-mainの中では使えない
    ))
(select-module tir03.cgi.auth)


;;; --------


(define cgi-auth-dispatch-key (make-parameter "_auth"))
(define cgi-auth-info (make-parameter #f))


;;; --------


(define-class <cgi-auth> ()
  (
   ;; access control list
   ;; #tなら全許可、#fなら全却下、listなら以下の書式でaclとして機能する
   ;; '(("aaa" . #t) ; :unique-idが"aaa"のアカウントはログイン可
   ;;   ("bbb" . #t) ; :unique-idが"bbb"のアカウントはログイン可
   ;;   ("ccc" . #f) ; :unique-idが"ccc"のアカウントはログイン不可
   ;;   )
   ;; ToDo: もう少し汎用性のある書式にしたいが……。
   (acl
     :accessor acl-of
     :init-keyword :acl
     :init-value #t)
   ;; 上記のaclに含まれないアカウントのポリシー。
   (acl-fallback-policy
     :accessor acl-fallback-policy-of
     :init-keyword :acl-fallback-policy
     :init-value #f) ; #tか#fのみ。#tならログイン可。#fなら不可。
   ;; 上記のaclにひっかかったユーザに見せるhtml
   (acl-drop-html
     :accessor acl-drop-html-of
     :init-keyword :acl-drop-html
     :init-form (list
                  (cgi-auth:make-html:form :mode 'logout)
                  (html:hr)
                  (html:p
                    "このサービスはプライベートモードに設定されています。"
                    "指定されたユーザ以外は利用できません。"
                    )))
   ;; ログイン時のhtmlに使われる設定群
   (html-keywords
     :accessor html-keywords-of
     :init-keyword :html-keywords
     :init-value (list
                   ;; 大体、以下の値が必要。
                   ;:encoding (symbol->string (gauche-character-encoding))
                   ;:base-url #f
                   ;:css-url #f
                   ;:css-body #f
                   ;:js-url #f
                   ;:js-body #f
                   ;:js #f
                   :robots "NOINDEX,NOFOLLOW"
                   :title "認証して下さい"
                   ;:body-header #f
                   ;:body-footer #f
                   ))
   ;; ファイル類は、このディレクトリの中に生成する
   (storage-dir
     :accessor storage-dir-of
     :init-keyword :storage-dir
     :init-value #f)
   ;; フック(procを指定する)
   (login-hook
     :accessor login-hook-of
     :init-keyword :login-hook
     :init-value #f)
   (logout-hook
     :accessor logout-hook-of
     :init-keyword :logout-hook
     :init-value #f)
   ;; セッション関連
   ;; ToDo: もう少し上手くラッピングしたいが……。
   (session-maker
     :accessor session-maker-of
     :init-keyword :session-maker
     :init-form (lambda (self)
                  (make
                    <session-lite>
                    :dbm-path (string-append (storage-dir-of self) "/session")
                    )))

   ;; セッションクッキーについての情報
   (cookie-name
     :accessor cookie-name-of
     :init-keyword :cookie-name
     :init-value "tir03-auth")
   ;; keywordsの詳細は、rfc.cookieのconstruct-cookie-stringを参照
   (cookie-keywords
     :accessor cookie-keywords-of
     :init-keyword :cookie-keywords
     :init-value '(
                   :discard #t
                   :path "/"
                   ))

   ;; internal slot
   (session-manager
     :accessor session-manager-of
     :init-value #f)
   ))


(define-method initialize ((self <cgi-auth>) initargs)
  (next-method)
  ;; check :storage-dir
  (unless (storage-dir-of self)
    (errorf "~a must be need to :storage-dir" (class-name (class-of self))))
  ;; prepare :storage-dir
  (unless (file-exists? (storage-dir-of self))
    (make-directory* (storage-dir-of self)))
  ;; make session-manager
  (set!
    (session-manager-of self)
    ((session-maker-of self) self))
  )


;;; --------


(define-method cgi-auth-info-save! ((self <cgi-auth>) new-cgi-auth-info
                                                     . opt-sid)
  (let1 new-cgi-auth-info (cond
                            ((list? new-cgi-auth-info) new-cgi-auth-info)
                            ((is-a? new-cgi-auth-info <parameter>)
                             (new-cgi-auth-info))
                            (else
                              (error "invalid parameter" new-cgi-auth-info)))
    (let-optionals* opt-sid ((sid #f))
      (session-save! (session-manager-of self) sid new-cgi-auth-info))))


(define-method cgi-auth-info-delete! ((self <cgi-auth>) sid)
  (session-delete! (session-manager-of self) sid))


(define-method redirect/cookie ((self <cgi-auth>) sid . opt-url)
  (let-optionals* opt-url ((url (self-url/path-info)))
    (write-tree
      (cgi-header
        :pragma "no-cache"
        :cache-control "no-cache"
        :location url
        :cookies (construct-cookie-string
                   (list
                     (list*
                       (cookie-name-of self)
                       sid
                       (cookie-keywords-of self))))))))


(define (get-cgi-auth-info-from-cookie self)
  ;; cookieからcgi-auth-info(または#f)を取り出す。
  (and-let* ((sid (cgi-get-parameter
                    (cookie-name-of self)
                    (cgi-parse-parameters :query-string ""
                                          :merge-cookies #t))))
    (session-load (session-manager-of self) sid #f)))


(define (get-cgi-params)
  (and
    (equal? "GET" (cgi-get-metavariable "REQUEST_METHOD"))
    (and-let* ((query-string (cgi-get-metavariable "QUERY_STRING")))
      (cgi-parse-parameters :query-string query-string))))


(define (redirect-to-self)
  (write-tree
    (cgi-header
      :pragma "no-cache"
      :cache-control "no-cache"
      :location (self-url/path-info))))
(define-method cgi-auth:dispatch-login ((self <cgi-auth>) params)
  (errorf "internal error occured in ~s" (class-of self)))
(define-method cgi-auth:dispatch-logout ((self <cgi-auth>) params)
  (errorf "internal error occured in ~s" (class-of self)))
(define-method cgi-auth:dispatch-logined ((self <cgi-auth>) params)
  (redirect-to-self))
(define-method cgi-auth:dispatch-logouted ((self <cgi-auth>) params)
  (redirect-to-self))
(define *dispatch-table*
  (hash-table
    'equal?
    `("login" . ,cgi-auth:dispatch-login)
    `("logout" . ,cgi-auth:dispatch-logout)
    `("logined" . ,cgi-auth:dispatch-logined)
    `("logouted" . ,cgi-auth:dispatch-logouted)
    ))


(define-method with-auth-info ((self <cgi-auth>) thunk)
  (or
    (and-let* ((cgi-params (get-cgi-params))
               (dispatch-key (cgi-get-parameter
                               (cgi-auth-dispatch-key) cgi-params))
               (dispatch-proc (hash-table-get *dispatch-table*
                                              dispatch-key
                                              #f))
               )
      (dispatch-proc self cgi-params)
      #t)
    (parameterize ((cgi-auth-info (get-cgi-auth-info-from-cookie self)))
      (define (allowed)
        (thunk))
      (define (denied)
        (cgi-main
          (lambda (params)
            (apply
              cgi-tree-make
              :body (acl-drop-html-of self)
              (append
                (html-keywords-of self)
                (list
                  ;; fallback
                  :robots "NOINDEX,NOFOLLOW"
                  :title "認証"
                  ))))))
      (cond
        ((not (cgi-auth-info)) (allowed)) ; not logined (all ok)
        ((eq? #t (acl-of self)) (allowed)) ; all allowed
        ((not (acl-of self)) (denied)) ; all denied
        ((list? (acl-of self))
         (let1 unique-id (get-keyword :unique-id (cgi-auth-info))
           (if (assoc-ref
                 (acl-of self) unique-id (acl-fallback-policy-of self))
             (allowed)
             (denied))))
        (else
          (error "assertion"))))))


(define-method must-required-auth-with ((self <cgi-auth>) thunk . keywords)
  (if (cgi-auth-info)
    (thunk)
    (let-keywords* keywords ((html (list
                                     (cgi-auth:make-html:form)
                                     (html:hr)
                                     (get-description-html self)
                                     )))
      (cgi-main
        (lambda (params)
          (apply
            cgi-tree-make
            :body html
            (append
              (html-keywords-of self)
              (list
                ;; fallback
                :robots "NOINDEX,NOFOLLOW"
                :title "認証して下さい"
                ))))
        ;:on-error cgi-on-error/stack-trace
        ))))


(define-method get-description-html ((self <cgi-auth>))
  (errorf
    "internal error occured in ~s (not defined get-description-html)"
    (class-of self)))


(define-syntax choose-from-mode
  (syntax-rules ()
    ((_ mode logined-now logouted-now)
     (if (case mode
           ((flip) (cgi-auth-info))
           ((login) #f)
           ((logout) #t)
           (else
             (error ":mode must be 'flip or 'login or 'logout")))
       logouted-now
       logined-now))))


(define (cgi-auth:make-url . keywords)
  (let-keywords* keywords ((mode 'flip)) ; 'flip or 'login or 'logout
    (string-append
      (self-url/path-info)
      "?"
      (cgi-auth-dispatch-key) "=" (choose-from-mode mode
                                                    "login"
                                                    "logout"))))


(define (cgi-auth:make-html:a . keywords)
  (let-keywords* keywords ((mode 'flip) ; 'flip or 'login or 'logout
                           (label (choose-from-mode mode
                                    "ログイン"
                                    "ログアウト"))
                           (target #f)
                           )
    (html:a
      :href (cgi-auth:make-url :mode mode)
      :target target
      (html-escape-string label))))


(define (cgi-auth:make-html:form . keywords)
  (let-keywords* keywords ((mode 'flip)
                           (label (choose-from-mode mode
                                    "ログイン"
                                    "ログアウト"))
                           (target #f)
                           ;; ToDo: htmlを指定できるようにすべき？
                           )
    (make-form
      (self-url/path-info)
      `((,(cgi-auth-dispatch-key) ,(choose-from-mode mode
                                                     "login"
                                                     "logout")))
      (html:input :type "submit"
                  :value label)
      :method "get"
      :target target)))


;;; --------


(provide "tir03/cgi/auth")
