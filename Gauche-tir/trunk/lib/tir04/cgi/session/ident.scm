;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; 認証情報をセッションに保存するクラス
;;; 単なる、認証クラスとsessionクラスを上手く呼び出す為のラッパー。

;;; ToDo: タイムアウト時の対応をどうする？
;;;       「タイムアウトしました」ぐらいは出したいが、現状では
;;;       何も言わずにident-infoが#fになる為、判定が困難。
;;;       あとで考える。今のところは対応無しで作る。

#|
(define *csa*
  (make <cgi-session-ident>
        :dbm-type <fsdbm>
        :dbm-path "/path/to/dbm-file"
        :expire-second (* 1 24 60 60)
        :cookie-name "cgi-session"
        :cookie-keywords '(:discard #t :path "/") ; ここでexpiresは設定しない事
        :dispatch-keyname "cgi-session-ident" ; :cgi-identのinternal-key-prefixとは別枠である事に注意
        :cgi-ident (make <cgi-ident-hatena>
                        :internal-key-prefix "cgi-ident"
                        :error-html-proc ...
                        :error-html-keywords '(...)
                        :api-key "..."
                        :sec-key "..."
                        )
        ...))

(cgi-main
  (lambda (params)
    (with-cgi-session-ident
      *csa*
      params
      (lambda (fixed-params ident-info session-parameter)
        ;; fixed-paramsには、dispatch-keynameが除去されたparamsが入る。
        ;; cgi.ident.*とは違い、ログイン後は常にident-infoが与えられる。
        ;; 但し、タイムアウトしたら#fになる。
        ;; ident-infoが与えられている場合は、(session-parameter obj)で、
        ;; read/write invariance性を持つ値を記憶する事が出来る。
        ;; (session-parameter)で、その値を参照できる。
        ;; 初期値は#f。
        ;; この値はセッションが切れるまでの間のみ有効。
        ;;
        ;; ログイン/ログアウトurlを生成する。
        ;; typeには、'login 'logoutのいずれかを設定する。
        ;; ログイン/ログアウト後、現在のpath付きurlに、
        ;; callback-paramsで指定したパラメータ付きで戻ってくる。
        (make-cgi-session-ident-url *csa* callback-params type)
        ;; ログイン/ログアウトformを生成する。
        (make-cgi-session-ident-form *csa*
                                    callback-params type html-tree . keywords)
        ;; ログアウトurlやログアウトボタンが押されると、セッションが破棄され、
        ;; ident-infoと(session-parameter)は#fを返すようになる。
        ;; (クッキー自体はそのまま放置される。)
        ... ; 様々な処理を行う
        ))))
|#


(define-module tir04.cgi.session.ident
  (use srfi-1)
  (use gauche.parameter)
  (use text.html-lite)
  (use www.cgi)
  (use tir04.cgi.util)
  (use tir04.cgi.ident)
  (extend tir04.cgi.session)

  (export
    <cgi-session-ident>
    with-cgi-session-ident
    make-cgi-session-ident-url
    make-cgi-session-ident-form
    ))
(select-module tir04.cgi.session.ident)


(define-class <cgi-session-ident> (<cgi-session>)
  (
   (cgi-ident ; 認証インスタンスを指定する
     :accessor cgi-ident-of
     :init-keyword :cgi-ident
     :init-value #f)
   (dispatch-keyname
     ;; with-cgi-session-identで処理を横取りする為のcgiパラメータのkey文字列。
     ;; 他のkeyと被らないようにする事。
     :accessor dispatch-keyname-of
     :init-keyword :dispatch-keyname
     :init-value "tir04-cgi-session-ident")
   ))


(define-method initialize ((self <cgi-session-ident>) initargs)
  (next-method)
  (unless (cgi-ident-of self)
    (error ":cgi-ident must be required"))
  )


(define-method with-cgi-session-ident ((self <cgi-session-ident>) params proc)
  (with-cgi-ident
    (cgi-ident-of self)
    params
    (lambda (params-2 ident-info)
      (cgi-with-session
        self
        (lambda (old-session-data)
          (let ((dispatch-val (cgi-get-parameter (dispatch-keyname-of self)
                                                 params-2))
                (params-3 (alist-delete (dispatch-keyname-of self)
                                        params-2
                                        equal?)))
            (cond
              (ident-info (do-login self ident-info params-3))
              ((equal? dispatch-val "logout") (do-logout self params-3))
              (else
                (let ((session-ident-info (and
                                           old-session-data
                                           (car old-session-data)))
                      (session-parameter (make-parameter
                                           (and
                                             old-session-data
                                             (cadr old-session-data)))))
                  (let1 result (proc params-3
                                     session-ident-info
                                     session-parameter)
                    ;; 既にセッションが存在する時のみ、updateする
                    (when old-session-data
                      (cgi-update-session! self (list
                                                  (car old-session-data)
                                                  (session-parameter))))
                    result))))))))))

(define (do-login self ident-info params)
  (cgi-create-session! self (list ident-info #f))
  (location
    (append-params-to-url
      (self-url/path-info)
      params)))

(define (do-logout self params)
  (cgi-remove-session! self)
  (location
    (append-params-to-url
      (self-url/path-info)
      params)))


(define-method make-cgi-session-ident-url ((self <cgi-session-ident>)
                                          callback-params type)
  (case type
    ((login) (make-ident-url (cgi-ident-of self) callback-params))
    ((logout) (append-params-to-url
                (self-url/path-info)
                (list*
                  `(,(dispatch-keyname-of self) "logout")
                  callback-params)))
    (else (error "assertion"))))


(define-method make-cgi-session-ident-form ((self <cgi-session-ident>)
                                           callback-params
                                           type
                                           html-tree . keywords)
  (case type
    ((login) (apply make-ident-form
                    (cgi-ident-of self)
                    callback-params
                    html-tree
                    keywords))
    ((logout) (apply
                make-form
                (self-url/path-info)
                (list*
                  `(,(dispatch-keyname-of self) "logout")
                  callback-params)
                (or html-tree (html:input
                                :type "submit"
                                :value "logout"))
                keywords))
    (else (error "assertion"))))




(provide "tir04/cgi/session/ident")
