;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; はてな認証を行うクラス

;;; 概要:
;;; このモジュールは、はてな認証を行う為のクラスを提供する。
;;; このクラスは、ユーザの識別機能のみを提供する為、別途、
;;; セッションクラスを用意する必要がある(tir04.cgi.session)。
;;; または、セッションとは別に、
;;; 何らかの重要な副作用を持つ行動(データの削除や物品の購買等)を
;;; ユーザが行う際の単体確認用途としても利用可能。

;;; NB: はてな認証では、ident-infoの取得は副作用を持つ為、
;;;     ident-infoが取得できた時の返り値は、
;;;     locationによるリダイレクトとするべきである。
;;;     そうしないと、ブラウザの履歴移動によってもう一度アクセスされた際に
;;;     同じcertによる認証確認が行われてしまい、一つのcertによって行える
;;;     認証確認は一回きりなので、エラーが表示されてしまう。

;;; NB: 上記の制約に伴い、このモジュールを使ってident-infoを取得した際には、
;;;     余分な「cert」のパラメータが追加されてしまう。
;;;     このパラメータ自体は内部で削除するようになっているが、
;;;     コールバックパラメータとして「cert」のパラメータ名を使用する事は
;;;     できない。
;;;     よって、この認証モジュールを使用する際には、
;;;     「cert」のパラメータ名を使わないように注意する必要がある。

#|
(define *cgi-ident*
  (make <cgi-ident-hatena>
        :internal-key-prefix "cgi-ident"
        :error-html-proc ...
        :error-html-keywords '(...)
        :api-key "..."
        :sec-key "..."
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
        ;;
        ;; これは重要なので、もう一度繰り返しておく。
        ;; はてな認証では、ident-infoの取得は副作用を持つ為、
        ;; ident-infoが取得できた時の返り値は、
        ;; locationによるリダイレクトとするべきである。
        ;; そうしないと、ブラウザの履歴移動によってもう一度アクセスされた際に
        ;; 同じcertによる認証確認が行われてしまい、一つのcertによって行える
        ;; 認証確認は一回きりなので、エラーが表示されてしまう。
        ;; '(
        ;;   :ident-type 'hatena ; 固定
        ;;   :ident-path "" ; 固定
        ;;   :ident-id "..." ; はてなID
        ;;   :uid "..." ; "hatena::はてなID"をsha1 digestしてhexifyした文字列
        ;;   :x-ident-info '(("name" "はてなID")
        ;;                  ("image_url" "プロフィール画像のURL")
        ;;                  ("thumbnail_url" "プロフィール画像サムネイルのURL")
        ;;                  )
        ;;   )
        ;; 認証ページへのurlを生成。
        (make-ident-url *cgi-ident* callback-params)
        ;; 認証ボタン生成
        (make-ident-form *cgi-ident* callback-params html-tree . keywords)
        ... ; 様々な処理を行う
        ))))
|#


(define-module tir04.cgi.ident.hatena
  (use text.tree)
  (use text.html-lite)
  (use www.cgi)
  (use tir04.cgi.util)
  (extend tir04.cgi.ident)
  (use rfc.http)
  (use rfc.md5)
  (use rfc.sha1)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use sxml.tools)

  (export
    <cgi-ident-hatena>
    with-cgi-ident
    make-ident-url
    make-ident-form
    ))
(select-module tir04.cgi.ident.hatena)


(define *hatena-ident-url* "http://auth.hatena.ne.jp/auth")
(define *hatena-api-server* "auth.hatena.ne.jp")
(define *hatena-api-path* "/api/auth.xml")



(define-class <cgi-ident-hatena> (<cgi-ident>)
  (
   (api-key
     :accessor api-key-of
     :init-keyword :api-key
     :init-value #f)
   (sec-key
     :accessor sec-key-of
     :init-keyword :sec-key
     :init-value #f)
   ))


(define-method initialize ((self <cgi-ident-hatena>) initargs)
  (next-method)
  (unless (api-key-of self)
    (error ":api-key must be required"))
  (unless (sec-key-of self)
    (error ":sec-key must be required"))
  )


(define (get-callback-path)
  (if (cgi-get-metavariable "PATH_INFO")
    (cgi-get-metavariable "REQUEST_URI")
    ""))

(define (redirect self cpath true-params)
  (location
    (append-params-to-url
      cpath
      (cons
        (list (dispatch-key-of self) "logined")
        true-params))))

;; 子クラスが実装すべきmethods
(define-method dispatch-or-ident-info ((self <cgi-ident-hatena>) dispatch-val
                                                        cgi-params
                                                        true-params
                                                        ident-info-cont)
  (if (equal? "logined" dispatch-val)
    (let1 cpath (or
                  (cgi-get-parameter (internal-key self "cpath") cgi-params)
                  "")
      (if (#/^\// cpath)
        (redirect self cpath true-params)
        (try-get-ident-info self true-params ident-info-cont)))
    (let1 params-without-api-sig (list*
                                   `("api_key" ,(api-key-of self))
                                   `(,(dispatch-key-of self) "logined")
                                   `(,(internal-key self "cpath") ,(get-callback-path))
                                   true-params)
      (location
        (append-params-to-url
          *hatena-ident-url*
          (cons
            (list "api_sig" (make-api-sig self params-without-api-sig))
            params-without-api-sig))))))


(define (make-api-sig self params)
  (digest-hexify
    (md5-digest-string
      (tree->string
        (list
          (sec-key-of self)
          (sort
            params
            (lambda (x y)
              (string<? (car x) (car y)))))))))

(define (try-get-ident-info self params ident-info-cont)
  (guard (e (else (get-error-html self (ref e 'message))))
    (ident-info-cont (get-ident-info self params))))

(define (get-ident-info self params)
  (let* ((cert (or (cgi-get-parameter "cert" params) (error "invalid access")))
         (api-sig (make-api-sig
                    self
                    `(("api_key" ,(api-key-of self))
                      ("cert" ,cert))))
         (hatena-api-path (append-params-to-url
                            *hatena-api-path*
                            `(("api_key" ,(api-key-of self))
                              ("cert" ,cert)
                              ("api_sig" ,api-sig))))
         )
    (receive (status-code headers body) (http-get
                                          *hatena-api-server*
                                          hatena-api-path
                                          :user-agent (x->string
                                                        (module-name
                                                          (current-module)))
                                          )
      (unless (#/^200/ (or status-code ""))
        (errorf "~aでエラーが発生しました。" *hatena-api-server*))
      (let1 sxml (with-input-from-string
                   body
                   (cut ssax:xml->sxml (current-input-port) '()))
        (when (equal?
                "true"
                ((if-car-sxpath '(// has_error *text*)) sxml))
          (error
            (or
              ((if-car-sxpath '(// message *text*)) sxml)
              "assertion")))
        (let1 x-ident-info (map
                             (lambda (elem)
                               (list
                                 (x->string (sxml:element-name elem))
                                 (or ((if-car-sxpath '(*text*)) elem) "")))
                             ((sxpath '(// user *)) sxml))
          (let1 ident-id (cadr (assoc "name" x-ident-info))
            `(
              :ident-type hatena
              :ident-path ""
              :ident-id ,ident-id
              :uid ,(digest-hexify
                      (sha1-digest-string
                        (string-append "hatena::" ident-id)))
              :x-ident-info ,x-ident-info
              )))))))


(define-method get-remove-params-key-list ((self <cgi-ident-hatena>))
  '("cert"))


(define-method with-cgi-ident ((self <cgi-ident-hatena>) cgi-params proc)
  (next-method))

(define-method make-ident-url ((self <cgi-ident-hatena>) misc-params)
  (next-method))

(define-method make-ident-form ((self <cgi-ident-hatena>)
                               misc-params
                               html-tree . keywords)
  (next-method))


(provide "tir04/cgi/ident/hatena")
