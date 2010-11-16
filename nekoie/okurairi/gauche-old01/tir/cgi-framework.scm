#!/usr/local/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; usage:
;;; - CGI/1.1環境が前提
;;; - initialize-thunkスロットは、名前に反して、main-thunk実行前に毎回必ず
;;;   実行される。本当に一回のみ実行したいような物は、cgi-framework-main
;;;   実行前に行っておくか、delay/forceを使う事。
;;; -- このinitialize-thunk時にエラーが出た場合、メンテナンス画面が表示される
;;; - あとで書きます

;;; spec:
;;; - ディスパッチ、formパラメータ、path-infoパラメータ、cookieパラメータ、
;;;   sessionパラメータ等を楽に操作できるようにする
;;; -- これらのパラメータはparameterize変数経由でアクセスする
;;; - formパラメータの日本語コード自動変換
;;; - 実際に実行するthunkは、以下のどれかの値を返す事で、
;;;   HTTPクライアントに対する応答をカスタマイズする事が出来る
;;; -- HTTPレスポンスのtext.treeそのもの(従来通りの動作)
;;; -- (values :redirect url) ; リダイレクトする
;;; -- (values :html-body body) ; bodyの外に適当にxhtmlの枠が出力される
;;; -- (values :session session-alist :html-body body) ; セッション発行
;;; -- (values :session session-alist :redirect url) ; セッション発行
;;; -- (values :direct-output #t) ; 既に直接stdoutに出力されたので何もしない
;;; -- (values :cookie cookie :html-body body) ; セッション使わず直接cookie出力


;;; note: parameterizeを全面的に使って、外側だけでもスッキリした構成にする


;;'((title "(untitled)")
;;  (css-url "http://css.tir.jp/tir.css")
;;  (body-class #f)
;;  )


(define-module tir.cgi-framework
  (use gauche.parameter)
  (use srfi-2) ; and-let*
  (use rfc.uri)
  (use text.html-lite)
  (use text.tree)
  (use util.list)
  (use www.cgi)

  (use tir.cgi)

  (export
    <cgi-framework>
    cgi-framework-main
    cgi-form-param
    cgi-path-info
    cgi-cookie-param
    cgi-session-param
    ))
(select-module tir.cgi-framework)


(define-class <cgi-framework> ()
  (
   ;; 自動描画されるhtmlの枠部分の設定
   (html-settings
     :accessor html-settings-of
     :init-keyword :html-settings
     :init-value '())
   ;; エラー時の通知先メールアドレス
   (contact-mail-address
     :accessor contact-mail-address-of
     :init-keyword :contact-mail-address
     :init-value #f)

   (initialize-thunk
     :accessor initialize-thunk-of
     :init-keyword :initialize-thunk
     :init-value #f)
   (main-thunk
     :accessor main-thunk-of
     :init-keyword :main-thunk
     :init-value #f)
   (maintenance-page-thunk
     :accessor maintenance-page-thunk-of
     :init-keyword :maintenance-page-thunk
     :init-value #f)
   (error-page-thunk
     :accessor error-page-thunk-of
     :init-keyword :error-page-thunk
     :init-value #f)

   ;; 以下は、内部用スロット
   ))

(define-method initialize ((self <cgi-framework-lite>) initargs)
  (next-method)
  (unless (main-thunk-of self)
    (error "this class must be need to :main-thunk"))
  )


;; ----


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





(define (get-default-error-page self)
  (make-page
    self
    (list
      (html:p
        (hes "sorry, this service was temporarily unavailable now.")
        (html:br)
        (hes "please access later."))
      (html:p
        (hes "申し訳ないが、現在、一時的にサービスを停止している。")
        (html:br)
        (hes "また後でアクセスしてくれ。"))
      (let1 e (mail-address-of self)
        (if e
          (list
            (html:hr)
            (html:p
              (html:small
                (hes "(")
                (hes "もし、充分に長い間サービスが停止しているのであれば、")
                (hes "下のボタンを押す事によって、")
                (hes "サービス管理者に催促する事が可能")
                (hes ")")))
            (html:form
              :action (self-url)
              :method "post"
              :target "_blank"
              :style "text-align:center"
              (html:input
                :type "hidden"
                :name "error_c"
                :value "send")
              ;(hes "連絡メッセージ : ")
              ;(html:input
              ;  :type "input"
              ;  :name "error_msg"
              ;  :value "")
              (html:input
                :type "submit"
                :value "催促する")
              ))
          '())))))

(define (get-sended-page self bool)
  (make-page
    self
    (html:h1
      (hes
        (if bool
          "* 催促しました *"
          "* 催促不可能 *")))))

(define (send-mail to header-alist body)
  ;; あとで
  #f)


;;;;

(define-method cgi-framework-main ((self <cgi-framework>))
  (define (do-initialize)
    (let1 initializer (initialize-thunk-of self)
      (when initializer
        (with-error-handler
          (lambda (e)
            (report-error e)
            #f)
          (lambda ()
            (initializer)
            #t)))))
  (define (do-main)
    (cgi-main
      (lambda (params)
        ((main-proc-of self) params))))
  (define (do-error)
    (cgi-main
      (lambda (params)
        (define (r key)
          (cgi-get-parameter key params))

        (let (
              (error_c (r "error_c"))
              (error_msg (r "error_msg"))
              )
          (if (equal? error_c "send")
            (let1 email (mail-address-of self)
              (get-sended-page
                self
                (and
                  email
                  (send-mail
                    email
                    '()
                    (or error_msg "エラー通知/催促が行われました")))))
            (let1 ept (error-page-thunk-of self)
              (if ept
                (list
                  (cgi-header :pragma "no-cache")
                  (ept))
                (get-default-error-page self))))))))

  (if (do-initialize)
    (do-main)
    (do-error)))


(provide "tir/cgi-framework")


