;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; 概要:
;;; CGI用セッションマネージャ。
;;; セッションはcookieに保存される。

;;; note: セッションに#fを保存する事はできない。
;;;       (cgi-with-session時に、セッションが無いのか、#fが入っているのか
;;;        判断できなくなる為。)
;;;       #fを保存しようとすると、既存のセッションを削除する仕様とした。
;;;       #f相当を保存したい時は、'(#f)等にすべき。

;;; note: このモジュールでは、expiresを設定したクッキーは使用できない。
;;;       (用途がセッションの為。)


#|
(define *cgi-session*
  (make
    <cgi-session>
    :dbm-type <fsdbm>
    :dbm-path "/path/to/dbm-file"
    :expire-second (* 1 24 60 60)
    :cookie-name "cgi-session"
    :cookie-keywords '(:discard #t :path "/") ; expiresは設定しない事
    ))
(cgi-main
  (lambda (params)
    (cgi-with-session
      *cgi-session*
      (lambda (session-data)
        ...
        ))))
|#

(define-module tir04.cgi.session
  (use gauche.parameter)
  (use rfc.cookie)
  (use www.cgi)
  (extend tir04.session.dbm)

  (export
    <cgi-session>
    cgi-with-session
    cgi-create-session!
    cgi-update-session!
    cgi-remove-session!
    ))
(select-module tir04.cgi.session)


(define-class <cgi-session> (<session-dbm>)
  (
   ;; セッションクッキーについての情報
   (cookie-name
     :accessor cookie-name-of
     :init-keyword :cookie-name
     :init-value "cgi-session")
   ;; keywordsの詳細は、rfc.cookieのconstruct-cookie-stringを参照
   (cookie-keywords
     :accessor cookie-keywords-of
     :init-keyword :cookie-keywords
     :init-value '(
                   :discard #t
                   :path "/"
                   ))
   ;; 整形されたSet-Cookieヘッダを一時的に記憶するスロット
   (cookie-headers
     :accessor cookie-headers-of
     :init-form (make-parameter #f))
   ;; その他のスロットについては、<session-dbm>と<session>を参照
   ))


(define-method initialize ((self <cgi-session>) initargs)
  (next-method)
  ;; 今のところは、特に処理無し
  #t)


;;; --------


(define-method cgi-with-session ((self <cgi-session>) proc)
  ;; note: 以下の点が、元のwith-sessionと異なっている。
  ;;       - cookieからsidを読み出す事。
  ;;       - procの返り値に、必要であればSet-Cookieヘッダを追加する事。
  ;; note: もし今後、クッキーにexpiresを使えるようにする場合は、
  ;;       この手続きは毎回、Set-Cookieを出力するように直す必要がある。
 (let1 sid (cgi-get-parameter
             (cookie-name-of self)
             (cgi-parse-parameters :query-string ""
                                   :merge-cookies #t))
    (parameterize (((cookie-headers-of self) '()))
      (let1 result (with-session self sid proc)
        (list ((cookie-headers-of self)) result)))))

(define-method cgi-create-session! ((self <cgi-session>) session-data)
  (let1 sid (create-session! self session-data)
    ;; ((cookie-headers-of self))を更新する
    ((cookie-headers-of self) (list
                                "Set-Cookie: "
                                (car ; 今のところ、cookieは一つ固定
                                  (construct-cookie-string
                                    `((,(cookie-name-of self) ,sid
                                       ,@(cookie-keywords-of self)))
                                    0))
                                "\r\n"))))
(define-method cgi-create-session! (session-data)
  (unless (cgi-session)
    (error "cannot found <cgi-session>"))
  (cgi-create-session! (cgi-session) session-data))

(define-method cgi-update-session! ((self <cgi-session>) session-data)
  ;; 今のところ、追加が必要な処理は無し
  (update-session! self session-data))
(define-method cgi-update-session! (session-data)
  (unless (cgi-session)
    (error "cannot found <cgi-session>"))
  (cgi-update-session! (cgi-session) session-data))

(define-method cgi-remove-session! ((self <cgi-session>))
  ;; 今のところ、追加が必要な処理は無し
  ;; (可能なら、古いクッキーも消すようにしたいが、ちょっと無理っぽい)
  (remove-session! self))
(define-method cgi-remove-session! ()
  (unless (cgi-session)
    (error "cannot found <cgi-session>"))
  (cgi-remove-session! (cgi-session)))


;;; --------


(provide "tir04/cgi/session")
