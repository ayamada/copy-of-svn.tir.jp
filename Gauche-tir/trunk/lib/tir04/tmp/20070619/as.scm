;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; account scheduler

;;; 概要:
;;; カレンダー付き手帳アプリ。

;;; TODO: どこから修正/作成するか考える。
;;; - データ操作部分を独立プロセス化する？
;;; -- とりあえず、ライブラリ分割ぐらいはすべき。
;;; --- モジュール名は？
;;; ---- 仮に、as.storageとする
;;; -- プロトコルはSETP
;;; - 問題点は、ファイルの送受信の効率が悪い事
;;; -- サーバをまたがないという前提で良いなら、
;;;    ファイルパスのみを転送する事でもokだし、
;;;    ファイルで送受信したいものは大きなサイズのアーカイヴや画像/映像/音声
;;;    などの事が多い為、それら本体をジャーナリングログに残す意味は薄く、
;;;    それなら転送せずに反映する方法でもよいと思われる

;;; TODO: url構成を変更する。
;;; - /yyyy/mm/dd ではなく、
;;;   /groupname/yyyy/mm/dd とする。
;;;   groupnameに旧仕様のregionを入れ、そこの
;;;   .metadataファイルに、グループ設定を入れる。
;;; - 各アカウントは最初、一つだけのグループに属し、
;;;   プライベートな情報はそこに書き込むという仕様にする。

;;; TODO: 識別情報登録時に、
;;;       最初に登録したアカウントを、
;;;       強制的に、root(""だか'()だか#fだか)の所有者とするように
;;;       する事。
;;;       そして、上位ディレクトリ/グループの所有者は、
;;;       下位ディレクトリ/グループへの権限を持つ、という仕様とする。
;;;       これによって、権限を管理できるようにする。


;;; TODO: MIME typeを決める。
;;; - Content-Type: application/as; data-type=moge
;;; -- application/as以外の場合は、そのまま出力する。


;;; TODO:
;;; - http://as.tir.jp/one に設置する。
;;; - プラグインシステムを実装する事。
;;; - ジャーナリングログが必要。
;;; -- これはsyslog転送のような、汎用の仕組みにすべきか？
;;; - データ暗号化があるのが好ましいが、必須ではない
;;; - PATH_INFOではなく、dnsのAAAAレコードを使うか？
;;; -- wikiならそれでもいいが、今回はカレンダー機能が軸になる為、
;;;    逆順になるのが好ましくないので、PATH_INFO使用とする。

;;; TODO: これは、httpd化した方がいいかも知れない
;;;       (しかし、httpd化には時間がかかる為、そうするにしても、
;;;        まず、一通り動作するものが完成してからにする事！)

;;; urlについて:
;;; - PATH_INFOをwikinameとして使う。
;;; - wikiname自体は基本的に\w系文字のみとする。
;;; -- %nnエンコードが必要になる名前は不可。
;;; -- 内部情報構成の都合上、.も不可。
;;; -- 間違わないように、小文字のみでの構成とする。大文字不可。
;;; -- アンダーバーはok。
;;; - 区切り文字は/。
;;; -- rootのみが、PATH_INFOの末尾を/で終える事が可能とする。
;;; -- root以外は、PATH_INFOの末尾を/で終えないようにする。
;;; --- とは言え、内部的には、path-info-keylistを使っているので、
;;;     どちらなのかは区別できないが。
;;;     ただ、このシステムからPATH_INFO付きurlを出す際には、
;;;     root以外は末尾を/で終えないようにする、という約束とする。

;;; note:
;;; - 内部形式はwikiに近い状態とする。
;;; -- データは、PATH_INFOの通りのディレクトリを作り、
;;;    そのディレクトリ内に.contentというファイルを作り、
;;;    その中にrfc822形式でメタデータ込みで保存する。
;;;    (つまり、追記okかつヘッダにメタデータを持たせる事を可能にする)

;;; TODO: 一通り完成したら、再利用可能なように、各部分をモジュールに分解する。
;;; - content-treeを操作/レンダリングする部分
;;; -- rfc822型ファイル操作ユーティリティ
;;; - 各種プラグインシステム
;;; - アカウントマネージャ
;;; - その他

;;; TODO: データのメモリキャッシュの実装


;;; TODO: 同一アカウントに別々の接続元から同時にログインできない仕組みを
;;;       実装する必要がある


(define-module tir04.tmp.20070619.as
  (use gauche.parameter)

  (use rfc.822)
  (use file.util)
  (use util.list)
  (use dbm)
  (use text.html-lite)
  (use text.tree)
  (use www.cgi)
  (use tir04.cgi.util)
  (use tir04.cgi.ident)
  (use tir04.cgi.session.ident)
  (use tir04.dbm.util)

  (use srfi-19)
  (use gauche.sequence)
  (use srfi-1)

  ;(use wc-frame)
  (use tir04.util.calendar)


  (use tir04.tmp.20070619.as.storage)

  (export
    <cgi-as>
    cgi-as-execute
    ))
(select-module tir04.tmp.20070619.as)


(define-class <cgi-as> ()
  (
   ;; TODO: 情報定義スロットがとりあえず必要
   ;;       (とりあえず、秘密にすべき設定値はスロットで持たせる事)
   (information-keywords
     :accessor information-keywords-of
     :init-keyword :information-keywords
     :init-value '())
   (setting-keywords
     :accessor setting-keywords-of
     :init-keyword :setting-keywords
     :init-value '())
   (cgi-ident
     :accessor cgi-ident-of
     :init-keyword :cgi-ident
     :init-value #f)

   ;; private slot
   (csa
     :accessor csa-of
     :init-value #f)
   ))


(define-method informations-of ((self <cgi-as>) keyword . opt-fallback)
  (apply get-keyword keyword (information-keywords-of self) opt-fallback))

(define-method settings-of ((self <cgi-as>) keyword . opt-fallback)
  (apply get-keyword keyword (setting-keywords-of self) opt-fallback))

(define-method initialize ((self <cgi-as>) initargs)
  (next-method)
  (unless (file-exists? (settings-of self :storage-dir))
    (make-directory* (settings-of self :storage-dir)))
  (set!
    (csa-of self)
    (make
      <cgi-session-ident>
      :dbm-type (settings-of self :dbm-type)
      :dbm-path (string-append
                  (settings-of self :storage-dir)
                  "/"
                  "csa")
      :expire-second (* 1 24 60 60)
      :cgi-ident (cgi-ident-of self)))
  #t)



;;; ----




(define (make-response:not-logined self params)
  (make-page
    self
    (list
      (html:h1 (het (informations-of self :title)))
      (html:ul
        (html:li
          (html:login/logout self 'login "ログイン")))
      )))

(define (html:login/logout self type label)
  (make-cgi-session-ident-form
    (csa-of self)
    '()
    type
    (html:input
      :type "submit"
      :value label)))


(define (path-info-keylist->date path-info-keylist)
  (define (str-is-number-only? str)
    (and
      str
      (not
        (#/\D/ str))))
  (define (path-info-keylist->year+month+day+rest)
    ;; まず、最初の要素をチェック
    (let1 year? (car path-info-keylist)
      (if (not
            (and
              (str-is-number-only? year?)
              (= 4 (string-size year?))))
        (values #f #f #f path-info-keylist)
        (let1 month? (and
                       (not (null? (cdr path-info-keylist)))
                       (cadr path-info-keylist))
          (if (not
                (and
                  (str-is-number-only? month?)
                  (= 2 (string-size month?))))
            (values (x->integer year?)
                    1
                    1
                    (cdr path-info-keylist))
            (let1 day? (and
                         (not (null? (cddr path-info-keylist)))
                         (caddr path-info-keylist))
              (if (not
                    (and
                      (str-is-number-only? day?)
                      (= 2 (string-size day?))))
                (values (x->integer year?)
                        (x->integer month?)
                        1
                        (cddr path-info-keylist))
                (values
                  (x->integer year?)
                  (x->integer month?)
                  (x->integer day?)
                  (cdddr path-info-keylist)))))))))
  (cond
    ((not path-info-keylist) (current-date))
    ((null? path-info-keylist) (current-date))
    (else
      ;; まず、path-info-keylistから、年月日である可能性のある要素を取り出す
      (receive (year month day rest) (path-info-keylist->year+month+day+rest)
        (values
          (if (not year)
            (current-date)
            (make-date 0 0 0 0 day month year (date-zone-offset
                                                (current-date))))
          rest)))))
          
(define (with-dbm self dbm-name rw-mode proc)
  (with-dbm-open
    (settings-of self :dbm-type)
    :path (string-append
            (settings-of self :storage-dir)
            "/"
            (x->string dbm-name))
    :rw-mode rw-mode
    :key-convert #t
    :value-convert #t
    proc))

(define (get-user-info self ident-info)
  ;; dbmからuser-infoを読む
  (guard (e (else #f)) ; TODO: エラー処理
    (with-dbm
      self
      "user-info"
      :read
      (lambda (dbm)
        (dbm-get dbm (get-keyword :uid ident-info) #f)))))

(define (set-user-info! self ident-info user-info)
  (guard (e (else #f)) ; TODO: エラー処理
    (with-dbm
      self
      "user-info"
      :write
      (lambda (dbm)
        (dbm-put! dbm (get-keyword :uid ident-info) user-info)))))

(define (make-response:logined:user-info
          self params ident-info path-info-keylist user-info)
  (let1 cc (cgi-get-parameter "cc" params)
    ;; まずディスパッチする
    (cond
      ((equal? "confirm" cc)
       (make-response:logined:user-info:confirm
         self params ident-info user-info))
      ((equal? "submit" cc)
       (make-response:logined:user-info:submit
         self params ident-info user-info))
      ((equal? "done" cc)
       (make-response:logined:user-info:done
         self params ident-info user-info))
      (else ; or "edit"
        (make-response:logined:user-info:edit
          self params ident-info user-info)))))

(define (make-response:logined:user-info:confirm
          self params ident-info user-info)
  ;; TODO: 入力内容のチェックが必要
  (let ((name (cgi-get-parameter "name" params)))
    (make-page
      self
      (make-form
        (self-path/path-info)
        `(
          ("c" "ui")
          ("cc" "submit")
          ("name" ,name)
          )
        (list
          (html:p "以下の内容で設定を行います。")
          (html:div
            "名前: "
            (html:tt
              (het name)))
          (html:input
            :type "submit"
            :value "本当に設定する")
          )))))
(define (make-response:logined:user-info:submit
          self params ident-info user-info)
  ;; TODO: 入力内容のチェックが必要
  (let ((name (cgi-get-parameter "name" params)))
    (set-user-info! self ident-info `(:name ,name))
    (location
      (append-params-to-url
        (self-path/path-info)
        '(("c" "ui")
          ("cc" "done"))))))
(define (make-response:logined:user-info:done
          self params ident-info user-info)
  (make-page
    self
    (list
      (html:p "設定が完了しました。")
      (html:ul
        (html:li
          (html:a
            :href (self-path/path-info)
            "戻る"))))))
(define (make-response:logined:user-info:edit
          self params ident-info user-info)
  ;; 設定項目の編集画面を出す
  ;; TODO: まず、設定可能項目を決める必要がある。
  ;; - 名前
  ;; - 他には？？？
  (define (read-from-user-info keyword . opt-fallback)
    (if (not user-info)
      (car opt-fallback)
      (apply get-keyword keyword user-info opt-fallback)))

  (make-page
    self
    (make-form
      (self-path/path-info)
      '(
        ("c" "ui")
        ("cc" "confirm")
        )
      (list
        (if (not user-info)
          (html:p "識別情報が設定されていません。設定を行ってください。")
          (html:p "識別情報を編集できます。"))
        (html:div
          "名前: "
          (html:input
            :type "text"
            :name "name"
            :value (read-from-user-info :name "")))
        (html:input
          :type "submit"
          :value "設定する")
        (html:p
          "(今のところ、名前しか設定できません。"
          "設定項目は今後、増える予定です)")
        (if (not user-info)
          '()
          (list
            (html:hr)
            (html:ul
              (html:li
                (html:a :href (self-path/path-info)
                        "(戻る)")))))))))

(define (do-redirect self src-path params)
  ;; まず、pathを検証する。
  ;; /始まりで、以降は\wまたは/のみを含む文字列とする。
  ;; 但し、pathが空なら、/が指定されたものとして扱う。
  (let1 path (if (string=? "" src-path)
               "/"
               src-path)
    (or
      (and-let* ((m (#/^\/[\/\w]*$/ path)))
        (location
          (append-params-to-url
            (string-append (self-path) path)
            (remove
              (lambda (x)
                (equal? (car x) "redirect_path"))
              params))))
      (make-page
        self
        (list
          (html:h1 "error")
          (html:div
            "不正なpathが指定されました"))))))

(define (make-response:logined
          self params path-info-keylist ident-info session-parameter)
  ;; まず、ユーザ情報を取り出す
  ;; ユーザ情報が存在しない場合は、強制的に設定画面へと遷移させる
  (let1 user-info (get-user-info self ident-info)
    (if (not user-info)
      (make-response:logined:user-info
        self params ident-info (current-date) user-info)
      ;; CGI引数の解釈を行う
      (let (
            (redirect-path (cgi-get-parameter "redirect_path" params))
            (c (cgi-get-parameter "c" params))
            )
        (if redirect-path
          (do-redirect self redirect-path params)
          (cond
            ((equal? "ui" c) (make-response:logined:user-info
                               self params ident-info path-info-keylist user-info))
            ((equal? "edit" c) (make-response:logined:edit
                                 self params ident-info path-info-keylist user-info))
            ((equal? "submit" c) (make-response:logined:submit
                                   self params ident-info path-info-keylist user-info))
            (else
              ;; fallback or "display"
              (make-response:logined:display
                self params ident-info path-info-keylist user-info))))))))

(define (make-month m y)
  (make-date 0 0 0 0 1 m y (date-zone-offset (current-date))))

(define (date->prev-month date)
  (if (= (date-month date) 1)
    (make-month 12 (- (date-year date) 1))
    (make-month (- (date-month date) 1) (date-year date))))

(define (date->next-month date)
  (if (= (date-month date) 12)
    (make-month 1 (+ (date-year date) 1))
    (make-month (+ (date-month date) 1) (date-year date))))

(define (url-chop url)
  ;; urlの末尾の要素を落とした文字列を返す。
  (let1 m (#/\/[^\/]*\/?$/ url)
    (if m
      (m 'before)
      url)))

(define (calendar-html path-info-keylist edit-mode?)
  (let1 date (path-info-keylist->date path-info-keylist)
    ;; TODO: 各日付にリンクをつける
    ;;       先月/来月も含める
    ;;       先月/来月へのリンクもつける(nnk互換)
    ;; 以下のようなフォーマットとする。
    ;; ←    2007/07     → 
    ;; 日 月 火 水 木 金 土 
    ;; 01 02 03 04 05 06 07 
    ;; 08 09 10 11 12 13 14 
    ;; 15 16 17 18 19 20 21 
    ;; 22 23 24 25 26 27 28 
    ;; 29 30 31 01 02 03 04
    ;; TODO: input text形式の入力ボックスもつけるべき？
    ;;       今はつけない。つけるとしても後で。
    (define (date->url-month date)
      (format "~a/~4,'0d/~2,'0d"
              (self-path)
              (date-year date)
              (date-month date)))
    (define (date->url-day date)
      (format "~a/~4,'0d/~2,'0d/~2,'0d"
              (self-path)
              (date-year date)
              (date-month date)
              (date-day date)))
    (define (url-filter url)
      (if edit-mode?
        (append-params-to-url
          url
          '(("c" "edit")))
        url))

    (let (
          (prev-month (date->prev-month date))
          (next-month (date->next-month date))
          )
      (html:table
        :summary "calendar"
        (html:thead
          (html:tr
            (html:th :colspan "1" '())
            (html:th
              :colspan "1"
              (if (null? path-info-keylist)
                "↑"
                (html:a :href (url-filter
                                (let1 new-url (url-chop (self-path/path-info))
                                  (if (< (string-length (self-path))
                                         (string-length new-url))
                                    new-url
                                    (self-path/slash))))
                        "↑")))
            (html:th
              :colspan "3"
              (html:a :href (url-filter (date->url-day (current-date)))
                      "今"))
            (html:th
              :colspan "1"
              (if (null? path-info-keylist)
                "√"
                (html:a :href (url-filter (self-path/slash))
                        "√")))
            (html:th :colspan "1" '())
            )
          (html:tr
            (html:th (html:a :href (url-filter (date->url-month prev-month))
                             "←"))
            (html:th
              :colspan "5"
              (html:a :href (url-filter (date->url-month date))
                      (het
                        (date->string date "~Y/~m"))))
            (html:th (html:a :href (url-filter (date->url-month next-month))
                             "→"))
            )
          (html:tr
            (map
              (lambda (x)
                (html:th
                  (het x)))
              '("日" "月" "火" "水" "木" "金" "土"))))
        (html:tbody
          (map
            (lambda (row)
              (html:tr
                (map
                  (lambda (col)
                    (html:td
                      col))
                  row)))
              (datetime->calendar-list
                (date->datetime date)
                (lambda (datetime target-day? current-month?)
                  (let1 str (format "~2,'0d" (date-day
                                               (datetime->date datetime)))
                    (define (filter-current-month str)
                      (if current-month?
                        str
                        (html:span
                          :style "font-size:0.5em"
                          str)))
                    (let1 link-filter (if edit-mode?
                                        (cut
                                          html:a
                                          :href (url-filter
                                                  (date->url-day
                                                    (datetime->date datetime)))
                                          <>)
                                        ;; TODO: edit-mode時以外にも、
                                        ;;       該当日付に対応するコンテンツが
                                        ;;       あるなら、
                                        ;;       リンク化しないといけない。
                                        ;;       あとで実装する事。
                                        identity)
                      (link-filter
                        (if (and
                              (= (date-year (datetime->date datetime))
                                 (date-year (current-date)))
                              (= (date-month (datetime->date datetime))
                                 (date-month (current-date)))
                              (= (date-day (datetime->date datetime))
                                 (date-day (current-date))))
                          ;; TODO: このままでは背景色が赤系だとまずくなる。
                          ;;       何らかのportableな方法を導入する事。
                          (html:span :style "color:red"
                                     (filter-current-month
                                       (het str)))
                          (filter-current-month
                            (het str))))))))))
        (html:tfoot
          (html:tr
            (html:th
              :colspan "7"
              (if edit-mode?
                (html:a :href (self-path/path-info)
                        "(view mode)")
                (html:a :href (append-params-to-url
                                (self-path/path-info)
                                '(("c" "edit")))
                        "(edit mode)")
                ))))))))


(define (html:page-selector-form self path-info-keylist edit-mode?)
  (make-form
    (self-path/path-info)
    (if edit-mode?
      '(("c" "edit"))
      '())
    (html:div
      "ページ直接移動: "
      (html:input
        :type "text"
        :name "redirect_path"
        :value (let1 p (string-join path-info-keylist "/" 'prefix)
                 (if (string=? p "")
                   "/"
                   p)))
      (html:input
        :type "submit"
        :value "移動")
      )))

(define (make-response:logined:edit:form
          self params ident-info path-info-keylist user-info)
  (make-page
    self
    (list
      (html:page-selector-form self path-info-keylist #t)
      (html:hr)
      (html:as-edit-form self params ident-info path-info-keylist)
      (html:hr)
      (html:div
        :style "float:right"
        (html:login/logout self 'logout "ログアウト"))
      (calendar-html path-info-keylist #t)
      )))

(define (make-response:logined:edit
          self params ident-info path-info-keylist user-info)
  (let1 cc (cgi-get-parameter "cc" params)
    ;; まずディスパッチする
    (cond
      ((equal? "preview" cc)
       (make-response:logined:edit:preview
         self params ident-info path-info-keylist user-info))
      ((equal? "submit" cc)
       (make-response:logined:edit:submit
         self params ident-info path-info-keylist user-info))
      ((equal? "done" cc)
       (make-response:logined:edit:done
         self params ident-info path-info-keylist user-info))
      (else ; or "form"
        (make-response:logined:edit:form
          self params ident-info path-info-keylist user-info)))))

(define (make-response:logined:display
          self params ident-info path-info-keylist user-info)
  (make-page
    self
    (list
      (html:page-selector-form self path-info-keylist #f)
      (html:hr)
      (html:display self params ident-info path-info-keylist)
      (html:hr)
      ;(html:h1 (het (informations-of self :title)))
      (html:div
        :style "float:right; text-align:right"
        (html:login/logout self 'logout "ログアウト")
        ;; TODO: 名前表示をすべきかは微妙
        (html:div
          "名前: " (html:tt
                       (het (get-keyword :name user-info))))
        (html:div
          (html:a :href (append-params-to-url
                          (self-path/path-info)
                          '(("c" "ui")))
                  "(識別情報編集)"))
        )
      (html:div
        :style "float:left"
        (calendar-html path-info-keylist #f))
      )))

(define (html:as-edit-form self params ident-info path-info-keylist)
  (receive (path
            path-dir
            content-lock-file
            path-content-file
            path-dir-config-file) (get-content-parameter self
                                                         path-info-keylist)
    ;; まず、現在の内容を取り出す
    ;;;;; TODO: あとで
    (let1 pair #f
      (html:div
        (make-form
          (self-path/path-info)
          '(("c" "edit")
            ("cc" "preview")
            )
          (list
            ;; TODO: Content-Type変更の為のリンクを追加
            (html:input
              :type "text"
              :name "text"
              :value "")
            (html:input
              :type "submit"
              :value "送信")
            ))))))

(define (get-content-parameter self path-info-keylist)
  (let* (
         (path (string-join
                 path-info-keylist
                 "/"
                 'prefix))
         (path-dir (string-append
                     (settings-of self :storage-dir)
                     "/"
                     "content-tree"
                     path))
         (content-lock-file (string-append
                             (settings-of self :storage-dir)
                             "/"
                             "content-tree.lock"))
         (path-content-file (string-append path-dir "/.content"))
         (path-dir-config-file (string-append path-dir "/.config"))
         )
    (values path
            path-dir
            content-lock-file
            path-content-file
            path-dir-config-file)))


(define (html:display self params ident-info path-info-keylist)
  ;; path-info-keylistから、該当データを取り出し、適切に表示する
  ;; TODO: まずは、該当するディレクトリ及び.contentファイルのpathを求めるところまで作る事
  (receive (path
            path-dir
            content-lock-file
            path-content-file
            path-dir-config-file) (get-content-parameter self
                                                         path-info-keylist)
    ;; 表示すべき情報を、以下の手順で求める
    ;; - まず、path-contentが存在するなら、それを取得する(rfc822形式)
    ;; - 取得したcontentまたは#fに対し、以下を順に適用する
    ;; -- path-dir-configによる、何らかの設定が存在するなら、それを適用する
    ;;    (結果はrfc822形式または#f)
    ;; -- pathに対応する、フィルタプラグインが存在するなら、それを適用する
    ;;    (結果はrfc822形式または#f)
    ;; -- 最後に、その結果を、Content-Typeに応じた出力プラグインを用いて、
    ;;    html形式(またはその他の形式)として出力する。
    ;;    最後まで#fのままだった場合は、404またはそれに類するページを出す。
    ;; TODO: ファイルに異常がある場合、
    ;;       (ファイルは存在するがアクセス権限が無くて読めない等)
    ;;       適切にエラーを返すようにしたい
    ;; 但し、.contentは追記型ファイルとして、膨大なデータが
    ;; 溜め込まれている可能性もある為、
    ;; rfc822形式のbody部分は直接取り込まずに、
    ;; 最初はport状態として保持しておくものとする。
    ;; (port状態か、その他の状態かは、キーワードで示すものとする)
    (define (path-dir-config-filter pair)
      ;; TODO: あとで
      pair)
    (define (path-plugin-filter pair)
      ;; TODO: あとで
      pair)
    (define (rfc822-output-filter pair)
      (if (not pair)
        (list
          (html:h1 "content not found")
          (html:div
            "このurlにはデータが存在しません。"
            (html:br)
            "おそらく、url間違いか、削除されたかのどれかです。"
            (html:br)
            "「"
            (html:a
              :href (append-params-to-url
                      (self-path/path-info)
                      '(("c" "edit")))
              "edit mode")
            "」のところから、内容を新たに作成する事も可能です。"
            ))
        ;; TODO: あとで
        (html:div "データがあります。あとで表示部分を実装します。")))

    (let1 path-content-port #f
      (dynamic-wind
        (lambda ()
          ;; TODO: 同時にロックした方がいいかも
          ;;       (しかし詳細未定)
          (set!
            path-content-port
            (open-input-file path-content-file :if-does-not-exist #f)))
        (lambda ()
          (let* ((path-content-headers (if path-content-port
                                         (rfc822-header->list
                                           path-content-port)
                                         #f))
                 (path-content-pair0 (if path-content-port
                                       (cons path-content-headers
                                             path-content-port)
                                       #f))
                 ;; まず、これをpath-dir-configに設定されたフィルタにかける
                 (path-content-pair1 (path-dir-config-filter
                                       path-content-pair0))
                 ;; 次に、これをpathに対応するプラグインフィルタにかける
                 (path-content-pair2 (path-plugin-filter
                                       path-content-pair1))
                 )
            ;; 最後に、結果を、Content-Typeに応じて適切にフォーマットして返す
            ;; 最後まで#fだった場合は、404またはそれに類する結果を返す事
            (rfc822-output-filter path-content-pair2)))
        (lambda ()
          ;; TODO: dynamic-wind突入時にロックしているなら、
          ;;       ここでアンロックする事
          (when path-content-port
            (close-input-port path-content-port))
          (set! path-content-port #f))))))


(define (make-response:logined:submit
          self params ident-info path-info-keylist user-info)
  ;; TODO: あとで
  (let1 new-msg (or (cgi-get-parameter "text" params) "")
    #f)
  (location
    (self-path)))

(define (make-page self . body)
  (cgi-tree-make
    :http-header '(:pragma "no-cache")
    :encoding (x->string (gauche-character-encoding))
    :css-url (informations-of self :css-url)
    :robots "NOINDEX,NOFOLLOW"
    :js #f
    :title (informations-of self :title)
    :body-header #f
    :body-footer (list
                   (html:hr)
                   (html:address
                     (html:a
                       :name "bottom"
                       (informations-of self :title))))
    :body body))







(define-method cgi-as-execute ((self <cgi-as>))
  (cgi-main
    (lambda (orig-params)
      (with-cgi-session-ident
        (csa-of self)
        orig-params
        (lambda (true-params ident-info session-parameter)
          (with-path-info-keylist
            (lambda (path-info-keylist)
              (if ident-info
                (make-response:logined
                  self
                  true-params
                  path-info-keylist
                  ident-info
                  session-parameter)
                (make-response:not-logined
                  self
                  true-params)))))))
    :on-error cgi-on-error/stack-trace
    )
  0)





;;; --------


(provide "tir04/tmp/20070619/as")

