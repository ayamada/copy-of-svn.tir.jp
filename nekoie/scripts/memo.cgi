#!/usr/local/gauche/bin/speedygosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(use dbm)
(use dbm.fsdbm)
(use gauche.parameter)
(use gauche.charconv)
(use srfi-1)
(use www.cgi)
(use text.html-lite)
(use text.tree)
(use util.list)
(use file.util)
(use rfc.822)
(use srfi-13)
(use srfi-19)

(add-load-path "/home/nekoie/copy-of-svn.tir.jp/Gauche-tir/trunk/lib")
(add-load-path "/home/nekoie/copy-of-svn.tir.jp/Gauche-tir/branches/Gauche-tir01/trunk")
(use www.cgi.dispatch-tir)
(use tir04.cgi.util)
(use tir.lock)

(define *dbm-file* "/home/nekoie/data/memo/memo.dbm")
(define *lock-file* "/home/nekoie/data/memo/memo.lock")
(define *log-file* "/home/nekoie/data/memo/memo.log")
(define *encoding* (ces-guess-from-string "" "*JP"))
(define *css-url* "http://css.tir.jp/tir.css")

(define *textarea-rows* "20")
(define *textarea-cols* "40")
(define *textarea-style* "width:90%")

;;; ----
;;; misc
(define (string-empty-or . strs)
  (cond
    ((null? strs) "")
    ((or
       (not (string? (car strs))) ; 文字列でないものもパスする
       (string=? "" (car strs))) (apply string-empty-or (cdr strs)))
    (else (car strs))))

(define (format-date date)
  (date->string
    date
    "~Y/~m/~d ~H:~M:~S (~a)"))

;;; contentの改行はCRLFとする
;;; これを守る為に、修正用の手続きを用意する
(define (convert-crlf str)
  (regexp-replace-all #/(\r\n|\r|\n)/ str "\r\n"))

(define (logging sexpr)
  (with-output-to-file
    *log-file*
    (lambda ()
      (write sexpr)
      (newline))
    :if-exists :append
    :buffering :full))


(define (log-filter-map-reverse proc)
  (or
    (with-input-from-file
      *log-file*
      (lambda ()
        (let next ((result '()))
          (let1 line (read-line)
            (if (eof-object? line)
              result
              (let1 mail (guard (e (else #f))
                           (read-from-string line))
                ;; TODO: mailが正常な構造になっているかをチェックした方が良い
                (if (not mail)
                  (begin
                    ;; エラーならスキップする
                    ;; TODO: 必要ならどこかにloggingしてもよい
                    (next result))
                  (let1 one (proc mail) ; filter-map動作を行う(但し逆順)
                    (next (if one
                            (cons one result)
                            result)))))))))
      :if-does-not-exist #f)
    '()))


(define (output-html . bodies)
  (cgi-tree-make
    :encoding *encoding*
    :css-url *css-url*
    :robots "NOINDEX,NOFOLLOW,NOARCHIVE"
    :title "めも"
    :body bodies
    ))



;;; ----
;;; parameters

(define p:params (make-parameter '()))
(define p:path (make-parameter #f))
(define p:dbm (make-parameter #f))
(define (cgp name . opt-args)
  (apply cgi-get-parameter name (p:params) opt-args))


;;; ----
;;; mail accessor
;;; mailは、carがコンテンツ本体、cdrがheadersのpairとする

;;; base
(define (mail->content mail)
  (car mail))
(define (mail->headers mail)
  (cdr mail))
(define (make-mail content headers)
  (cons content headers))

;;; converter
;;; この二つは今のところ、validでないrfc822形式を扱っている為、
;;; 将来に変更される可能性有り
;;; NB: 仮に、そのままの形式で保存する事になった
#|
(define (dbmval->mail str)
  ;; strは常にvalidなrfc822形式の文字列(validでない場合は例外を投げる)
  (call-with-input-string
    str
    (lambda (p)
      (let1 headers (rfc822-read-headers p :strict? #f)
        (make-mail (get-remaining-input-string p) headers)))))
(define (mail->dbmval mail)
  (with-output-to-string
    (lambda ()
      ;; write headers
      (rfc822-write-headers (mail->headers mail))
      ;; write content
      (display (mail->content mail))
      #t)))
|#

;;; ref/update
(define (mail-header-ref mail field-name . opt-default)
  (apply rfc822-header-ref (mail->headers mail) field-name opt-default))
(define (mail-header-update-one mail field-name new-value)
  ;; インスタンスを更新するのではなく、新しいインスタンスを作ってそっちを返す
  (let1 new-headers (if (mail-header-ref mail field-name)
                      ;; 更新
                      (map
                        (lambda (old-header)
                          (let ((old-name (car old-header))
                                (old-value (cadr old-header)))
                            (if (string=?
                                  (string-downcase old-name)
                                  (string-downcase field-name))
                              (list field-name new-value)
                              old-header)))
                        (mail->headers mail))
                      ;; 追加
                      (cons
                        (list field-name new-value) ; TODO: そのままでいいか謎
                        (mail->headers mail)))
    (make-mail (mail->content mail) new-headers)))

(define (mail-header-update mail update-headers)
  ;; update-headersは、headers形式とする
  (fold
    (lambda (name&value old-mail)
      (apply mail-header-update-one old-mail name&value))
    mail
    update-headers))

(define (mail-update mail content update-headers)
  (make-mail
    content
    (mail->headers
      (mail-header-update mail update-headers))))

(define (mail->url mail)
  (string-append (self-url) "/" (mail-header-ref mail "path")))



(define (get-current-mail)
  (and
    (p:path)
    (dbm-get (p:dbm) (p:path) #f)))


;;; ----

(define-cgi-entry (memo:purge-log:done) '(("c" "p") ("cc" "d"))
  (output-html
    (html:h1 "編集履歴ログ倉庫送り完了")
    (html:p "編集履歴ログを退避しました。")
    (html:ul
      (html:li
        (html:a :href (self-path) (het "一覧に戻る"))))))
(define-cgi-entry (memo:purge-log:submit) '(("c" "p") ("cc" "s"))
  #f)

(define-cgi-entry (memo:purge-log) '(("c" "p"))
  (output-html
    (html:h1 (het "編集履歴ログ倉庫送り"))
    (html:ul
      (html:li
        (html:a :href (self-path) (het "一覧に戻る"))))
    (html:hr)
    (html:p
      (html:strong
        ""
        "(ここに説明文を書く)"
        "(あとで)"
        ""
        ))
    (html:hr)
    (cgi-entry->html:form
      'memo:purge-log:submit
      :method "post"
      :target "_self"
      :submit-label "本当に編集履歴ログを倉庫に送る")
    ))




(define-cgi-entry (memo:history:desc) '(("c" "h") ("cc" "d"))
  (define (get-backup-mail)
    (let ((path (p:path))
          (serial (cgp "serial")))
      (let1 r (log-filter-map-reverse
                (lambda (mail)
                  (and
                    (equal? path (mail-header-ref mail "path"))
                    (equal? serial (mail-header-ref mail "serial"))
                    mail)))
        (if (null? r)
          #f
          (car r)))))

  (let1 mail (get-backup-mail)
    (if (not mail)
      (output-html (html:h1 "該当するデータが見付かりません"))
      (output-html
        (html:p
          (html:strong
            (het "これは、このページの過去のバージョンです")))
        (html:ul
          (html:li
            (cgi-entry->html:a 'memo:history :label "履歴一覧に戻る"))
          (html:li
            (html:a :href (self-path/path-info)
                    (het "最新のページに戻る")))
          )
        (html:div "英語名: " (html:tt (het (mail-header-ref mail "path"))))
        (html:div "更新番号: " (html:tt (het (cgp "serial"))))
        (html:div "更新日時: "
                  (html:tt
                    (het
                      (format-date
                        (time-utc->date (mail->time-utc mail))))))
        ;; タイトル
        (html:h1 (het 
                   (string-empty-or
                     (mail-header-ref mail "subject")
                     (mail-header-ref mail "path"))))
        ;; 本文の表示
        (render mail)
        ))))

(define-cgi-entry (memo:history:list) '(("c" "h") ("cc" "l"))
  (let1 mail (get-current-mail)
    (if (not mail)
      (output-html (html:h1 "不正なアクセスです"))
      (output-html
        (html:h1 "過去の履歴一覧")
        (html:ul
          (html:li
            (html:a :href (self-path/path-info) "戻る")))
        (html:hr)
        (html:table
          (html:tr
            (html:th "更新番号")
            (html:th "タイトル")
            (html:th "更新時刻")
            (html:th "備考")
            )
          (let1 path (mail-header-ref mail "path")
            (log-filter-map-reverse
              (lambda (old-mail)
                (and
                  (equal? path (mail-header-ref old-mail "path"))
                  (let1 serial (mail-header-ref old-mail "serial")
                    (list
                      (html:tr
                        (html:th :style "text-align: right"
                                 serial)
                        (html:td
                          (cgi-entry->html:a
                            'memo:history:desc
                            :base-url (self-path/path-info)
                            :params `(("serial" ,serial))
                            :label (het
                                     (string-empty-or
                                       (mail-header-ref old-mail "subject")
                                       (mail-header-ref old-mail "path")))))
                        (html:td
                          (html:tt
                            (het
                              (format-date
                                (time-utc->date (mail->time-utc old-mail))))))
                        (html:td :style "text-align: center"
                                 "-")
                        ))))))))
        (html:hr)
        (html:h2 "解説")
        (html:ul
          (html:li "このページの過去の状態を見る事が出来ます。")
          (html:li "「追記」した際の履歴は残りません"
                   "(これは、削除してしまった文章やページを"
                   "あとで確認する為の機能なので)"
                   "。")
          (html:li "「削除」したページの履歴を見たい時は、一旦、"
                   "元のurlと同じページを新規生成してください"
                   "。")
          (html:li "履歴ログが倉庫入りになると、"
                   "古いものから順に見れなくなります"
                   "(履歴ログは溜まると重くなる為、時々倉庫送りする予定です)"
                   "。")
          )
        ))))

(define-cgi-entry (memo:history) '(("c" "h"))
  (memo:history:list))

(define (append/unshift filter-proc)
  (let ((mail (get-current-mail))
        (line (cgp "line")))
    (cond
      ((not mail)
       (output-html (html:h1 "不正なアクセスです")))
      ((equal? "" line)
       (location (self-url/path-info)))
      (else
        (let* ((timestamp? (cgp "timestamp"))
               ;; lineの末尾には常に、"\r\n"を付与する
               (line-true (if timestamp?
                            (format "~a: ~a\r\n"
                                    (format-date (current-date))
                                    line)
                            (format "~a\r\n" line)))
               (new-content (filter-proc (mail->content mail) line-true))
               (new-date (x->string (time->seconds (current-time))))
               (new-serial (x->string
                             (+ 1
                                (x->number (mail-header-ref mail "serial")))))
               (new-mail (mail-update
                           mail
                           (convert-crlf new-content)
                           `(("date" ,new-date)
                             ("serial" ,new-serial)
                             )))
               )
          ;; 保存する
          (dbm-put! (p:dbm) (p:path) new-mail)
          ;; 元のページに飛ばす
          (location (self-url/path-info))
          )))))
  
(define-cgi-entry (memo:unshift) '(("c" "u"))
  ;; unshift(先頭に追記)
  (append/unshift
    (lambda (content line)
      ;; lineの末尾は既に改行がついているので、そのまま結合するだけでok
      (string-append line content))))

(define-cgi-entry (memo:append) '(("c" "a"))
  ;; append(末尾に追記)
  (append/unshift
    (lambda (content line)
      ;; contentの末尾が改行終わりでない場合は、"\r\n"を付与する必要がある
      ;; 但し、contentが完全に空文字列の時には、"\r\n"を付与してはいけない
      (if (string=? "" content)
        line
        (string-append
          content
          (if (#/\n$/ content) "" "\r\n")
          line)))))

(define-cgi-entry (memo:delete:done) '(("c" "d") ("cc" "d"))
  (output-html
    (html:h1 "ページ削除完了")
    (html:p "ページが削除されました。")
    (html:ul
      (html:li
        (html:a :href (self-path) (het "一覧に戻る"))))))

(define-cgi-entry (memo:delete:submit) '(("c" "d") ("cc" "s"))
  (let1 mail (get-current-mail)
    (if (not (equal? (cgp "serial")
                     (mail-header-ref mail "serial")))
      (output-html
        (html:h1 "エラー")
        (html:p "このページは、あなたの確認中に、"
                "他の誰かによって変更されました。"
                (html:br)
                "もう一度、元のページを確認する事を推奨します。"
                )
        (html:hr)
        (html:ul
          (html:li
            (html:a :href (self-path/path-info)
                    "元のページを確認する"))))
      (begin
        ;; 削除する
        (guard (e (else e))
          (dbm-delete! (p:dbm) (p:path)))
        ;; 古いmailをログに残す
        (guard (e (else e))
          (logging mail))
        ;; doneにリダイレクト
        (location
          (cgi-entry->url 'memo:delete:done
                          :params `(("path" ,(p:path)))
                          :base-url (self-url)))))))

(define-cgi-entry (memo:delete:confirm) '(("c" "d") ("cc" "c"))
  (let1 mail (get-current-mail)
    (if (not mail)
      (output-html (html:h1 "不正なアクセスです"))
      (output-html
        (html:h1 (het "ページ削除"))
        (html:p
          (html:strong
            "ページ「"
            (het (mail-header-ref mail "subject"))
            "」( "
            (het (mail-header-ref mail "path"))
            " )を削除します。"
            ))
        (cgi-entry->html:form
          'memo:delete:submit
          :method "post"
          :target "_self"
          :params `(
                    ("serial" ,(mail-header-ref mail "serial"))
                    )
          :submit-label "本当に削除")
        (html:hr)
        (html:ul
          (html:li
            (html:a :href (self-path/path-info) (het "このページを見る")))
          (html:li
            (html:a :href (self-path) (het "一覧に戻る"))))
        ))))

(define-cgi-entry (memo:delete) '(("c" "d"))
  ;; delete(削除/確認付き)
  (memo:delete:confirm))

(define-cgi-entry (memo:edit:done) '(("c" "e") ("cc" "d"))
  (output-html
    (html:h1 "ページ更新完了")
    (html:p "ページが更新されました。")
    (html:ul
      (html:li
        (let1 url (self-url/path-info)
          (html:a :href url (het url))))
      (html:li
        (html:a :href (self-path) (het "一覧に戻る"))))))

(define-cgi-entry (memo:edit:submit) '(("c" "e") ("cc" "s"))
  (let1 mail (get-current-mail)
    ;; まずチェックを行う
    (or
      (and-let* ((errors (memo:edit:check mail)))
        (output-html
          (html:h1 "エラー")
          (map
            (lambda (x)
              (html:div (html:strong (het x))))
            errors)))
      (memo:edit:check-serial mail)
      (let* (
             (date (if (equal? "#t" (cgp "sage"))
                     (mail-header-ref mail "date")
                     (x->string (time->seconds (current-time)))))
             (serial (x->string
                       (+ 1
                          (x->number (mail-header-ref mail "serial")))))
             (new-mail (mail-update
                         mail
                         (convert-crlf (cgp "content"))
                         `(("subject" ,(cgp "subject"))
                           ("date" ,date)
                           ("serial" ,serial)
                           ("status" "ok")
                           )))
             )
        ;; 保存する
        (dbm-put! (p:dbm) (p:path) new-mail)
        ;; 古いmailをログに残す
        (guard (e (else e))
          (logging mail))
        ;; doneにリダイレクト
        (location
          (cgi-entry->url 'memo:edit:done
                          :base-url (self-url/path-info)))))))

(define (memo:edit:check mail)
  ;; パラメータの内容に問題がなければ、#fを返す
  ;; そうでなければ、エラー内容の文章をlistで返す
  (define (check-subject)
    (let1 subject (cgp "subject")
      (cond
        ((not subject) "タイトルがありません")
        ;((zero? (string-size subject)) "タイトルが空です")
        ((< 64 (string-size subject)) "タイトルが長すぎます")
        (else #f))))
  (define (check-content)
    (let1 content (cgp "content")
      (cond
        ((not content) "本文がありません")
        ;((zero? (string-size content)) "本文が空です")
        ((< 65535 (string-size content)) "本文が長すぎます")
        (else #f))))
  (define (check-all)
    (filter-map
      identity
      (list
        (check-subject)
        (check-content))))
  (if (not mail)
    '("このページは作成されていません")
    (let1 errors (check-all)
      (if (null? errors)
        #f
        errors))))

(define (memo:edit:check-serial mail)
  (and
    (not (equal? (cgp "serial")
                 (mail-header-ref mail "serial")))
    (output-html
      (html:h1 "エラー")
      (html:p "このページは、あなたの編集中に、"
              "他の誰かによって変更されました。"
              (html:br)
              "もう一度、元のページから編集し直してください。"
              )
      (html:hr)
      (html:p "現在のこのページと、あなたが編集した内容の差分は以下の通りです。")
      (html:p "(TODO: あとでつくる)")
      (html:hr)
      (html:p "あなたが編集した全文は以下の通りです。")
      (html:textarea :name "content"
                     :rows *textarea-rows*
                     :cols *textarea-cols*
                     :style *textarea-style*
                     (cgp "content"))
      (html:hr)
      (html:ul
        (html:li
          (html:a :href (self-path/path-info)
                  :target "_blank"
                  "元のページを別画面で開く"))
        (html:li
          (cgi-entry->html:a
            'memo:edit
            :base-url (self-path/path-info)
            :target "_blank"
            :label "元のページの編集画面を別画面で開く"))))))

(define-cgi-entry (memo:edit:preview) '(("c" "e") ("cc" "p") ("just" :*))
  (memo:edit:submit))

(define-cgi-entry (memo:edit:preview) '(("c" "e") ("cc" "p"))
  (let1 mail (get-current-mail)
    ;; まずチェックを行う
    (or
      (and-let* ((errors (memo:edit:check mail)))
        (output-html
          (html:h1 "エラー")
          (map
            (lambda (x)
              (html:div (html:strong (het x))))
            errors)))
      (memo:edit:check-serial mail)
      ;; プレビュー画面表示
      (output-html
        (html:h1 (het "プレビュー"))
        (html:p
          (html:strong
            "これは確認画面です。"
            (html:br)
            "一番下の「保存」ボタンを押してください。"
            ))
        (cgi-entry->html:form
          'memo:edit:submit
          :method "post"
          :target "_self"
          :params `(
                    ("subject" ,(cgp "subject"))
                    ("content" ,(cgp "content"))
                    ("serial" ,(cgp "serial"))
                    ("sage" ,(cgp "sage"))
                    )
          :internal-html (edit-form-preview
                           :subject (cgp "subject")
                           :content (cgp "content")
                           ))
        ))))
(define-cgi-entry (memo:edit:form) '(("c" "e") ("cc" "f"))
  ;; フォーム
  (let1 mail (get-current-mail)
    (if (not mail)
      (output-html
        (html:h1 "エラー")
        (html:p "このページは作成されていません"))
      (output-html
        (html:h1 (het "編集"))
        (html:div
          (html:a :href (self-path/path-info) "戻る"))
        (html:hr)
        (cgi-entry->html:form
          'memo:edit:preview
          :base-url (self-path/path-info)
          :params `(("serial" ,(mail-header-ref mail "serial")))
          :internal-html (edit-form
                           :subject (mail-header-ref mail "subject")
                           :content (mail->content mail)
                           ))
        (html:hr)
        (html:h2 "概要")
        (html:ul
          (html:li "好きな内容を書いてください。")
          (html:li "htmlタグは使えません。")
          (html:li "urlは勝手にリンクになります。")
          (html:li "ページを移動させたい場合は、新しいページを作って同じ内容を書き込んでから、古いページを削除してください。")
          )
        ))))

(define-cgi-entry (memo:edit) '(("c" "e"))
  ;; edit(編集/プレビュー付き/他者変更チェック有り/sage機能有り)
  (memo:edit:form))

(define (render mail)
  (html:div
    :style "border:1px black solid; padding: 4px; margin: 4px; font-family: monospace"
    (text->inline-html
      (string-empty-or (mail->content mail) "(本文がありません)")
      :url->href:keywords '(:target "_blank"))))

(define-cgi-entry (memo:view) '(("c" "v"))
  ;; view(表示)
  (let1 mail (get-current-mail)
    (cond
      ((not mail)
       (output-html
         (html:h1 "エラー")
         (html:p "ページが見付かりません")))
      (else
        (output-html
          ;; ヘッダ
          ;; 今のところはヘッダ無し
          ;; タイトル
          (html:h1 (het 
                     (string-empty-or
                       (mail-header-ref mail "subject")
                       (mail-header-ref mail "path"))))
          ;; 先頭に追加するフォーム
          (cgi-entry->html:form
            'memo:unshift
            :base-url (self-path/path-info)
            :internal-html (list
                             (html:input :type "checkbox"
                                         :name "timestamp"
                                         :value "#t"
                                         :title "日時付き")
                             (html:input :type "text"
                                         :name "line"
                                         :size "40"
                                         :value "")
                             (html:input :type "submit" :value "先頭に追記")))
          ;; 本文の表示
          (render mail)
          ;; 末尾に追加するフォーム
          (cgi-entry->html:form
            'memo:append
            :base-url (self-path/path-info)
            :internal-html (list
                             (html:input :type "checkbox"
                                         :name "timestamp"
                                         :value "#t"
                                         :title "日時付き")
                             (html:input :type "text"
                                         :name "line"
                                         :size "40"
                                         :value "")
                             (html:input :type "submit" :value "末尾に追記")))
          (html:hr)
          ;; 必要な付加情報があるなら、ここに追加する
          ;; TODO: 更新日時の表示
          ;; フッタ
          (html:div
            (html:tt
              ;; 一覧
              (html:a :href (self-path) (het "一覧に戻る"))
              " | "
              ;; 編集
              (cgi-entry->html:a 'memo:edit
                                 :base-url (self-path/path-info)
                                 :label "ページ編集")
              " | "
              ;; 履歴
              (cgi-entry->html:a 'memo:history
                                 :base-url (self-path/path-info)
                                 :label "過去の履歴")
              " | "
              ;; 削除
              (cgi-entry->html:a 'memo:delete
                                 :base-url (self-path/path-info)
                                 :label "ページ削除")
              ))
          )))))



(define-cgi-entry (memo:new:done) '(("c" "n") ("cc" "d"))
  (let1 mail (get-current-mail)
    (if (not mail)
      (output-html "正しいアクセスではありません")
      (output-html
        (html:h1 "ページ生成完了")
        (html:p "新しいページが生成されました。")
        (html:ul
          (html:li
            (cgi-entry->html:a 'memo:edit
                               :base-url (self-path/path-info)
                               :label "引き続き、ページの編集を行う"))
          (html:li
            (html:a :href (self-path) (het "一覧に戻る"))))))))

(define (memo:new:check)
  ;; パラメータの内容に問題がなければ、#fを返す
  ;; そうでなければ、エラー内容の文章をlistで返す
  (define (check-path)
    (let1 path (cgp "path")
      (cond
        ((not path) "英語名がありません")
        ((zero? (string-size path)) "英語名が空です")
        ((not (#/^[\w\-]+$/ path)) "英語名は半角英数と「_」「-」のみ使用可能です")
        ((< 32 (string-size path)) "英語名が長すぎます")
        ((dbm-get (p:dbm) path #f) "この英語名は既に存在します")
        (else #f))))
  (define (check-all)
    (filter-map
      identity
      (list
        (check-path)
        )))
  (let1 errors (check-all)
    (if (null? errors)
      #f
      errors)))

(define-cgi-entry (memo:new:submit) '(("c" "n") ("cc" "s"))
  ;; まずチェックを行う
  (or
    (and-let* ((errors (memo:new:check)))
      (output-html
        (html:h1 "エラー")
        (map
          (lambda (x)
            (html:div (html:strong (het x))))
          errors)))
    (let ((path (cgp "path"))
          (serial -1) ; ここは副作用を使って更新する
          )
      (log-filter-map-reverse
        (lambda (mail)
          (when (equal? path (mail-header-ref mail "path"))
            (let1 old-serial (x->number (mail-header-ref mail "serial"))
              (when (< serial old-serial)
                (set! serial old-serial))))
          #f)) ; 返り値は使わないので常に#fを返す
      ;; 保存する
      (dbm-put!
        (p:dbm)
        path
        (make-mail
          ""
          `(("content-type" "text/memo")
            ("path" ,path)
            ("subject" "")
            ("date" ,(x->string (time->seconds (current-time))))
            ("serial" ,(x->string (+ 1 serial)))
            ("status" "ok")
            )))
      ;; doneにリダイレクト
      (location
        (cgi-entry->url 'memo:new:done
                        :base-url (string-append (self-url) "/" path))))))

(define (edit-form-preview . keywords)
  (let-keywords keywords (
                          (subject "")
                          (content "")
                          )
    (list
      (html:div
        (html:span (het "ページのタイトル: "))
        (html:em (het subject)))
      (html:div
        (html:span (het "本文: "))
        (html:br)
        (html:div
          :style "border:1px black solid; padding: 4px; margin: 4px; font-family: monospace"
          (text->inline-html
            content
            :url->href:keywords '(:target "_blank"))))
      (html:input :type "submit" :value "保存")
      )))

(define (edit-form . keywords)
  (let-keywords keywords (
                          (subject "")
                          (content "")
                          )
    (list
      (html:div
        (html:table
          (html:tr
            (html:td "現在日時")
            (html:th
              :style "border:1px black solid; padding: 4px; margin: 4px"
              :title "トリプルクリックで選択できます"
              (het (format-date (current-date))))
            (html:td "")
            )))
      (html:div
        (html:em (het "ページのタイトル: "))
        (html:input :type "text"
                    :name "subject"
                    :size "40"
                    :value subject)
        ;(html:br)
        ;(het "(日本語でok)")
        )
      (html:div
        (html:em (het "本文: "))
        (html:br)
        (html:textarea :name "content"
                       :rows *textarea-rows*
                       :cols *textarea-cols*
                       :style *textarea-style*
                       content))
      (html:input :type "submit" :value "確認")
      " "
      (html:input :type "checkbox" :name "sage" :value "#t" :id "sage")
      (html:label :for "sage"
        (het "更新日時を変更しない(sage機能)"))
      (html:div
        :style "display: none"
        (html:br)
        (html:input :type "submit" :name "just" :value "即反映")
        "(危険)"
        )
      )))

(define-cgi-entry (memo:new:form) '(("c" "n") ("cc" "f"))
  (output-html
    (html:h1 (het "新しいページを作成"))
    (html:div
      (html:a :href (self-path/path-info) "戻る"))
    (html:hr)
    (cgi-entry->html:form
      'memo:new:submit
      :method "post"
      :target "_self"
      :internal-html (list
                       (html:div
                         (html:em (het "ページの英語名: "))
                         (html:input :type "text"
                                     :name "path"
                                     :size "24"
                                     :value "")
                         (html:br)
                         (het "(urlに使われます / 英数小文字で、記号は「_」「-」のみ使用可能 / これは一度決めたら変更できません！)"))
                       (html:input :type "submit" :value "ページ作成")))
    ))

(define-cgi-entry (memo:new) '(("c" "n"))
  ;; new(新規作成)
  (memo:new:form))

(define-cgi-entry (memo:fallback) '()
  ;; パラメータ指定無し時のfallback
  ;; この時は、(p:path)を見て判断する
  (if (p:path)
    (memo:view)
    (memo:list)))

(define (mail-date->time-utc mail-date)
  (or
    ;; mail-dateがrfc822形式のDate書式だった場合
    (and-let* ((date (rfc822-date->date mail-date)))
      (date->time-utc date))
    ;; mail-dateがtime-utc形式の小数だった場合
    (and (number? mail-date) (seconds->time mail-date))
    ;; mail-dateがtime-utc形式の小数の文字列だった場合
    (and-let* ((num (string->number mail-date)))
      (seconds->time num))
    ;; どれにもマッチしなかった場合
    #f))
(define (mail->time-utc mail)
  (and-let* ((mail-date-string (mail-header-ref mail "date")))
    (mail-date->time-utc (string->number mail-date-string))))
(define *time-zero*
  (seconds->time 0))

(define-cgi-entry (memo:list) '(("c" "l"))
  ;; list(一覧表示/更新日時降順/ページング無し)
  (let1 data (sort
               (filter
                 identity
                 (dbm-map
                   (p:dbm)
                   (lambda (key val)
                     (let1 mail val
                       ;; 削除されているなら#fを返す(表示しない)
                       ;; NB: この削除チェックは不要になった、が、
                       ;;     将来に同様の操作を行う可能性はあるので、
                       ;;     残しておく
                       (and
                         ;(not (equal? (mail-header-ref mail "status")
                         ;             "deleted"))
                         mail)))))
               (lambda (mail-a mail-b)
                 ;; ソート順は、以下で比較する
                 ;; - 最終更新日時(Date)が最近のものほど上
                 ;; - Dateが存在しないものは優先的に下
                 (< (time->seconds (or (mail->time-utc mail-b) *time-zero*))
                    (time->seconds (or (mail->time-utc mail-a) *time-zero*)))))
    ;; data(mailのlist)を、上から順に全部表示するのみ
    (output-html
      (html:h1 (het "めも一覧"))
      (html:div
        (cgi-entry->html:a 'memo:new :label "新しいページを作成する"))
      (html:hr)
      (if (null? data)
        (html:div (html:strong (het "一つもページがありません")))
        (html:table
          ;(html:tr
          ;  (html:th "タイトル")
          ;  (html:th "更新時刻")
          ;  (html:th "編集")
          ;  (html:th "削除")
          ;  )
          (map
            (lambda (mail)
              (html:tr
                (html:td
                  (html:a :href (mail->url mail)
                          (het
                            (string-empty-or
                              (mail-header-ref mail "subject")
                              (mail-header-ref mail "path")))))
                (html:td
                  (html:tt
                    (het
                      (format-date
                        (time-utc->date (mail->time-utc mail))))))
                (html:td
                  (cgi-entry->html:form 'memo:edit
                                        :base-url (mail->url mail)
                                        :submit-label "編集"))
                (html:td
                  (cgi-entry->html:form 'memo:delete
                                        :base-url (mail->url mail)
                                        :submit-label "削除"))
                ))
              data)))
      (html:hr)
      ;(html:div
      ;  (cgi-entry->html:a 'memo:backup :label "バックアップ確認(未実装)"))
      ;(html:hr)
      ;(html:div
      ;  (cgi-entry->html:a 'memo:purge-log :label "編集履歴ログを倉庫に送る"))
      ;(html:hr)
      (html:div (het "以上です"))
      )))

(define (main args)
  (cgi-main
    (lambda (params)
      (with-write-locks
        (lambda ()
          ;; dbmを用意する
          (let1 dbm #f
            (dynamic-wind
              (lambda ()
                (set! dbm (dbm-open <fsdbm>
                                    :path *dbm-file*
                                    :rw-mode :write
                                    :key-convert #f
                                    :value-convert #t
                                    ))
                #t)
              (lambda ()
                (parameterize ((p:params params)
                               (p:path (and-let* ((pl (get-path-info-keylist)))
                                         (string-join pl "/")))
                               (p:dbm dbm))
                  ;; ディスパッチ
                  ((cgi-params-dispatch params))))
              (lambda ()
                (guard (e (else #f))
                  (dbm-close dbm))
                (set! dbm #f)
                #t))))
        *lock-file*))
    :on-error cgi-on-error/stack-trace))







