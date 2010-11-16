#!/usr/local/gauche/bin/speedygosh
;#!/usr/local/gauche/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; S式バトラー

;;; gauche.gongに向けて、とりあえず動くまで作る事

;;; TODO: 試合中に(print ...)等を実行すると、ログに残るfeatureがあってもよい
;;;       (sayを廃止する？)

;;; TODO: エンディングは最後に特別メッセージを用意する
;;;       (ソース見ても表示されないように、別ファイルを読み込む)


(use text.tree)
(use text.html-lite)
(use srfi-1)
(use util.list)
(use www.cgi)
(use rfc.822)
(use rfc.uri)
(use rfc.http)
(use util.match)
(use gauche.parameter)
(use gauche.charconv)
(use rfc.sha1)
(use util.digest)

(use gauche.process)

(add-load-path "lib")
(use sb-text)

;;; ----
;;; グローバル値の定義
(define-constant *gosh-path* "/usr/local/gauche/bin/gosh")
(define-constant *sbattle-path* "./sbattle.scm")
(define-constant *errorlog-path* "/home/nekoie/tmp/sb-errorlog.txt")

;;; ----

;; コンディションを定義する
(define-condition-type <sbattle-timeout> <error>
  sbattle-timeout?
  )
(define-condition-type <sbattle-abnormal-exit> <error>
  sbattle-abnormal-exit?
  (status sbattle-abnormal-exit->status)
  )


;;; ----
;;; 汎用手続き類

(define (completion-uri uri server-name server-port https)
  (receive (uri-scheme
            uri-userinfo
            uri-hostname
            uri-port
            uri-path
            uri-query
            uri-fragment)
    (uri-parse uri)
    ;; uri-schemeが無い時にだけ補完する
    ;; 但し、server-nameが与えられていない場合は補完できないので、何もしない
    (if (or uri-scheme (not server-name))
      uri
      (let* ((scheme (if https "https" "http"))
             (default-port (if https 443 80))
             )
        (uri-compose
          :scheme scheme
          :userinfo uri-userinfo
          :host server-name
          :port (and
                  server-port
                  (not (eqv? default-port (x->number server-port)))
                  server-port)
          :path uri-path
          :query uri-query
          :fragment uri-fragment)))))

(define (path->url path)
  (completion-uri path
                  (cgi-get-metavariable "SERVER_NAME")
                  (cgi-get-metavariable "SERVER_PORT")
                  (cgi-get-metavariable "HTTPS")))

(define (symbol->path symbol . opt-params)
  ;; TODO: 面倒なので手続き的に書いてしまった。あとで直す事
  (let* ((params (get-optional opt-params #f))
         (str1 (symbol->string symbol)) ; 文字列化
         (str2 (regexp-replace #/^cgi\:/ str1 "")) ; 先頭がcgi:なら削る
         (path1 (or (cgi-get-metavariable "SCRIPT_NAME") "/"))
         (path2 (string-append path1 "?c=" str2))
         (path3 (if (not params)
                  path2
                  (tree->string
                    (list path2
                          "&"
                          (intersperse
                            "&"
                            (apply
                              append
                              (map
                                (lambda (key+vals)
                                  (let1 k (delay (uri-encode-string
                                                   (car key+val)))
                                    (map
                                      (lambda (val)
                                        (list
                                          (force k)
                                          "="
                                          (uri-encode-string val)))
                                      (cdr key+vals))))
                                params)))))))
         )
    path3))



(define (location url)
  (cgi-header
    :location url
    :pragma "no-cache"))

(define-syntax when/null
  (syntax-rules ()
    ((_ pred body ...) (if pred
                         (begin body ...)
                         '()))))

(define (make-cgi-response . keywords)
  (let-keywords keywords ((encoding #f) ; TODO: あとで考える
                          (extra-http-headers '())
                          ;; TODO: 必要に応じて、あとで追加する
                          . #t)
    (list
      (apply
        cgi-header
        (list*
          :pragma "no-cache"
          extra-http-headers))
      (apply make-html-response keywords))))

(define (make-html-response . keywords)
  (let-keywords keywords ((encoding (gauche-character-encoding))
                          (base-url #f)
                          (css-url #f)
                          (css-body #f)
                          (js-url #f)
                          (js-body #f)
                          (js #f)
                          (robots #f)
                          (title #f)
                          (title-format #f)
                          (title-format-args '())
                          (extra-html-headers '())
                          (html-body #f)
                          (html-body-attrs '())
                          (html-body-header #f)
                          (html-body-footer #f)
                          (html-frame-body #f)
                          . #t)
    (let ()
      (list
        ;; xml header
        (if encoding
          #`"<?xml version=\"1.0\" encoding=\",|encoding|\"?>\n"
          "<?xml version=\"1.0\"?>\n")
        ;; html doctype
        (html-doctype
          :type (if html-frame-body
                  :xhtml-1.0-frameset
                  :xhtml-1.0-transitional))
        ;; html
        (html:html
          :lang "ja-JP"
          :xml:lang "ja-JP"
          :xmlns "http://www.w3.org/1999/xhtml"
          (html:head
            (when/null encoding
              (html:meta :http-equiv "Content-Type"
                         :content #`"text/html; charset=,|encoding|"))
            (when/null base-url
              (html:base :href base-url))
            (when/null (or css-url css-body)
              (html:meta :http-equiv "Content-Style-Type"
                         :content "text/css"))
            (when/null (or js-url js-body js)
              (html:meta :http-equiv "Content-Script-Type"
                         :content "text/javascript"))
            (when/null robots
              (html:meta :name "ROBOTS"
                         :content robots))
            ;; titleの優先順位は、titleよりもtitle-formatの方を優先する
            (or
              (and
                title-format
                (guard (e (else #f))
                  (html:title
                    (html-escape-string
                      (apply format #f title-format title-format-args)))))
              (when/null title
                (html:title (html-escape-string title))))
            (when/null css-url
              (html:link :rel "Stylesheet"
                         :type "text/css"
                         :href css-url))
            (when/null css-body
              (html:style :type "text/css"
                          "<!--\n"
                          css-body
                          "\n-->"
                          ))
            (when/null js-url
              (html:script :type "text/javascript"
                           :src js-url))
            (when/null js-body
              (html:script :type "text/javascript"
                           "<!--\n"
                           js-body
                           "\n-->"
                           ))
            (when/null extra-html-headers extra-html-headers)
            )
          (when/null html-body
            (apply html:body
                   (append
                     html-body-attrs
                     (when/null html-body-header html-body-header)
                     (list html-body)
                     (when/null html-body-footer html-body-footer)
                     )))
          (when/null html-frame-body html-frame-body))))))

(define (response-frame keywords)
  (error "not yet") ; TODO: あとで
  )

(define (response-html html-body)
  (make-cgi-response
    ;; TODO: titleやcssも指定する
    ;; TODO: 統一がとれるように、他のキーワードを足す事(あとで)
    :title "S式バトラー"
    :css-url "http://css.tir.jp/tir.css"
    :robots "NOINDEX,NOFOLLOW"
    :html-body html-body))

(define (params->path params)
  (tree->string
    (list
      (cgi-get-metavariable "SCRIPT_NAME")
      "?"
      (intersperse
        "&"
        (map
          (lambda (x)
            (list
              (uri-encode-string (x->string (car x)))
              "="
              (uri-encode-string (x->string (cadr x)))))
          params)))))


(define (get-name)
  (receive (status headers body) (http-get "d.tir.jp" "/namegen.cgi")
    (if (and
          (equal? status "200")
          (< (string-size body) 64))
      (car (string-split (ces-convert body "EUC-JP") "\n"))
      "？？？")))


;;; ----
;;; playエントリのルール:
;;; - 「play:」というprefixで始まるシンボルに束縛される。
;;; - cgi:play-submitから以下の引数で呼ばれる。
;;;   (play:hoge params name sexpr-src sexpr)
;;; - cgi-mainが受け取る形式の返り値を返す。

;(define (play:check params name sexpr)
;  ;; この時点で、チェックに通っている筈なので、そのまま返すだけ
;  (cgi:play params '() #t))



;; TODO: 五人勝ち抜き対応にする必要がある
(define (play:output-result textonly? won? logs)
  (if textonly?
    (list
      (cgi-header :content-type "text/plain")
      (intersperse
        "\n"
        (map
          (lambda (x)
            (write-to-string x))
          logs)))
    (response-html
      (list
        (html:pre
          (intersperse
            "\n"
            (map
              (lambda (x)
                ;; 人間が読みやすい形式に変換する
                (let* ((cmd (cadr x))
                       (proc (hash-table-get *table:cmd->display-proc*
                                             cmd
                                             #f))
                       )
                  (if (not proc)
                    (html-escape-string
                      (write-to-string x))
                    (apply proc (car x) (cddr x)))))
              logs)))))))
(define *table:cmd->display-proc*
  (hash-table
    'eq?
    `(say    . ,(lambda (who message prior)
                  ;; TODO: priorに応じて変化させる事
                  (html-escape-string
                    (format
                      "『~a』は叫んだ！「~a」" who message))))
    `(ready  . ,(lambda (who action)
                  (let1 weapon (cond
                                 ((eq? action 'g) "Gアックス")
                                 ((eq? action 'c) "Cブレード")
                                 ((eq? action 'p) "Pシールド")
                                 (else ; #f
                                   #f))
                    (if weapon
                      (html-escape-string
                        (format
                          "『~a』は~aを構えた。" who weapon))
                      (html-escape-string
                        (format
                          "『~a』は構えを解いた。" who))))))
    `(attack . ,(lambda (who action)
                  (let1 weapon (cond
                                 ((eq? action 'g) "Gアックス")
                                 ((eq? action 'c) "Cブレード")
                                 ((eq? action 'p) "Pシールド")
                                 (else ; #f
                                   #f))
                    (if weapon
                      (html-escape-string
                        (format
                          "『~a』は~aで攻撃した。" who weapon))
                      (html-escape-string
                        (format
                          "『~a』は拳で殴りかかった。" who))))))
    `(parry  . ,(lambda (who)
                  (html-escape-string
                    (format
                      "しかし『~a』は攻撃を受け流した。" who))))
    `(stun   . ,(lambda (who)
                  (html-escape-string
                    (format
                      "『~a』は勢い余って転んでしまった。" who))))
    `(damage . ,(lambda (who num)
                  (html-escape-string
                    (format
                      "『~a』は~dのダメージを受けた。" who num))))
    `(hp     . ,(lambda (who num)
                  (html-escape-string
                    (format
                      "『~a』のHPは~dになった。" who num))))
    `(guard  . ,(lambda (who)
                  (html-escape-string
                    (format
                      "『~a』は相手の攻撃を防いだ。" who))))
    `(won    . ,(lambda (who)
                (html-escape-string
                  (format
                    "『~a』が勝った！" who))))
    `(fumble . ,(lambda (who)
                   (html-escape-string
                     (format
                       "『~a』はボーッとしている。" who))))
    `(elapse . ,(lambda (who total-steps)
                   (html-escape-string
                     (format
                       "戦闘開始から、~dステップが経過した。" total-steps))))
    `(draw   . ,(lambda (who)
                   (html-escape-string
                     (format
                       "勝負がつかないので判定が行われた。"))))
    ))


;;; ----
;;; cgiエントリのルール:
;;; - 「cgi:」というprefixで始まるシンボルに束縛される。
;;; - 引数を一つ取る(cgi-params)。
;;; - cgi-mainが受け取る形式の返り値を返す。


(define (cgi:explain params)
  (let1 html-back (html:ul
                    (html:li
                      (html:a :href (symbol->path 'menu) "戻る")))
    (response-html
      (list
        html-back
        (html:hr)
        (html:h1 "ストーリー(仮)")
        (html:pre
          (html-escape-string *text:story*))
        (html:h1 "概要")
        (html:pre
          (html-escape-string *text:brief*))
        (html:hr)
        html-back))))





;; エラーがあるならそのlistを、そうでなければ#fを返す
;; 尚、ここでは簡単なチェックしか行われない。
;; S式等の正確な検証はsbattle.scmで行われる事に注意する事。
(define (check-freeplay-parameter params)
  (let/cc return
    (define (goto-error . msgs)
      (return msgs))

    (let (
          (p1name (or (cgi-get-parameter "p1name" params) ""))
          (p2name (or (cgi-get-parameter "p2name" params) ""))
          (p1sexpr (or (cgi-get-parameter "p1sexpr" params) ""))
          (p2sexpr (or (cgi-get-parameter "p2sexpr" params) ""))
          (confirm (cgi-get-parameter "confirm" params))
          )
      (when (equal? "" p1name)
        (goto-error "キャラクタ1の名前が空です"))
      (when (equal? "" p2name)
        (goto-error "キャラクタ2の名前が空です"))
      (when (< 64 (string-size p1name))
        (goto-error "キャラクタ1の名前が長すぎます"))
      (when (< 64 (string-size p2name))
        (goto-error "キャラクタ2の名前が長すぎます"))
      (when (equal? p1name p2name)
        (goto-error "キャラクタ1とキャラクタ2の名前が同じです"))
      (when (equal? "" p1sexpr)
        (goto-error "キャラクタ1のS式が空です"))
      (when (equal? "" p2sexpr)
        (goto-error "キャラクタ2のS式が空です"))
      (when (< 2048 (string-size p1sexpr))
        (goto-error "キャラクタ1のS式が長すぎます"))
      (when (< 2048 (string-size p2sexpr))
        (goto-error "キャラクタ2のS式が長すぎます"))
      #f)))

(define (freeplay-output-result params winner-num logs)
  (if (cgi-get-parameter "textonly" params)
    (list
      (cgi-header :content-type "text/plain")
      (intersperse
        "\n"
        (map
          (lambda (x)
            (write-to-string x))
          logs)))
    (let (
          (p1name (or (cgi-get-parameter "p1name" params) ""))
          (p2name (or (cgi-get-parameter "p2name" params) ""))
          (p1sexpr (or (cgi-get-parameter "p1sexpr" params) ""))
          (p2sexpr (or (cgi-get-parameter "p2sexpr" params) ""))
          )
      (let (
            (p1name-re (string->regexp
                         (regexp-quote
                           (string-append "『" p1name "』"))))
            (p2name-re (string->regexp
                         (regexp-quote
                           (string-append "『" p2name "』"))))
            )
        (response-html
          (list
            ;; TODO: 「戻る」リンク等
            (html:pre
              (intersperse
                "\n"
                (map
                  (lambda (x)
                    ;; 人間が読みやすい形式に変換する
                    (let* ((cmd (cadr x))
                           (proc (hash-table-get *table:cmd->display-proc*
                                                 cmd
                                                 #f))
                           )
                      (if (not proc)
                        (html-escape-string
                          (write-to-string x))
                        (let1 str (apply proc (car x) (cddr x))
                          (cond
                            ((p1name-re str)
                             (html:span :style "color: blue" str))
                            ((p2name-re str)
                             (html:span :style "color: red" str))
                            (else
                              (html:span :style "color: black" str)))))))
                  logs)))))))))


(define (cgi:freeplay-result params)
  ;; 既にparamsの簡単なチェックは完了している
  (receive (winner-num logs) (sbattle.scm:battle params)
    ;; 結果を出力する
    (freeplay-output-result params winner-num logs)))


(define (cgi:freeplay-submit params)
  (let* (
         (p1name (let1 n (or (cgi-get-parameter "p1name" params) "")
                   (if (equal? "" n) (get-name) (or n ""))))
         (p2name (let1 n (or (cgi-get-parameter "p2name" params) "")
                   (if (equal? "" n) (get-name) (or n ""))))
         (new-params (map
                       (lambda (key+vals)
                         (cond
                           ((equal? (car key+vals) "p1name")
                            (list "p1name" p1name))
                           ((equal? (car key+vals) "p2name")
                            (list "p2name" p2name))
                           (else key+vals)))
                       params))
         (errors (check-freeplay-parameter new-params))
         (confirm (cgi-get-parameter "confirm" new-params))
         )
    (if errors
      (cgi:freeplay new-params errors)
      (guard (e
               ((sbattle-timeout? e)
                (cgi:report new-params (ref e 'message)))
               ((sbattle-abnormal-exit? e)
                (cgi:report new-params (ref e 'message)))
               ((<process-abnormal-exit> e)
                (cgi:report new-params "プロセスが異常終了しました"))
               (else
                 ;; その他のエラー
                 (cgi:freeplay new-params (list (ref e 'message)) #f)))
        (if confirm
          (cgi:freeplay-result new-params) ; 実際の勝負へ移行
          (begin
            ;; sbattle.scm:checkによるチェックだけ行って戻る
            (sbattle.scm:check new-params)
            (cgi:freeplay new-params '() #t)))))))


;; この手続きは、
;; check?が#tならsbattle.scmをcheck付きで呼ぶ。返り値は意味を持たない。
;; check?が#fなら、実際に戦闘シミュレートを行い、勝者の数値とlogsを多値で返す。
;; 何らかの問題が発生した時は、エラー例外を使って通知される
(define (%sbattle.scm check? params)
  (let (
        (p1name (cgi-get-parameter "p1name" params))
        (p2name (cgi-get-parameter "p2name" params))
        (p1sexpr (cgi-get-parameter "p1sexpr" params))
        (p2sexpr (cgi-get-parameter "p2sexpr" params))
        (p1hp 100)
        (p2hp 100)
        (args (if check? '("check") '()))
        )
    (let1 p (run-process `(,*gosh-path* ,*sbattle-path* ,@args)
                         :input :pipe
                         :output :pipe
                         ;:error :pipe
                         :wait #f
                         :fork #t
                         )
      (unwind-protect
        (let ((source-port (process-input p))
              (result-port (process-output p)))
          ;; 元データを送信する
          (write (list :p1name p1name
                       :p2name p2name
                       :p1sexpr p1sexpr
                       :p2sexpr p2sexpr
                       :p1hp p1hp
                       :p2hp p2hp)
                 source-port)
          (flush source-port)
          ;; 結果を取得する
          ;; 結果は、以下のパターンが有り得るので、それぞれの対策が必要になる
          ;; - result-portに`(ok)が返る
          ;; - result-portに`(done ,winner ,logs)が返る
          ;; - result-portに`(error ,errors)が返る
          ;; - 終了ステータス0以外で終了(内部エラー)※
          ;; - 規定時間内に終了しなかった(タイムアウト)※
          ;; - result-portに不正なS式が返る(バグ)
          ;; ※がついたものについては、特殊な処理を行う必要があるが、
          ;;   (レポート画面を出す)
          ;;   どうすればよい？
          ;;
          ;; このrに、結果を入れる。以下のどれかになる筈。
          ;; - rが文字列なら、doneまたはerror
          (let1 r (call-with-output-string
                    (lambda (buf)
                      (let retry ((times 20))
                        (if (<= times 0)
                          #t
                          (begin
                            (when (byte-ready? result-port)
                              (copy-port result-port buf :unit 'byte))
                            (process-wait p :nohang #t)
                            (when (process-alive? p)
                              (sys-sleep 1)
                              (retry (- times 1))))))
                      ;; プロセスが終了した、またはretry最大まで達した
                      (process-wait p :nohang #t)
                      (when (process-alive? p)
                        ;; タイムアウト
                        (error <sbattle-timeout> "処理がタイムアウトしました"))
                      ;; race condition対策に、最後にもう一度copyを試みる
                      (when (byte-ready? result-port)
                        (copy-port result-port buf :unit 'byte))
                      #t))
            (unless (zero? (process-exit-status p))
              ;; 内部エラー
              (error <sbattle-abnormal-exit>
                     :status (process-exit-status p)
                     "処理中に予期せぬエラーが発生しました"))
            (let1 res (read-from-string r)
              (when (eof-object? res)
                (error <sbattle-abnormal-exit>
                       :status (process-exit-status p)
                       "処理中に予期せぬエラーが発生しました"))
              (case (car res)
                ((ok) #t)
                ((done) (apply values (cdr res)))
                ((error) (error (apply string-append (cadr res))))
                (else
                  (error "assertion"))))))
        (begin
          (guard (e (else e))
            (process-kill p)) ; 強制終了(ファイル操作等は無い筈なので-9で良い)
          (guard (e (else e))
            (process-wait p :nohang #f)))))))



;; この手続きは、戦闘シミュレータを実行し、何か問題があればエラー例外を投げる
;; 正常に完了したなら、勝者の数値とlogsを多値で返す
(define (sbattle.scm:battle params)
  (%sbattle.scm #f params))

;; この手続きは、paramsをチェックし、問題があればエラー例外を投げる
;; 返り値は意味を持たない
(define (sbattle.scm:check params)
  (%sbattle.scm #t params))

(define (cgi:freeplay params . opt-errors+ok)
  (let ((errors (guard (e (else '()))
                  (car opt-errors+ok)))
        (check-ok (guard (e (else #f))
                    (cadr opt-errors+ok)))
        ;(mode (cgi-get-parameter "mode" params))
        (textonly (cgi-get-parameter "textonly" params))
        (p1name (let1 n (cgi-get-parameter "p1name" params)
                  (if (equal? "" n)
                    (get-name)
                    (or n ""))))
        (p1sexpr (or (cgi-get-parameter "p1sexpr" params) ""))
        (p2name (let1 n (cgi-get-parameter "p2name" params)
                  (if (equal? "" n)
                    (get-name)
                    (or n ""))))
        (p2sexpr (or (cgi-get-parameter "p2sexpr" params) ""))
        )
    (response-html
      (list
        (html:h1 "対戦設定")
        (if (null? errors)
          '()
          (html:div
            :style "border:solid 1px black"
            (html:strong :style "color: red" "error")
            (html:ul
              (map
                (lambda (e)
                  (html:li
                    (regexp-replace-all
                      #/\n/
                      (html-escape-string e)
                      (tree->string (html:br)))))
                errors))))
        (if (not check-ok)
          '()
          (html:div
            (html:p
              (html:em :style "color: blue"
                       "二つのS式がvalidであり、"
                       "対戦可能な事が確認されました。"
                       "下の「戦闘開始」ボタンを押してください。"
                       ))))
        (html:form
          :action (cgi-get-metavariable "SCRIPT_NAME")
          :target "_self"
          :method "post"
          (html:input :type "hidden" :name "c" :value "freeplay-submit")
          (html:table
            (html:tr
              (html:td
                (html:div
                  (html:div "キャラクタ1の名前を入力して下さい"
                            (html:br)
                            "(未入力の場合はランダムに生成されます)")
                  (html:div
                    "名前: " (html:input :type "text" :name "p1name" :value p1name)))
                (html:div
                  (html:div "キャラクタ1のS式を入力して下さい")
                  (html:textarea :name "p1sexpr" :rows "24" :cols "64" p1sexpr)))
              (html:td
                (html:div
                  (html:div "キャラクタ2の名前を入力して下さい"
                            (html:br)
                            "(未入力の場合はランダムに生成されます)")
                  (html:div
                    "名前: " (html:input :type "text" :name "p2name" :value p2name)))
                (html:div
                  (html:div "キャラクタ2のS式を入力して下さい")
                  (html:textarea :name "p2sexpr" :rows "24" :cols "64" p2sexpr)))
              ))
          (html:div
            (html:div
              (html:input :type "checkbox"
                          :name "textonly"
                          :value "#t"
                          :checked textonly)
              "戦闘結果を内部形式で表示する")
            )
          (html:div
            (html:input :type "submit" :value "S式をチェックする")
            "(数秒ぐらいかかります)"
            (html:br)
            (html:input :type "submit" :name "confirm" :value "戦闘開始")
            "(通常は数秒、最長で10秒くらいかかります)"
            )
          )
        (html:hr)
        (html:div
          (html:h2
            (html:a :name "history" "S式バトラーの更新履歴"))
          (html:p "(些細な修正はコッソリ行われる場合があります)")
          *html:history*)
        (html:hr)
        (html:div
          (html:h2
            (html:a :name "explain" "S式入力の説明"))
          (html:p "このページの一番下のリンクから、サンプルのS式を見た方が"
                  "早いかも知れません。")
          (html:ul
            (html:li (html:code "(define (battle-main) ...)")
                     (html:br)
                     "のように、battle-main手続きを定義してください。"
                     (html:br)
                     "尚、battle-main手続きが終了するとペナルティを与えられた"
                     "上で、battle-main手続きが再起動されます。"
                     "必要なら、"
                     (html:br)
                     (html:code "(define (battle-main) ... (battle-main))")
                     (html:br)
                     "のように、末尾再帰してください。"
                     )
            (html:li "マクロ生成とファイルロード系を除いたR5RS手続きと、"
                     "一部のGauche拡張手続きが使用可能です。")
            (html:li
              "戦闘の為に、以下の束縛が提供されます。"
              *html:explain*)
            ))
        ;(html:hr)
        ;(html:div
        ;  (html:h2
        ;    (html:a :name "rule" "詳細ルール"))
        ;  (html:p "読まなくても何とかなります。")
        ;  *html:rule*
        ;  )
        (html:hr)
        (html:ul
          (html:li
            (html:a :href (symbol->path 'freeplay-sample)
                    :target "_blank"
                    "サンプルS式を見る"))
          (html:li
            (html:a :href (symbol->path 'menu)
                    "ゲームを終了してメニューに戻る"))
          )
        ))))



;;; ----

(define (cgi:report-done params)
  (response-html
    (html:div
      :class "wireframe01"
      (html:p "エラーが発生したS式をログファイルに記録しました。")
      (html:p "あとで対応します。")
      (html:hr)
      (html:ul
        (html:li
          (html:a
            :href (symbol->path 'menu)
            "トップに戻る"))))))

(define (cgi:report-submit params)
  (define (logging msg)
    (with-output-to-file
      *errorlog-path*
      (lambda ()
        (write msg)
        (newline))
      :if-exists :append))

  (let ((p1sexpr (or (cgi-get-parameter "p1sexpr" params) ""))
        (p2sexpr (or (cgi-get-parameter "p2sexpr" params) ""))
        )
    (logging '----)
    (logging (sys-ctime (sys-time)))
    (logging p1sexpr)
    (logging p2sexpr)
    (location
      (path->url
        (symbol->path 'report-done)))))

(define (cgi:report params . opt-message)
  (let ((message (get-optional opt-message "(？？？)"))
        (p1sexpr (or (cgi-get-parameter "p1sexpr" params) ""))
        (p2sexpr (or (cgi-get-parameter "p2sexpr" params) ""))
        )
    (response-html
      (html:div
        :class "wireframe01"
        (html:h1 "致命的エラー")
        (html:p "以下の致命的エラーが発生しました。")
        (html:div (html:strong (html-escape-string message)))
        (html:p "このエラーの原因はおそらく、"
                "eval/svの未実装部分/不具合だと思われます。"
                (html:br)
                "(または、たまたまサーバが異常に高負荷だったか)"
                (html:br)
                "eval/svの完成の為に、"
                "このS式の情報を記録してもよろしいでしょうか？"
                (html:br)
                "よろしければ、下のボタンを押すと、今回のS式が"
                "サーバのログファイルに記録されます。"
                )
        (html:form
          :action (cgi-get-metavariable "SCRIPT_NAME")
          :target "_self"
          :method "post"
          (html:input :type "hidden" :name "c" :value "report-submit")
          (html:input :type "hidden" :name "p1sexpr" :value p1sexpr)
          (html:input :type "hidden" :name "p2sexpr" :value p2sexpr)
          (html:input :type "submit" :value "記録する"))
        (html:div
          (html:p "プレイヤー1のS式")
          (html:pre
            (html-escape-string p1sexpr)))
        (html:div
          (html:p "プレイヤー2のS式")
          (html:pre
            (html-escape-string p2sexpr)))
        ))))

(define (cgi:freeplay-sample params)
  (response-html
    (list
      (html:h1 "サンプルキャラクター")
      (html:p "コピペしてそのまま使えます。")
      *html:freeplay-sample*
      )))

(define (cgi:menu params)
  (response-html
    (list
      (html:h1 "S式バトラー")
      (html:ul
        (html:li
          (html:a :href (symbol->path 'explain) "はじめに"))
        (html:li
          (html:a ;:href (symbol->path 'play)
                  "一人プレイ(未完成)"))
        (html:li
          (html:a :href (symbol->path 'freeplay) "フリー対戦"))
        ))))

;;; ----

;;; cgiパラメータは？
(define (main argv)
  (cgi-main
    (lambda (params)
      (let1 c (x->string (or (cgi-get-parameter "c" params) "menu")) ; default
        (let1 cgi-entry-symbol (string->symbol (string-append "cgi:" c))
          (let1 proc (guard (e (else cgi:menu)) ; fallback
                       (eval cgi-entry-symbol (current-module)))
            (proc params)))))
    ;:on-error report-error
    ))



