;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ToDo: 雑多になってきたので、モジュール名をtir03.cgi.miscにする？

;;; ToDo: cgi-mainの内側か外側か判定可能な何かが無いかどうか確認し、
;;;       可能なら、locationは内側でも外側でも機能するように直す事。
;;; ToDo: location時の#の扱い
;;; - location手続きは対応済み
;;; - webサーバにhoge.cgi#abc等のアクセスが来た際に、
;;;   各種処理を行う前に#以降を除去しなくてはならない
;;;   (本来はwebサーバが行うべき？？？)
;;; - どうしても#付きurlにリダイレクトしたい時の為に、
;;;   meta http-equiv="Refresh"
;;;   でリダイレクトさせるhtml-treeを返す手続きを用意する。
;;; -- 更に、副作用を伴う動作後に上記リダイレクトを行わせたい時の為に、
;;;    locationでリダイレクト→meta http-equiv="Refresh"でリダイレクト
;;;    という順に動作する手続きも用意する。
;;; - 今のところは必要な場面は無いので、必要な場面が出たら作る。

;;; ToDo: test caseを用意
;;; ToDo: text->inline-html text->block-html関数を追加する事。
;;; ToDo: http-tree-makeを書きましょう。

(define-module tir03.cgi
  (use gauche.charconv)
  (use gauche.parameter)
  (use srfi-1)
  (use srfi-2)
  (use rfc.uri)
  (use text.html-lite)
  (use text.tree)
  (use util.list)
  (use www.cgi)

  (export
    hes
    html-tree-make
    http-tree-make
    cgi-tree-make
    get-html-tree-keyword-symbols
    get-http-tree-keyword-symbols
    get-cgi-tree-keyword-symbols
    cgi-metavariables->html
    cgi-params->html
    make-view-cgi-metavariables-thunk
    append-params-to-url
    completion-uri
    path->url
    location
    self-url
    self-url/path-info
    self-path
    self-path/path-info
    make-form
    cgi-on-error/stack-trace
    cgi-main/jp
    cgi-main/path
    cgi-main/path/jp
    cgi-main/jp/path
    html:form/jp
    get-path-info-keylist
    with-reverse-proxy
    ))
(select-module tir03.cgi)


(define (hes . params)
  (if (= 1 (length params))
    (html-escape-string (car params))
    (map html-escape-string params)))


(define-syntax when/null
  (syntax-rules ()
    ((_ pred body ...) (if pred
                         (begin body ...)
                         '()))))

(define-macro (let-keywords** keywords tree-keyword-symbols body . bodies)
  ;; このマクロはものすごく微妙なので、あとでもっとマシな方法を考えて直す事
  `(let-keywords* keywords ,(map
                              (lambda (x)
                                (list x #f))
                              (eval tree-keyword-symbols (current-module)))
     ,body . ,bodies))

(define *html-tree-keyword-symbols*
  '(encoding
    base-url
    css-url
    css-body
    js-url
    js-body
    js
    robots
    title
    title-format
    title-format-args
    body
    body-header
    body-footer
    frame-body
    ))

(define (html-tree-make . keywords)
  (let-keywords** keywords *html-tree-keyword-symbols*
    (list
      ;; xml header
      (if encoding
        #`"<?xml version=\"1.0\" encoding=\",|encoding|\"?>\n"
        "<?xml version=\"1.0\"?>\n")
      ;; html doctype
      (html-doctype
        :type (if frame-body
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
                  (hes
                    (apply format #f title-format title-format-args)))))
            (when/null title
              (html:title (hes title))))
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
                         :src js-url
                         ""))
          (when/null js-body
            (html:script :type "text/javascript"
                         "<!--\n"
                         js-body
                         "\n-->"
                         ))
          )
        (when/null body
          (html:body
            (when/null body-header body-header)
            body
            (when/null body-footer body-footer)
            ))
        (when/null frame-body
          frame-body)
        ))))


(define *http-tree-keyword-symbols*
  ;; まだ
  '(
    ))
(define (http-tree-make . keywords)
  ;; 未実装
  (error "not implemented"))


(define *cgi-tree-keyword-symbols*
  '(encoding
    content-type
    location
    http-header
    http-body
    body
    frame-body
    ))
(define (cgi-tree-make . keywords)
  (let-keywords** keywords *cgi-tree-keyword-symbols*
    (if location
      (apply
        cgi-header
        :pragma "no-cache"
        :cache-control "no-cache"
        :location location
        (or http-header '()))
      (let (
            (content-type-is-text (and
                                    content-type
                                    (#/^text\// content-type)))
            (content-type-has-charset (and
                                        content-type
                                        (string-scan content-type #\;)))
            (true-content-type (or content-type "text/html"))
            )
        (list
          (apply
            cgi-header
            :content-type (if content-type-has-charset
                            true-content-type
                            (if (not encoding)
                              true-content-type
                              (string-append
                                true-content-type
                                "; charset="
                                encoding)))
            (or http-header '()))
          (cond
            (http-body http-body)
            ((or body frame-body) (apply html-tree-make keywords))
            (else
              (error
                (string-append
                  "cgi-tree-make must be needed "
                  ":location or :body or :frame-body or :http-body")))))))))


(define (uniq src-list)
  ;; note: 今のところ、eq?でのみ判定を行う仕様とする
  (let loop ((left src-list)
             (result '()))
    (if (null? left)
      result
      (loop
        (cdr left)
        (if (memq (car left) result)
          result
          (cons (car left) result))))))
(define-syntax define-get-*-tree-keyword-symbols
  (syntax-rules ()
    ((_ proc-name target-list)
     (define proc-name
       (let1 promise (delay (uniq target-list))
         (lambda ()
           (force promise)))))))

(define (get-html-tree-keyword-symbols)
  *html-tree-keyword-symbols*)
(define-get-*-tree-keyword-symbols get-http-tree-keyword-symbols
  (append
    *html-tree-keyword-symbols*
    *http-tree-keyword-symbols*))
(define-get-*-tree-keyword-symbols get-cgi-tree-keyword-symbols
  (append
    *html-tree-keyword-symbols*
    *cgi-tree-keyword-symbols*))


(define (cgi-metavariables->html . opt-mv)
  ;; ToDo: 環境変数からもCGIメタ変数を取得する事。
  ;; ToDo: tir04にバージョンを上げる際にオプショナル引数は廃止する
  (let1 mv (get-optional opt-mv (cgi-metavariables))
    (html:dl
      (map (lambda (x)
             (list
               (html:dt (hes (car x)))
               (html:dd (hes (cadr x)))))
           (sort
             (or mv '())
             (lambda (x y)
               (string<? (car x) (car y))))))))

(define (cgi-params->html params)
  ;; まず、paramsからエンコーディングを推測する
  ;; formにバイナリデータが入っている事は、ここでは考えない。
  (let1 ces (ces-guess-from-string (tree->string params) "*JP")
    (html:dl
      (map
        (lambda (x)
          (list
            (html:dt (hes (ces-convert (car x) ces)))
            (map
              (lambda (y)
                (html:dd (hes (ces-convert (x->string y) ces))))
              (cdr x))))
        params))))

;; キーワード引数を与えて、CGIメタ変数をhtmlとして表示するだけの
;; CGIスクリプトthunkを生成する高階関数。
;; 通常は:css-url :robots :title :back-urlを与えれば充分。
;; （※:titleに日本語を使う場合は、:encodingも必要。
;;     但し、現在はまだform入力の自動日本語コード変換に対応していないので、
;;     :encodingは使わない方が良い）
;; 環境変数は表示しない。
;; ToDo : form-parameterの自動日本語コード変換機能
(define (make-view-cgi-metavariables-thunk . keywords)
  (let-keywords* keywords ((encoding #f)
                           (on-error #f)
                           (content-type (if encoding
                                           #`"text/html; charset=,|encoding|"
                                           "text/html"))
                           (back-url #f)
                           )
    (lambda ()
      (cgi-main
        (lambda (params)
          (define back-url-html
            (or
              (and
                back-url
                (html:ul
                  (html:li
                    (html:a
                      :href back-url
                      "back"))))
              '()))
          (define back-url-html-separator
            (if (null? back-url-html)
              '()
              (html:hr)))
          (define (make-html-body)
            (list
              back-url-html
              back-url-html-separator
              (html:h1
                :class "inline_centering"
                (hes (get-keyword :title keywords "cgi-metavariables"))
                )
              (cgi-metavariables->html (cgi-metavariables))
              back-url-html-separator
              back-url-html
              ))
          ;; 結果をtext.treeとして返す
          (apply
            cgi-tree-make
            :content-type content-type
            :body (make-html-body)
            keywords))
        :on-error on-error))))




(define (append-params-to-url url params)
  (if (null? params)
    url
    (receive (url-without-fragment fragment) (let1 m (#/(\#.*)/ url)
                                               (if m
                                                 (values (m 'before) (m 1))
                                                 (values url "")))
      (call-with-output-string
        (lambda (p)
          (letrec ((delimitee (if (#/\?/ url-without-fragment)
                                (lambda () "&")
                                (lambda ()
                                  (set! delimitee (lambda () "&"))
                                  "?"))))
            (display url-without-fragment p)
            (let loop ((left-params params))
              (if (null? left-params)
                (display fragment p)
                (let ((key-encoded (uri-encode-string (caar left-params)))
                      (vals (cdar left-params))
                      (next-left (cdr left-params))
                      )
                  (if (pair? vals)
                    (for-each
                      (lambda (val)
                        (display (delimitee) p) ; "?" or "&"
                        (display key-encoded p)
                        (display "=" p)
                        (display (uri-encode-string (if (string? val) val "")) p))
                      vals)
                    (begin
                      (display (delimitee) p)
                      (display key-encoded p)))
                  (loop next-left))))))))))


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
          :flagment uri-fragment)))))



(define (path->url path)
  (if (#/^\// path)
    (completion-uri
      path
      (cgi-get-metavariable "SERVER_NAME")
      (cgi-get-metavariable "SERVER_PORT")
      (cgi-get-metavariable "HTTPS"))
    path))


(define (location url)
  (define (chop-url-fragment url)
    (or
      (and-let* ((m (#/\#/ url)))
        (m 'before))
      url))
  (cgi-header
    :pragma "no-cache"
    :cache-control "no-cache"
    :location (chop-url-fragment (path->url url))))


(define (self-url)
  (path->url (self-path)))

(define (self-url/path-info)
  (path->url (self-path/path-info)))

(define (self-path)
  (or (cgi-get-metavariable "SCRIPT_NAME") "/"))

(define (self-path/path-info)
  ;; note: PATH_INFOは既にデコードされてしまっているので使わない事
  (let* ((r (or (cgi-get-metavariable "REQUEST_URI") "/"))
         (m (#/\?/ r))
         )
    (if m
      (m 'before)
      r)))




(define (make-form url hidden-params html-tree . keywords)
  (apply
    html:form
    :action url
    (append
      keywords
      (list
        :method "post"
        :target "_self")
      (map
        (lambda (key+vals)
          (let1 key (car key+vals)
            (map
              (lambda (val)
                (html:input
                  :type "hidden"
                  :name key
                  :value val))
              (cdr key+vals))))
        hidden-params)
      html-tree)))


(define (cgi-on-error/stack-trace e)
  `(,(cgi-header)
    ,(html-doctype)
    ,(html:html
      (html:head (html:title "Error"))
      (html:body (html:h1 "Error")
                 (html:pre (html-escape-string
                             (call-with-output-string
                               (cut
                                 with-error-to-port
                                 <>
                                 (cut report-error e)))))))))


;; ToDo: ファイルアップロードの際に問題が発生する可能性があるので、
;;       更に細かくパターンを分ける必要がある。
(define (cgi-main/jp proc . keywords)
  (define (reconv-params params)
    (let* ((guess-string (tree->string params))
           (ces (or
                  (ces-guess-from-string guess-string "*JP")
                  (cgi-output-character-encoding)))) ; fallback
      (define (conv str)
        (ces-convert str ces))
      (map
        (lambda (key+vals)
          (cons
            (conv (car key+vals))
            (map
              (lambda (val)
                (if (string? val)
                  (conv val)
                  val))
              (cdr key+vals))))
        params)))
  (apply
    cgi-main
    (lambda (params)
      (let1 new-params (reconv-params params)
        (proc new-params)))
    keywords))


(define (c/p proc-cgi-main target-proc keywords)
  (let ((path-info-keylist (get-path-info-keylist))
        (request-method (cgi-get-metavariable "REQUEST_METHOD")))
    ;; path-info-keylistが#fなら、一旦リダイレクトを行う。
    ;; 但し、メタ変数REQUEST_METHODがPOSTなら、リダイレクトは行わない。
    ;; (通常通り、procを実行する)
    (apply
      proc-cgi-main
      (lambda (params)
        (if (or
              path-info-keylist
              (equal? request-method "POST"))
          (target-proc params path-info-keylist)
          (location
            (append-params-to-url (string-append (self-url) "/") params))))
      keywords)))

(define (cgi-main/path proc . keywords)
  (c/p cgi-main proc keywords))
(define (cgi-main/path/jp proc . keywords)
  (c/p cgi-main/jp proc keywords))
(define cgi-main/jp/path cgi-main/path/jp)


(define (html:form/jp . args)
  (apply
    html:form
    (append args
            (html:input :type "hidden"
                        :name "_ces_identifier"
                        :value "日本語"))))


(define (get-path-info-keylist)
  ;; それぞれの場合で、以下のような値を返す。%xxのデコードは行わない。
  ;; (%xxのデコードを行わないのは、セキュリティ上の安全の為)
  ;; - /path/to/hoge.cgi          => #f
  ;; - /path/to/hoge.cgi/         => '()
  ;; - /path/to/hoge.cgi/?abc=def => '()
  ;; - /path/to/hoge.cgi/abc/def  => '("abc" "def")
  ;; - /path/to/hoge.cgi/abc/def/ => '("abc" "def")
  ;; - /path/to/hoge.cgi/%20      => '("%20")
  ;; - /path/to/hoge.cgi/a///b    => '("a" "" "" "b")
  ;; WARN: apache2系の古いバージョンでは、PATH_INFO部分にスラッシュが複数
  ;;       連続して存在する場合に、SCRIPT_NAMEが壊れるというバグがあるので、
  ;;       そういうバージョンではスラッシュが複数連続するようなアクセスが
  ;;       来ないようにしなくてはならない。
  ;;       (基本的に、セキュリティ的には問題は無いと思うので、
  ;;        特に対策コードは入れたりはしない予定。)
  ;; WARN: 今のところ、「REQUEST_URIは、常にSCRIPT_NAMEをprefixとして含む」
  ;;       という事を前提としている。
  ;;       「~」が「%7e」にされたり、大文字小文字を同一視するようなhttpdでは
  ;;       問題になるので注意する事。
  ;; ToDo: %7eや%7E等があっても正常に動作するようにしなくてはならない
  ;; ToDo: REQUEST_URIの末尾に?が無くて#があった場合の挙動対応
  (define (path-info-split path)
    (and-let* ((m (#/^\/(.*?)\/?$/ path))
               (plain-path (m 1))) ; 先頭と末尾の/を除去
      (if (string=? plain-path "")
        '()
        (string-split plain-path #\/))))

  (and-let* ((script-name (cgi-get-metavariable "SCRIPT_NAME"))
             (request-uri (cgi-get-metavariable "REQUEST_URI"))
             (re (string->regexp
                   (string-append "^" (regexp-quote script-name))))
             (m (re request-uri))
             (path-info+query (m 'after))
             (result (or
                       (and-let* ((m (#/\?/ path-info+query)))
                         (m 'before))
                       path-info+query)))
    (if (string=? result "")
      #f
      (path-info-split result))))


(define (with-reverse-proxy server-name server-port thunk)
  (parameterize ((cgi-metavariables
                   (list*
                     `("SERVER_NAME" ,(x->string server-name))
                     `("SERVER_PORT" ,(x->string server-port))
                     (remove
                       (lambda (key+val)
                         (or
                           (string=? (car key+val) "SERVER_NAME")
                           (string=? (car key+val) "SERVER_PORT")
                           ))
                       (or (cgi-metavariables) '())))))
    (thunk)))


(provide "tir03/cgi")

