;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ToDo: text->inline-htmlのurl->href:keywordsは廃止する。
;;;       代わりに、url->procを追加し、urlのような文字列を見付けたら
;;;       指定したprocが呼ばれるようにする。
;;; ToDo: test caseを用意
;;; ToDo: text->block-htmlの仕様を考えて、ちゃんと追加する
;;; 以下の文法に対応させたい。
;;; - text->inline-html分
;;; - 改行二つ -> html:p
;;; 以下の変換をサポートすべきかは微妙。
;;; - 文頭が「-」「+」何個かで始まっている -> html:ul, html:ol
;;; -- 単に数値のマイナス値やプラス値を書いていただけの時に誤認識しそう。
;;; - 文頭が空白で始まっている -> html:pre
;;; -- これも誤認識しそう。
;;; キーワード引数かオプショナル引数で変換法則を制御できるようにする？
;;; 本格的にやる場合は、wiki構文の再実装っぽくなってしまう気がする。
;;; どうすべき？

;;; ToDo: http-tree-makeを書きましょう。

(define-module tir04.cgi.util
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
    html-escape-tree
    het
    html-tree-make
    http-tree-make
    cgi-tree-make
    get-html-tree-keyword-symbols
    get-http-tree-keyword-symbols
    get-cgi-tree-keyword-symbols
    cgi-metavariables->html
    cgi-params->html
    append-params-to-url
    completion-uri
    path->url
    location
    location/fragment
    self-url
    self-url/path-info
    self-url/slash
    self-path
    self-path/path-info
    self-path/slash
    make-form
    cgi-on-error/stack-trace
    ces-convert-cgi-params
    get-path-info-keylist
    with-path-info-keylist
    with-reverse-proxy
    text->inline-html
    text->block-html
    ))
(select-module tir04.cgi.util)


(define (html-escape-tree . params)
  (cond
    ((null? params) '()) ; 引数無し
    ((null? (cdr params))
     (html-escape-string (car params))) ; 引数一つ(返り値も一つ)
    (else ; 引数が複数(返り値はlist)
      (map
        (lambda (x)
          (if (list? x)
            (apply html-escape-tree x)
            (html-escape-string x)))
        params))))
(define het html-escape-tree)


(define-syntax when/null
  (syntax-rules ()
    ((_ pred body ...) (if pred
                         (begin body ...)
                         '()))))

(define-macro (let-keywords** keywords tree-keyword-symbols body . bodies)
  ;; このマクロはものすごく微妙なので、あとでもっとマシな方法を考えて直す事
  `(let-keywords* keywords ,(append
                              (map
                                (lambda (x)
                                  (list x #f))
                                (eval tree-keyword-symbols (current-module)))
                              #f)
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
    html-header
    body
    body-attr
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
                  (het
                    (apply format #f title-format title-format-args)))))
            (when/null title
              (html:title (het title))))
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
          (when/null html-header html-header)
          )
        (when/null body
          (apply
            html:body
            (append
              (when/null body-attr body-attr)
              (list
                (when/null body-header body-header)
                body
                (when/null body-footer body-footer)
                ))))
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


(define (cgi-metavariables->html)
  ;; ToDo: 環境変数からもCGIメタ変数を取得する事。
  ;;       (environを参照する手段が提供されたら実装する)
  ;;       実装されそうにない場合は、自分でGaucheに対するパッチを
  ;;       書いて送る事。
  ;;       (sys-environ)とかで参照できるように。
  (define (sys-environ) '()) ; 今は無いので、ダミーを定義しておく

  (let* ((env-mv (filter
                   (lambda (x)
                     ;; ToDo: 通常の環境変数と、CGIメタ変数を
                     ;;       ふるい分ける必要がある。
                     ;;       あとで実装する。
                     #f)
                   (sys-environ)))
         (mv (delete-duplicates
               (append
                 (or (cgi-metavariables) '())
                 env-mv)
               (lambda (x y)
                 (equal? (car x) (car y))))))
    (html:dl
      (map (lambda (x)
             (list
               (html:dt (het (car x)))
               (html:dd (het (cadr x)))))
           ;; mapにかける前に、keyのアルファベット順でsortする
           (sort
             mv
             (lambda (x y)
               (string<? (car x) (car y))))))))

(define (cgi-params->html params)
  ;; note: paramsが日本語を含む可能性がある場合は、
  ;;       事前にces-convert-cgi-paramsにかけておく事。
  (html:dl
    (map
      (lambda (x)
        (list
          (html:dt (het (write-to-string (car x))))
          (map
            (lambda (y)
              (html:dd (het (write-to-string y))))
            (cdr x))))
      params)))



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
          :fragment uri-fragment)))))



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


(define (location/fragment url)
  (if (not (#/\#/ url))
    (location url)
    (list
      (cgi-header
        :pragma "no-cache"
        :cache-control "no-cache"
        :content-type "text/html")
      (html-tree-make
        :robots "NOINDEX,NOFOLLOW"
        :html-header (html:meta :http-equiv "Refresh"
                                :content (string-append "0;URL=" url))
        :body (html:div (html:a :href url
                                (het url)))))))


(define (self-url)
  (path->url (self-path)))

(define (self-url/path-info)
  (path->url (self-path/path-info)))

(define (self-url/slash)
  (string-append (self-url) "/"))

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

(define (self-path/slash)
  (string-append (self-path) "/"))



(define (make-form url hidden-params html-tree . keywords)
  (apply
    html:form
    :action url
    (append
      (when/null (not (get-keyword :method keywords #f))
        '(:method "post"))
      (when/null (not (get-keyword :target keywords #f))
        '(:target "_self"))
      keywords
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


(define (ces-convert-cgi-params params . keywords)
  (let-keywords* keywords ((from-code #f)
                           (to-code (gauche-character-encoding))
                           (excluded-key-list '())
                           )
    (receive (target-params excluded-params) (partition
                                               (lambda (x)
                                                 (not
                                                   (find
                                                     (lambda (y)
                                                       (equal? y (car x)))
                                                     excluded-key-list)))
                                               params)
      (let1 params-encoding (or
                              from-code
                              (ces-guess-from-string
                                (tree->string target-params) "*JP")
                              "none")
        (define (conv str)
          (ces-convert str params-encoding to-code))
        (append
          excluded-params
          (map
            (lambda (key+vals)
              (cons
                (conv (car key+vals))
                (map
                  (lambda (val)
                    ;; valは#t, #fの場合もあるので、それを判定する必要がある
                    (if (string? val)
                      (conv val)
                      val))
                  (cdr key+vals))))
            target-params))))))



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
  ;;       そのような場合は常に#fを返すので、
  ;;       もし(get-path-info-keylist)が#fを返した時は、一旦
  ;;       (string-append (self-url) "/")
  ;;       にリダイレクトしてから処理を行うようにすべき。
  ;;       (リダイレクト後なら、REQUEST_URIとSCRIPT_NAMEは一致すると思われる)
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


;; note: この手続きはcgi-main内で呼び出す事。
;; ToDo: リダイレクト後もREQUEST_URIとSCRIPT_NAMEが一致しない場合、
;;       無限リダイレクトループが発生してしまう。
;;       対策を考える事。
(define (with-path-info-keylist proc)
  (let1 path-info-keylist (get-path-info-keylist)
    (if (or
          (equal? "POST" (cgi-get-metavariable "REQUEST_METHOD"))
          path-info-keylist)
      (proc (or path-info-keylist '()))
      ;; 本当なら、locationを行う際には、append-params-to-urlを使って、
      ;; 今回のリクエストで渡されたparamsも反映させつつリダイレクトしたいが、
      ;; ここにはparamsは渡されていないので、それはできないので、
      ;; params無しでリダイレクトを行う。
      (location (string-append (self-url) "/")))))


(define (with-reverse-proxy server-name server-port thunk)
  (parameterize ((cgi-metavariables
                   (delete-duplicates
                     (list*
                       `("SERVER_NAME" ,(x->string server-name))
                       `("SERVER_PORT" ,(x->string server-port))
                       (or (cgi-metavariables) '()))
                     (lambda (x y)
                       (equal? (car x) (car y))))))
    (thunk)))

(define (google-redirector url)
  (string-append
    "http://www.google.com/url?sa=D&q="
    url))

(define *re:url*
  #/https?\:\/\/[\!\#\$\%\&\'\(\)\*\+\,\-\.\/0-9\:\;\=\?\@A-Z\_a-z\~]+/)
(define *re:linefeed*
  (string->regexp "\r\n|\r|\n"))
(define *re:text->inline-html*
  (string->regexp
    (tree->string
      (intersperse
        "|"
        (map
          (lambda (x)
            (list "(?:" x ")"))
          (list
            (regexp->string *re:linefeed*)
            (regexp->string *re:url*)
            " "
            "\t"
            ))))))
(define (text->inline-html text . keywords)
  ;; 変換すべき構文は、以下の通り。
  ;; - urlが書いてある -> html:a
  ;; - 改行一つ -> html:br
  ;; - #\space -> &nbsp;
  ;; - #\tab -> &nbsp; * n (まだ未実装)
  ;; 残りはhetにかける。
  (let-keywords* keywords ((url->href #t)
                           (url->href:filter google-redirector)
                           (url->href:keywords '()) ; targetとかnameとか
                           (linefeed->br #t)
                           (space->nbsp #t)
                           (tab->nbsp #f) ; ToDo: 未実装
                           )
    (define (convert target)
      (cond
        ((*re:linefeed* target) (if linefeed->br
                                  (html:br)
                                  (het target)))
        ((equal? " " target) (if space->nbsp
                               "&nbsp;"
                               (het target)))
        ((equal? "\t" target) (if tab->nbsp
                                (het target) ; ToDo: 未実装
                                (het target)))
        ((*re:url* target) (if url->href
                             (apply html:a
                                    :href (if url->href:filter
                                            (url->href:filter target)
                                            target)
                                    (append
                                      url->href:keywords
                                      (list (het target))))
                             (het target)))
        (else
          (error "assertion"))))

    (tree->string
      (let recursive ((leftover text))
        (if (string=? "" leftover)
          '()
          (let1 m (*re:text->inline-html* leftover)
            (if (not m)
              (het leftover)
              (list
                (het (m 'before))
                (convert (m))
                (recursive (m 'after))))))))))

(define (text->block-html text . keywords)
  ;; ToDo: あとでちゃんと実装する事
  (html:p
    (apply text->inline-html text keywords)))


(provide "tir04/cgi/util")

