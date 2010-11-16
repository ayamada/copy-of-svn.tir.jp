#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ToDo: ドキュメント書き
;;; ToDo: test caseを用意

(define-module tir.cgi
  (use gauche.parameter)
  (use srfi-2)
  (use rfc.uri)
  (use text.html-lite)
  (use text.tree)
  (use util.list)
  (use www.cgi)

  (export
    hes
    html-tree-make
    make-webpage-main
    cgi-metavariables->html
    make-view-cgi-metavariables-thunk
    make-url-from-form-params
    completion-uri
    location
    self-url
    self-url/path
    make-form
    ))
(select-module tir.cgi)


(define (hes . params)
  (if (= 1 (length params))
    (html-escape-string (car params))
    (map html-escape-string params)))


(define-syntax when/null
  (syntax-rules ()
    ((_ pred body ...) (if pred
                         (begin body ...)
                         '()))))


;; ToDo : frame対応
(define (html-tree-make . keywords)
  (let-keywords* keywords ((encoding #f)
                           (css-url #f)
                           (css-body #f)
                           (js-url #f)
                           (js-body #f)
                           (robots #f)
                           (title #f)
                           (body #f)
                           (body-class #f)
                           (frame #f) ; ToDo : まだ未実装
                           )
    (list
      (if encoding
        #`"<?xml version=\"1.0\" encoding=\",|encoding|\"?>"
        "<?xml version=\"1.0\"?>")
      "\n"
      (html-doctype :type :xhtml-1.0-transitional)
      (html:html
        :lang "ja-JP"
        :xml:lang "ja-JP"
        :xmlns "http://www.w3.org/1999/xhtml"
        (html:head
          (when/null encoding
            (html:meta :http-equiv "Content-Type"
                       :content #`"text/html; charset=,|encoding|"))
          (when/null (or css-url css-body)
            (html:meta :http-equiv "Content-Style-Type"
                       :content "text/css"))
          (when/null (or js-url js-body)
            (html:meta :http-equiv "Content-Script-Type"
                       :content "text/javascript"))
          (when/null robots
            (html:meta :name "ROBOTS"
                       :content robots))
          (when/null title
            (html:title (hes title)))
          (when/null css-url
            (html:link :rel "Stylesheet"
                       :type "text/css"
                       :href css-url))
          ;; ToDo : css-body, js-url, js-body
          )
        (when/null body
          (if body-class
            (html:body :class body-class body)
            (html:body body)))))))


;; cgi-mainの代わりに使えるwebpage-main関数を生成する高階関数。
;; キーワード引数としてencodingとon-errorを指定する事。
(define (make-webpage-main . k)
  (let-keywords* k ((encoding #f)
                    (on-error #f)
                    (default-content-type #f)
                    )
    (let1 default-content-type (or
                                 default-content-type
                                 (if encoding
                                   #`"text/html; charset=,|encoding|"
                                   "text/html"))
      ;; procは、通常のtext.treeの代わりに、以下のようなlistを返す事。
      ;; '(
      ;;   :ignore-encoding #f ; 出力時自動日本語コード変換を行うかどうか
      ;;   :http-header (:cache-control "private" :pragma "no-cache")
      ;;   :http-body #f ; コレが#fでないなら、html出力は行わずに、コレを出力
      ;;   ... ； その他、html-tree-makeが必要とするkeywords
      ;;   )
      (lambda (proc)
        (parameterize ((cgi-output-character-encoding
                         (gauche-character-encoding))) ; デフォルトは変換しない
          (begin0
            (apply
              cgi-main
              (lambda (params)
                ;; ToDo : form-parameterの自動日本語コード変換機能
                ;;        （バイナリファイルアップロード対応含む）
                (let* ((keywords (proc params)) ; procを実行し、結果を受け取る

                       (ignore-encoding
                         (get-keyword :ignore-encoding keywords #f))
                       (http-header
                         (get-keyword :http-header     keywords '()))
                       (http-body
                         (get-keyword :http-body       keywords #f))

                       (content-type
                         (get-keyword :content-type http-header #f))
                       (location
                         (get-keyword :location     http-header #f))
                       )
                  (unless ignore-encoding
                    (cgi-output-character-encoding encoding)) ; encodingに変換
                  ;; 結果をtext.treeとして返す
                  (cons
                    (apply
                      cgi-header
                      (if (or content-type location)
                        http-header
                        (list*
                          :content-type default-content-type
                          http-header)))
                    (cond
                      (location '())
                      (http-body (list http-body))
                      (else (apply
                              html-tree-make
                              keywords))))))
              (if on-error
                `(:on-error ,on-error)
                '()))))))))


(define (cgi-metavariables->html mv)
  (html:dl
    (map (lambda (x)
           (list
             (html:dt (hes (car x)))
             (html:dd (hes (cadr x)))))
         (sort
           (or mv '())
           (lambda (x y)
             (string<? (car x) (car y)))))))


;; キーワード引数を与えて、CGIメタ変数をhtmlとして表示するだけの
;; CGIスクリプトthunkを生成する高階関数。
;; 通常は:css-url :robots :title :back-urlを与えれば充分。
;; （※:titleに日本語を使う場合は、:encodingも必要。
;;     但し、現在はまだform入力の自動日本語コード変換に対応していないので、
;;     :encodingは使わない方が良い）
;; 環境変数は表示しない。
(define (make-view-cgi-metavariables-thunk . keywords)
  (lambda ()
    (let1 webpage-main (apply
                         make-webpage-main
                         (or
                           (and-let* ((encoding
                                        (get-keyword :encoding keywords #f)))
                             (list :encoding encoding))
                           '()))
      (webpage-main
        (lambda (params)
          (define back-url-html
            (or
              (and-let* ((back-url (get-keyword :back-url keywords #f)))
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
              (cgi-metavariables->html
                (cgi-metavariables))
              back-url-html-separator
              back-url-html
              ))

          (list*
            :body (make-html-body)
            keywords
            ))))))


(define (make-url-from-form-params url alist)
  (if (null? alist)
    url
    (tree->string
      (list
        url
        "?"
        (intersperse
          "&"
          (map
            (lambda (elem)
              (let ((key (uri-encode-string (car elem)))
                    (vals (cdr elem)))
                (if (null? vals)
                  key
                  (intersperse
                    "&"
                    (map
                      (lambda (val)
                        (list key "=" (uri-encode-string val)))
                      vals)))))
            alist))))))


(define (completion-uri location server-name server-port https)
  (receive (uri-scheme
            uri-userinfo
            uri-hostname
            uri-port
            uri-path
            uri-query
            uri-fragment)
    (uri-parse location)
    ;; uri-schemeが無い時にだけ補完する
    ;; 但し、server-nameが与えられていない場合は補完できないので、何もしない
    (if (or uri-scheme (not server-name))
      location
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


(define (location url)
  (let1 full-url (if (#/^\// url)
                   (completion-uri
                     url
                     (cgi-get-metavariable "SERVER_NAME")
                     (cgi-get-metavariable "SERVER_PORT")
                     (cgi-get-metavariable "HTTPS"))
                   url)
    (cgi-header :location full-url)))


(define (self-url)
  (cgi-get-metavariable "SCRIPT_NAME"))


(define (self-url/path)
  ;; note: PATH_INFOは既にデコードされているので使わない
  (let* ((r (cgi-get-metavariable "REQUEST_URI"))
         (m (#/\?/ r))
         )
    (if m
      (m 'before)
      r)))


(define (make-form url hidden-alist . htmls)
  (html:form
    :action url
    :method "post"
    :target "_self"
    (map
      (lambda (x)
        (html:input
          :type "hidden"
          :name (car x)
          :value (cdr x)))
      hidden-alist)
    htmls))


(provide "tir/cgi")

