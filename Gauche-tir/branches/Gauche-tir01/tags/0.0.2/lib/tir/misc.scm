#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; note : このモジュールは、一つのモジュールにまとめる前の、
;;;        一時的な状態の関数等を保存するモジュールとする。


(define-module tir.misc
  ;(use srfi-2) ; and-let*

  (export
    hes
    html-tree-make
    make-webpage-main
    cgi-metavariables->html
    make-view-cgi-metavariables-thunk

    number->string-japanese-weekday
    number->string-japanese-wday
    time->japanese-date-string
    ))
(select-module tir.misc)


;;; ----
;;; tir.cgi?


(use srfi-2)
(use gauche.parameter)
(use text.html-lite)
(use text.tree)
(use www.cgi)

(define hes html-escape-string)

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



;;; ----
;;; tir.japanese?


(define (number->string-japanese-count num)
  ;; ToDo : あとで
  num)


(define *japanese-weekday-vector*
  #("日曜日" "月曜日" "火曜日" "水曜日" "木曜日" "金曜日" "土曜日"))
(define *japanese-wday-vector*
  #("日" "月" "火" "水" "木" "金" "土"))

(define (number->string-japanese-weekday num)
  (or
    (vector-ref *japanese-weekday-vector* num #f)
    (error "invalid number")))
(define (number->string-japanese-wday num)
  (or
    (vector-ref *japanese-wday-vector* num #f)
    (error "invalid number")))

(define (time->japanese-date-string time)
  (let1 tm (sys-localtime time)
    (format
      "~a年~a月~a日(~a)"
      (+ 1900 (number->string-japanese-count (ref tm 'year)))
      (+ 1 (number->string-japanese-count (ref tm 'mon)))
      (number->string-japanese-count (ref tm 'mday))
      (number->string-japanese-wday (ref tm 'wday))
      )))


;;; ----


(provide "tir/misc")

