;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme sw=2 ts=2 et:
;;; $Id$

;;; ToDo : vhostのregexp生成部分を作り、動作確認しましょう。
;;; ToDo : apacheのServerAlias実装

;;; ToDo : 可能なら、lazyな評価を行うようにしたい
;;;        （autoloadのように、必要な部分のみ実行するようにしたい）

(define-module tcpcgi.dispatch
  (use gauche.regexp) ; regexp-quote
  (use srfi-1) ; filter-map
  (use srfi-2) ; and-let*
  (use text.tree)
  (use util.list) ; alist->hash-table intersperse

  (export
    <tcpcgi.dispatch.vhost>
    <tcpcgi.dispatch.path>
    <tcpcgi.dispatch.none>
    make-dispatch-vhost
    make-dispatch-path
    make-dispatch-none
    dispatch.vhost->dispatch.path
    dispatch.path->dispatch.none
    dump-dispatch.none
    ))
(select-module tcpcgi.dispatch)




(define-class <tcpcgi.dispatch.vhost> ()
  (
   ;; keyはvhost-name, valueは<tcpcgi.dispatch.path>
   (dispatch-table :accessor dispatch-table-of
                   :init-keyword :dispatch-table
                   :init-value #f)
   ;; 上のテーブルのkeyがあるかどうかを正規表現にしたもの
   (dispatch-regexp :accessor dispatch-regexp-of
                    :init-keyword :dispatch-regexp
                    :init-value #f)
   ))
(define-class <tcpcgi.dispatch.path> ()
  (
   ;; keyはscript-name, valueは<tcpcgi.dispatch.none>
   (dispatch-table :accessor dispatch-table-of
                   :init-keyword :dispatch-table
                   :init-value #f)
   ;; 上のテーブルのkeyがあるかどうかを正規表現にしたもの
   (dispatch-regexp :accessor dispatch-regexp-of
                    :init-keyword :dispatch-regexp
                    :init-value #f)
   ))
(define-class <tcpcgi.dispatch.none> ()
  (
   (cgi-target :accessor cgi-target-of
               :init-keyword :cgi-target
               :init-value #f)
   (nph :accessor nph-of
        :init-keyword :nph
        :init-value #f)
   ))



(define-syntax string-size-sort-maker
  (syntax-rules ()
    ((_ comparison)
     (lambda (target-list)
       (sort
         target-list
         (lambda (x y)
           (comparison (string-size x) (string-size y))))))))

(define string-size-sort
  (string-size-sort-maker <))
(define string-size-reverse-sort
  (string-size-sort-maker >))


;; 1:完全マッチ集合で名前の長い順
;; 2:ワイルドカードマッチ集合で名前の長い順
;; で、vhost-dispatch-regexpを生成する。
;; 但し、fallbackはコレから除外する。
;; 最終的に、以下のような文字列を生成する
;; "((?i:^aaa\\.bb)|(?i:^cc\\.dd)|(?i:\\.ggg\\.hh))(?:\\:\d+)?$"
;; （当然vhostの文字列はエスケープしておく事）
;; これは、aaa.bb, cc.dd, *.ggg.hhにマッチする。
;; 元となるhash-tableのキーは例えば、"aaa.bb" "cc.dd" ".ggg.hh"等。
;; (match-obj 1)でdispatch-hashを引く為の名前が得られる。
(define (make-dispatch-vhost-re ht)
  ;; まず、完全マッチとワイルドカードマッチの二つに分類する。
  ;; ついでなので、この段階で、regexp-quoteも行う
  ;; （complete-vhost-listのみ、頭に^をつける必要がある為）
  (receive (complete-vhost-list wildcard-vhost-list)
    (let loop ((paths
                 (string-size-sort ; 結果構築時に自然にreverseされる
                   (hash-table-keys ht)))
               (c-result '())
               (w-result '())
               )
      (if (null? paths)
        (values c-result w-result)
        (apply
          loop
          (cdr paths)
          (let1 path (car paths)
            (if (#/^\./ path)
              (list
                c-result
                (cons (regexp-quote path) w-result))
              (list
                (cons (list "^" (regexp-quote path)) c-result)
                w-result))))))
    ;; この二つのリストを使ってregexpを構築する。
    (string->regexp
      (tree->string
        (list
          "("
          (intersperse
            "|"
            (map
              (lambda (item)
                (list
                  "(?i:" ; host名は大文字小文字を区別しない
                  item ; 既にregexp-quote済
                  ")"))
              (append
                complete-vhost-list
                wildcard-vhost-list)))
          ")(?:\\:\d+)?$"))))) ; port-numberは無視する





;; 最終的に、以下のような文字列を生成する
;; "^((?:/aaa/bbb)|(?:/ccc/ddd)|(?:/eee/fff))((?:\\/.*)?)$"
;; ↑(match 1)でscript-nameが、(match 2)でpath-infoが取得できる
(define (make-dispatch-path-re ht)
  (string->regexp
    (tree->string
      (list
        "^("
        (intersperse
          "|"
          (map
            (lambda (item)
              (list
                "(?:"
                (regexp-quote item)
                ")"))
            (string-size-reverse-sort
              (hash-table-keys ht))))
        ")((?:\\/.*)?)$"))))




(define (make-dispatch-vhost alist)
  (and
    alist
    (list? alist)
    (not (null? alist))
    (let* ((ht (alist->hash-table
                 (map
                   (lambda (set)
                     (cons
                       (car set)
                       (make-dispatch-path (cadr set))))
                   alist)
                 'string=?))
           (re (make-dispatch-vhost-re ht))
           )
      (make <tcpcgi.dispatch.vhost>
        :dispatch-table ht
        :dispatch-regexp re
        ))))

(define (make-dispatch-path alist)
  (and
    alist
    (list? alist)
    (not (null? alist))
    (let* ((ht (alist->hash-table
                 (map
                   (lambda (set)
                     (cons
                       (car set)
                       (make-dispatch-none (cdr set))))
                   alist)
                 'string=?))
           (re (make-dispatch-path-re ht))
           )
      (make <tcpcgi.dispatch.path>
        :dispatch-table ht
        :dispatch-regexp re
        ))))

(define (make-dispatch-none param)
  (and
    (not (null? param)) ; '()では無い事を保証
    (let* ((param-is-list? (list? param))
           (cgi-target (if param-is-list?
                         (car param)
                         param))
           (keywords (if param-is-list?
                         (cdr param)
                         '()))
           )
      (apply
        make <tcpcgi.dispatch.none>
        :cgi-target cgi-target
        keywords))))



;; (value dispatch-path server-name) を返す。
;; マッチしなかった場合は、dispatch-pathとして#fを返す。
(define-method dispatch.vhost->dispatch.path ((dispatch-vhost
                                                <tcpcgi.dispatch.vhost>)
                                              target)
  (or
    (and-let* ((re (dispatch-regexp-of dispatch-vhost))
               (m (re target))
               (match-name (m 1))
               (dispatch-path
                 (ref (dispatch-table-of dispatch-vhost) match-name))
               )
      (values
        dispatch-path
        target))
    (values #f #f)))


;; (value dispatch-none script-name plain-path-info) を返す。
;; マッチしなかった場合は、dispatch-noneとして#fを返す。
;; ※但し、"/"のみの指定があった場合は、特別扱いとして、
;;   path-infoでのマッチングを行わない。
(define-method dispatch.path->dispatch.none ((dispatch-path
                                               <tcpcgi.dispatch.path>)
                                             target)
  (or
    (and-let* ((re (dispatch-regexp-of dispatch-path))
               (m (re target))
               (script-name (m 1))
               (plain-path-info (m 2))
               (not-root? (or
                            ;; script-nameが/でないなら問題無し
                            (not (string=? "/" script-name))
                            ;; script-nameが/で、path-infoが無いのもok
                            ;; しかし、そうでないなら却下
                            (string=? "" plain-path-info)))
               (dispatch-none
                 (ref (dispatch-table-of dispatch-path) script-name))
               )
      (values
        dispatch-none
        script-name
        (and
          (not (string=? "" plain-path-info))
          plain-path-info)))
    (values #f #f #f)))


;; (list cgi-target :nph boolean) を返す。
;; キーワード部分は今後追加になる（authキーワードとか）。
(define-method dump-dispatch.none ((dispatch-none <tcpcgi.dispatch.none>))
  (list
    (cgi-target-of dispatch-none)
    :nph (nph-of dispatch-none)
    ))



(provide "tcpcgi/dispatch")

