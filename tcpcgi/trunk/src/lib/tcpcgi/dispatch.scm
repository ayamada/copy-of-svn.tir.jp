;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$



(define-module tcpcgi.dispatch
  (use gauche.parameter)
  (use gauche.regexp) ; regexp-quote
  (use srfi-1) ; filter-map
  (use srfi-2) ; and-let*
  (use rfc.uri)
  (use text.tree)
  (use util.list) ; alist->hash-table intersperse
  (use www.cgi)

  (use tcpcgi.execute)
  (export
    <tcpcgi.dispatch>

    make-dispatch-for-fallback
    make-dispatch-for-path
    make-dispatch-for-vhost

    dispatch-from-fallback ; fallbackでディスパッチを行い、結果を得る
    dispatch-from-path ; pathでディスパッチを行い、結果を得る
    dispatch-from-vhost ; vhostでディスパッチを行い、結果を得る
    ))
(select-module tcpcgi.dispatch)


;; utility
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

(define (alist? alist)
  (and
    alist
    (null? alist)
    (list? (car alist))))



(define-class <tcpcgi.dispatch> ()
  (
   (dispatch-type :accessor dispatch-type-of
                  :init-keyword :dispatch-type
                  :init-value #f) ; 'vhost 'path 'fallback のどれか

   ;; 以下は、内部用スロット
   (dispatch-execute :accessor dispatch-execute-of
                     :init-keyword :dispatch-execute
                     :init-value #f)
   (dispatch-table :accessor dispatch-table-of
                   :init-keyword :dispatch-table
                   :init-value #f)
   ;; 上のテーブルのkeyを正規表現化したもの。
   ;; マッチング後、(match-obj 1)を使って上のテーブルを引くという仕様に注意。
   (dispatch-regexp :accessor dispatch-regexp-of
                    :init-keyword :dispatch-regexp
                    :init-value #f)
   ))




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








;; note :
;; - make-dispatch-for-*には、必ず一つの引数が渡る(keyword渡しなので)。
;; - make-dispatch-for-*の引数targetは、以下のどれか。
;; -- #f
;; --- 何もせずに、#fを返す
;; -- thunk
;; --- `(script ,thunk) の省略形
;; -- 文字列
;; --- `(filer ,文字列) の省略形
;; -- list
;; --- (make <tcpcgi.execute>) のkeywordとして渡すリスト
;; -- alist
;; --- vhost, pathの構築時に使う
;; -- null list
;; --- #fを返す。空のalistだった時に、コレが渡される可能性がある

(define (make-dispatch-for-fallback target)
  (if (or (not target) (null? target))
    #f
    (if (alist? target)
      (error "invalid target")
      (let1 e (make <tcpcgi.execute>
                :execute-list target)
        (and
          e
          (make <tcpcgi.dispatch>
            :dispatch-type 'fallback
            :dispatch-execute e))))))
(define (make-dispatch-for-path target)
  (if (or (not target) (null? target))
    #f
    (if (not (alist? target))
      (make-dispatch-for-fallback target)
      (let* ((ht (alist->hash-table
                   (map
                     (lambda (key&vals)
                       (cons
                         (car key&vals)
                         (make-dispatch-for-fallback (cdr key&vals))))
                     target)
                   'string=?))
             (re (make-dispatch-path-re ht))
             )
        (make <tcpcgi.dispatch>
          :dispatch-type 'path
          :dispatch-table ht
          :dispatch-reqexp re)))))
(define (make-dispatch-for-vhost target)
  (if (or (not target) (null? target))
    #f
    (if (not (alist? target))
      (make-dispatch-for-fallback target)
      (let* ((ht (alist->hash-table
                   (map
                     (lambda (key&vals)
                       (cons
                         (car key&vals)
                         (if (alist? (cadr key&vals))
                           (make-dispatch-for-path (cadr key&vals))
                           (make-dispatch-for-fallback (cdr key&vals)))))
                     target)
                   'string=?))
             (re (make-dispatch-vhost-re ht))
             )
        (make <tcpcgi.dispatch>
          :dispatch-type 'vhost
          :dispatch-table ht
          :dispatch-reqexp re)))))







(define-method dispatch-from-fallback ((self <tcpcgi.dispatch>))
  ;; executeeまたは#fを返す
  (dispatch-execute-of self))

(define-method dispatch-from-path ((self <tcpcgi.dispatch>) target-path)
  ;; (list executee script-name plain-path-info)または#fを返す
  (and-let* ((ht (dispatch-table-of self))
             (re (dispatch-regexp-of self))
             (m (re target-path))
             (script-name (m 1))
             (pre-plain-path-info (m 2))
             (executee (hash-table-get ht script-name))
             )
    (let1 plain-path-info (if (string=? "" pre-plain-path-info)
                            #f
                            pre-plain-path-info)
      (if (and
            (string=? "/" script-name)
            plain-path-info)
        #f ; script-nameが"/"の時のみ、path-info化しないようにする必要がある
        (list executee script-name plain-path-info)))))

(define-method dispatch-from-vhost ((self <tcpcgi.dispatch>) target-host
                                                             target-path)
  ;; (list executee script-name plain-path-info server-name)または#f
  ;; またはserver-nameのみ(vhostはマッチしたがpathマッチ失敗時)を返す
  (and-let* ((ht (dispatch-table-of self))
             (re (dispatch-regexp-of self))
             (server-name (or
                            (and-let* ((m (#/\:\d+$/ target-host)))
                              (m 'before))
                            target-host))
             (m (re server-name))
             (match-name (m 1))
             (path-instance (hash-table-get ht match-name))
             )
    (let1 dispatch-path-list (dispatch-from-path path-instance target-path)
      (if dispatch-path-list
        (append! dispatch-path-list (list server-name))
        server-name))))



(provide "tcpcgi/dispatch")

