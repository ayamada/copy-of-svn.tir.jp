#!/usr/local/gauche-0.8.14/bin/speedygosh --goshpath=/usr/local/gauche-0.8.14/bin/gosh
;#!/usr/local/gauche-0.8.14/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; すること:
;;; - (試しに)ちゃんと正規化する事！
;;;   逆引きは総当たりで行う方針で。

;;; dbmの構造について:
;;; - keyはzid。
;;; - valは以下の構造。検索については常に総当たりを行う。
;;;   '(
;;;     :name ; uniqueな文字列
;;;     :class ; 分類(料理とか)のsymbol(逆引きする)
;;;     ;; - cooking 料理完成品
;;;     ;; - plant 施設
;;;     ;; - device 道具
;;;     ;; - material 素材(使用アイテム及び拾い集めるもの含む)
;;;     ;; - equipment 装備
;;;     ;; - ability アビリティ
;;;     ;; - exodus 脱出計画関連
;;;     :builders (...) ; 材料アイテムのzidのlist(逆引きする)
;;;     :requirements "..." ; 要求される能力値及び作成条件のtext(逆引きしない)
;;;     :note "..." ; 備考(作成回数、回復量、経験値、得られる品、値段等のtext)
;;;     )
;;; - 正規化モジュールみたいなのを考える
;;; -- uniqueになるkeyの数だけ、テーブル(相当)を作る
;;; -- インターフェース的には、一つのテーブルのように見えてるとベター

(define-module zi-item
  (export
    item-main
    item-dump
    ))
(select-module zi-item)

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

(use www.cgi.dispatch-tir)
(add-load-path "/home/nekoie/Gauche-tir/trunk/lib")
(add-load-path "/home/nekoie/j/ide/lib")
(use tir04.cgi.util)
(add-load-path "/home/nekoie/Gauche-tir/branches/Gauche-tir01/trunk")



;;; ----
;;; parameters

(define p:params (make-parameter '()))
(define p:path (make-parameter #f))
(define p:src (make-parameter #f))
(define p:encoding (make-parameter #f))
(define (cgp name . opt-args)
  (apply cgi-get-parameter name (p:params) opt-args))


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


(define (output-html . bodies)
  (cgi-tree-make
    :encoding (p:encoding)
    :css-url "http://css.tir.jp/tir.css" 
    :robots "NOINDEX,NOFOLLOW,NOARCHIVE"
    :title "ぢきゅうぢそくitemビューア"
    :body bodies
    ))



;;; ----
;;; misc

(define *class-table*
  '(("material" "素材及びアイテム")
    ("cooking" "料理")
    ("plant" "施設")
    ("device" "道具")
    ("handicraft" "工作")
    ("equipment" "装備")
    ("ability" "アビリティ")
    ("exodus" "脱出計画")
    ))


;;; ----


(define (name->data name)
  (find
    (lambda (data)
      (equal? name (data->name data)))
    (p:src)))

(define-cgi-entry (item:desc) '(("c" "d"))
  (let* ((name (cgp "name"))
         (data (name->data name)))
    (if (not data)
      (output-html (html:h1 "該当する完成品がありません"))
      (output-html
        (html:h1 "完成品詳細")
        (html:p
          (html:em "「" (het name) "」"))
        (desc data)
        (html:hr)
        (html:p (cgi-entry->html:a 'item:fallback :label "一覧に戻る"))
        ))))

;; この手続きは、requirements及びnoteの文字列をhtml化する
;; (そこに含まれている、アイテム名と思われる文字列をリンク化しながら)
(define (desc->html desc)
  ;; TODO: あとで実装する
  (het desc))

(define (get-target-num data target-name)
  (any
    (lambda (m)
      (and
        (equal? target-name (material->name m))
        (material->num m)))
    (data->materials data)))
(define (desc data . opt-target-name)
  (let1 target-name (get-optional opt-target-name #f)
    (list
      (if target-name
        (list
          (html:em (format "~d個 " (get-target-num data target-name)))
          (cgi-entry->html:a 'item:desc
                             :label (data->name data)
                             :params `(("name" ,(data->name data))))
          (html:br))
        '())
      ;; TODO: あとで種別も出すようにする
      ;"種別: "
      ;...
      ;(html:br)
      "材料: "
      (intersperse
        "、"
        (map
          (lambda (m)
            (cgi-entry->html:a
              'item:reverse
              :label (format "~a(~d)" (material->name m) (material->num m))
              :params `(("name" ,(material->name m)))))
          (data->materials data)))
      (html:br)
      "要求: "
      (desc->html (data->requirements data))
      (html:br)
      "獲得: "
      (desc->html (data->note data))
      )))
(define (take-items target-name)
  (filter
    (lambda (data)
      (and
        (find
          (lambda (m)
            (equal? target-name (material->name m)))
          (data->materials data))
        data))
    (p:src)))

(define-cgi-entry (item:reverse) '(("c" "r"))
  (let1 target-name (cgp "name")
    (output-html
      (html:h1 "素材詳細")
      (html:p
        (html:em "「" (het target-name) "」")
        "を使用する完成品は以下の通り：")
      (let1 results (take-items target-name)
        (if (null? results)
          (html:div (html:strong "この素材を使用する完成品は存在しません"))
          (html:ul
            (map
              (lambda (data)
                (html:li (desc data target-name)))
              results))))
      (html:hr)
      (html:p (cgi-entry->html:a 'item:fallback :label "一覧に戻る"))
      )))

;; TODO: これは動作確認を取ってからtir03.cgi.utilに入れる事
(define (make-html:select name option-list . opt-selected)
  (html:select
    :name name
    (map
      (lambda (data)
        (let* ((value (if (pair? data)
                        (car data)
                        data))
               (label (cond
                        ((not (pair? data)) value)
                        ((null? (cdr data)) value)
                        ((not (pair? (cdr data))) (cdr data))
                        (else
                          (cadr data))))
               (selected+label (cond
                                 ((null? opt-selected) `(,label))
                                 ((equal? value (car opt-selected))
                                  `(:selected "selected" ,label))
                                 (else `(,label))))
               )
          (apply html:option :value value selected+label)))
      option-list)))

;; TODO: これは比較的使うので、パラメタライズして使い回した方がいい
(define (get-materials)
  (delete-duplicates
    (fold-right
      (lambda (data prev-result)
        (append
          (map
            (lambda (material)
              (material->name material))
            (data->materials data))
          prev-result))
      '()
      (p:src))))

(define-cgi-entry (item:list) '(("c" "l"))
  (output-html
    (html:h1 "材料逆引き一覧")
    (html:div
      (intersperse
        "\n / \n"
        (map
          (lambda (material-name)
            (cgi-entry->html:a 'item:reverse
                               :label material-name
                               :params `(("name" ,material-name))))
          (get-materials))))
    (html:hr)
    (html:h1 "完成品一覧")
    (html:div
      (intersperse
        "\n / \n"
        (map
          (lambda (data)
            (cgi-entry->html:a 'item:desc
                               :label (data->name data)
                               :params `(("name" ,(data->name data)))))
          (p:src))))
    ))

(define-cgi-entry (item:fallback) '()
  ;; パラメータ指定無し時のfallback
  (item:list))


;;; ----

(define (materials? alist)
  (if (null? alist)
    #t
    (and
      (list? alist)
      (string? (material->name (car alist)))
      (integer? (material->num (car alist)))
      (materials? (cdr alist)))))

(define (material->name material)
  (car material))
(define (material->num material)
  (cadr material))

(define (data->class data)
  (list-ref data 0))
(define (data->name data)
  (list-ref data 1))
(define (data->materials data)
  (list-ref data 2))
(define (data->requirements data)
  (list-ref data 3))
(define (data->note data)
  (list-ref data 4))

(define (check-type data)
  ;; (class-symbol "名前" (("材料1" 3) ...) "..." "...")
  (unless (list? data)
    (error "invalid type" data))
  (unless (= 5 (length data))
    (error "invalid length" data))
  (unless (symbol? (data->class data))
    (error "invalid class-symbol" data))
  (unless (string? (data->name data))
    (error "invalid name" data))
  (unless (materials? (data->materials data))
    (error "invalid materials" data))
  (unless (string? (data->requirements data))
    (error "invalid requirements" data))
  (unless (string? (data->note data))
    (error "invalid note" data))
  #t)

(define (read-src src-path)
  (with-input-from-file
    src-path
    (lambda ()
      (let next ((result '()))
        (let1 data (read)
          (if (eof-object? data)
            (reverse result)
            (begin
              (check-type data) ; 型チェックする
              (next (cons data result)))))))))

(define (item-main . keywords)
  (let-keywords keywords ((src-path #f)
                          (encoding #f)
                          )
    (cgi-main
      (lambda (params)
        (let1 src (guard (e (else #f))
                    (read-src src-path))
          (if (condition? src)
            (output-html
              (html:h1 "データファイルの解析に失敗しました")
              (html:pre (het (ref src 'message))))
            (parameterize ((p:params params)
                           (p:path (and-let* ((pl (get-path-info-keylist)))
                                     (string-join pl "/")))
                           (p:encoding encoding)
                           (p:src src))
              ;; ディスパッチ
              ((cgi-params-dispatch params))))))
      :on-error cgi-on-error/stack-trace)))

(define (item-dump . keywords)
  (let-keywords keywords ((src-path #f)
                          (encoding #f)
                          )
    (parameterize ((p:encoding encoding)
                   (p:src (read-src src-path)))
      (dump))))

(define (data->dump-string data)
  (tree->string
    (list
      ;; アイテム名
      (data->name data)
      ;; 材料
      "["
      (intersperse
        "、"
        (map
          (lambda (material)
            (list
              (material->name material)
              (material->num material)))
          (data->materials data)))
      "]"
      ;; その他に要求されるもの
      (let1 requirements (data->requirements data)
        (if (equal? requirements "")
          '()
          (list "(" requirements ")")))
      )))

(define (dump)
  (for-each
    (lambda (data)
      (print (data->dump-string data)))
    (p:src)))



(provide "zi-item")


