;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; htmlの見た目の設定を保持し、必要に応じてprint outするクラス(主にcgi用)
;;; print out形式はtext.treeなので、必要に応じてtree->string等を行う事。
;;; コンテンツ本体はprint out時に:bodyキーワード等で指定する。

;;; (現在のところの)デフォルト値一覧:
;;; - :encoding (symbol->string (gauche-character-encoding))
;;; - :robots "NOINDEX,NOFOLLOW"

(define-module tir03.cgi.html-render
  ;(use gauche.parameter)

  ;(use text.tree)
  ;(use text.html-lite)
  ;(use www.cgi)
  (use srfi-1)

  (use tir03.cgi)

  (export
    <html-render>
    spawn
    ;; プリントアウト用メソッド
    render-to-html-tree
    render-to-cgi-tree
    render-to-http-tree
    ))
(select-module tir03.cgi.html-render)


;;; --------


(define *default-encoding* (symbol->string (gauche-character-encoding)))
(define *empty* (gensym))

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

(define-macro (define-class-<html-render>)
  (eval
    `(define-class <html-render> ()
       ,(map
          (lambda (slot-symbol)
            `(,slot-symbol
               :init-keyword ,(make-keyword slot-symbol)
               :init-value *empty*))
          (uniq
            (append
              (get-html-tree-keyword-symbols)
              (get-http-tree-keyword-symbols)
              (get-cgi-tree-keyword-symbols)))))
    (current-module)))
(define-class-<html-render>)


(define-method initialize ((self <html-render>) initargs)
  (next-method)
  ;; 今のところは無し
  )


;;; --------

;; スロットシンボルのlist
(define *slots* (map (cut car <>)
                     (class-slots <html-render>)))

(define-method spawn ((original <html-render>) . override-keywords)
  (let1 new-instance (make <html-render>)
    (for-each
      (lambda (slot-symbol)
        (set!
          (ref new-instance slot-symbol)
          (get-keyword*
            (make-keyword slot-symbol)
            override-keywords
            (ref original slot-symbol))))
      *slots*)
    new-instance))


(define (render self render-proc slot-name-list extra-keywords)
  (define (process-keywords)
    ;; 処理概要:
    ;; - selfから、slot-name-listにあるslotからだけ、値を取り出す
    ;; - その際に更に、値が*empty*のものは除外する
    ;; - 上記の結果に対して、extra-keywordsで上書きする
    ;; - 最後に、特定スロットが*empty*だった為に存在しない場合のみ、
    ;;   デフォルト値として追加(cons)する。
    ;; 実際の処理:
    ;; - 上記の処理概要は副作用的なので、以下のように行う。
    ;; - 以下のリストを結合する。
    ;; -- extra-keywords全て
    ;; -- selfのslotの内、値が*empty*ではなく、extra-keywordsに無いもの
    ;; - 上記の結果にに特定slotが含まれていないなら、
    ;;   特定slotをデフォルト値として追加(cons)する
    (let1 interim-result (fold
                           (lambda (slot-symbol tail)
                             (let1 val (ref self slot-symbol)
                               (if (eq? val *empty*)
                                 tail
                                 (let1 key (make-keyword slot-symbol)
                                   (if (eq? *empty*
                                            (get-keyword key
                                                         extra-keywords
                                                         *empty*))
                                     (list* key val tail)
                                     tail)))))
                           extra-keywords
                           slot-name-list)
      (define (append-default-kv key default-value kv-list)
        (if (eq? *empty* (get-keyword key kv-list *empty*))
          (list* key default-value kv-list)
          kv-list))

      (append-default-kv :encoding *default-encoding*
                         (append-default-kv :robots "NOINDEX,NOFOLLOW"
                                            interim-result))))

  (let1 result-keywords (process-keywords)
    (apply render-proc result-keywords)))

(define-method render-to-html-tree ((self <html-render>) . extra-keywords)
  (render self html-tree-make (get-html-tree-keyword-symbols) extra-keywords))


(define-method render-to-cgi-tree ((self <html-render>) . extra-keywords)
  (render self cgi-tree-make (get-cgi-tree-keyword-symbols) extra-keywords))


(define-method render-to-http-tree ((self <html-render>) . extra-keywords)
  (render self http-tree-make (get-http-tree-keyword-symbols) extra-keywords))


;;; --------


(provide "tir03/cgi/html-render")
