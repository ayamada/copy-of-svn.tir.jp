;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; alist table module

;;;   Copyright (c) 2005 atsuo yamada, All rights reserved.
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;;; 概要:
;;; alistをkeyに使える、hash-tableのようなクラス。
;;; 重い。
;;; alistに対するマッチングっぽいものが可能な事以外にはメリットは無し。
;;; alistのvalueとして、:*を指定する事で、wildcard指定が使用可能。

;;; note: alistのvalueに対するwildcardは、細かく指定する事もできる(多分)。
;;;       '(("key" . ("val1" "val2" . :*))) とか。

;;; note:
;;; 要するに、与えたalistが、このテーブルのkeyに指定したalist(template)を
;;; 内包していると、「マッチしている」とみなします。
;;; 尚、マッチするtemplateが複数ある場合は、以下の条件で優先順位が決定される。
;;; - 優先度大: 優先key(最もよく使用されているkey)を持つtemplateである事。
;;; - 優先度中: templateの条件が厳しい(多くの要素を持つalistである)事。
;;; - 優先度小: wildcardを使っていないtemplateである事。

;;; note: このクラスは(gensym)を含む為、シリアライズすると、結果がおかしくなる
;;;       可能性が高い。但し、wildcardを使わず、シリアライズする前に
;;;       cached-tree-delete!すれば、シリアライズ可能。

;;; 用語:
;;; - template = require-template = template-alist
;;;   <alist-table>での、hash-tableにおけるkeyの役を果たすもの。
;;;   tableを引く際に、このtemplateの全てのkeyとvalueにマッチする要素を、
;;;   指定されたalistが含んでいるなら、マッチしたとみなされ、entityが得られる。
;;;   (要するに、tableを引く際に指定するalistに内包されるようなtemplateが
;;;    選ばれ、そのvalueが返る、という事)
;;; - entity = value
;;;   <alist-table>での、hash-tableにおけるvalueの役を果たすもの。
;;;   普通にkeyとvalueと言うと、template-alistのkeyとvalueと混同してしまった
;;;   ので、分かりやすくする為に、わざわざ名前を別にする事にした。



(define-module www.cgi.dispatch-tir.alist-table
  (use gauche.parameter)

  (use srfi-1)
  (use srfi-2) ; and-let*

  (use util.list)

  (export
    <alist-table>

    alist-table-put!
    alist-table-get

    alist-table-fold
    alist-table-fold-right
    alist-table-map
    alist-table-for-each
    alist-table-any

    ;; for internal manipulation
    cached-tree-get
    cached-tree-delete!
    cached-tree-update!
    ))
(select-module www.cgi.dispatch-tir.alist-table)


;;; --------


(define *fallback-is-empty* (gensym)) ; fallbackが無い事を示すシンボル


;;; --------
;;; wildcard判定関連

;; note:
;; これらの関数群を使用する前には、必ず、
;; (parameterize ((is-wildcard?-parameter (is-wildcard?-pred-of self))) ... )
;; する事！
;; 具体的には、cached-tree-update!時とalist-table-get時に必要になる。
;; 他にも、関数を追加した際に必要になるかもしれない。

;; デフォルトのwildcard判定関数
(define (is-wildcard?-default x)
  (eq? :* x))

;; wildcard判定関数を使いやすくする為のgauche.parameterとラッパー
(define is-wildcard?-parameter (make-parameter is-wildcard?-default))

(define (is-wildcard? x)
  ((is-wildcard?-parameter) x))


;; WARN: 通常のequal?とは違い、template側の位置が固定な事に注意
(define (wildcard-equal? template value)
  (or
    (equal? template value) ; 普通に一致するなら、そのまま通す
    (is-wildcard? template) ; templateがwildcardなら、なんでも#t
    (and
      (pair? template)
      (pair? value)
      ;; 両方ともpairなら、再帰的にチェックする
      (wildcard-equal? (car template) (car value))
      (wildcard-equal? (cdr template) (cdr value)))))

;; WARN: template-vals側にwildcardを含む。templateのkeyはwildcardは考慮しない。
(define (wildcard-assoc-ref template-alist key . opt-default)
  (or
    (any
      (lambda (template-key+val)
        (and
          (wildcard-equal? (car template-key+val) key)
          (cdr template-key+val)))
      template-alist)
    (get-optional opt-default #f)))



;;; --------


(define-class <alist-table> ()
  (
   ;; wildcardを変更したい時は、ココに新しいwildcard判定関数を入れる
   ;; (デフォルトのwildcardは、:*だが、万が一「:*」がtemplateに出現する可能性が
   ;;  あるなら、適当に、templateに出現しないwildcardを(gensym)等で
   ;;  用意しなくてはならない。)
   ;; ただ単にwildcard機能を殺したい時は、適当に(lambda (x) #f)でも指定する事。
   (is-wildcard?-pred
     :accessor is-wildcard?-pred-of
     :init-keyword :is-wildcard?-pred
     :init-value is-wildcard?-default)

   ;; entry-listは、以下のようなlistとする
   ;; '((entity . template-alist) ...)
   (entry-list
     :accessor entry-list-of
     :init-keyword :entry-list
     :init-value '())

   ;; alist-table高速検索の為の各種キャッシュ情報をココに保存する
   (cached-tree
     :accessor cached-tree-of
     :init-value #f)
   ))




(define-method alist-table-put! ((self <alist-table>) template entity)
  ;; cached-treeをクリア
  (cached-tree-delete! self)
  ;; entry-listに追加
  (set!
    (entry-list-of self)
    (acons
      entity template
      (alist-delete template (entry-list-of self) alist-equal?))))





(define-method alist-table-get ((self <alist-table>)
                                target-alist . opt-fallback)
  (define (do-fallback)
    (if (null? opt-fallback)
      (errorf "alist table doesn't have an entry for key ~s" target-alist)
      (car opt-fallback)))

  (if (null? (entry-list-of self))
    (do-fallback)
    (parameterize ((is-wildcard?-parameter (is-wildcard?-pred-of self)))
      (unless (cached-tree-of self)
        (cached-tree-update! self))
      (let1 result (resolve-tree (cached-tree-of self) target-alist)
        (if (eq? result *fallback-is-empty*)
          (do-fallback)
          result)))))

(define (resolve-tree cached-tree target-alist)
  (let1 fallback (cadr cached-tree)
    (let resolving ((tree (car cached-tree)))
      (if (null? tree)
        fallback
        (receive (current-branch next-trunk) (car+cdr tree)
          (let ((tree-key (car current-branch))
                (tree-val-list (cadr current-branch))
                )
            (let1 target-key+val (assoc tree-key target-alist)
              (if (not target-key+val)
                (resolving next-trunk)
                (let* ((target-val (cdr target-key+val))
                       (node (wildcard-assoc-ref tree-val-list target-val))
                       )
                  ;; nodeは、次のどっちか。
                  ;; - another-cached-tree
                  ;; -- 次の木を引けばいい。
                  ;; - #f
                  ;; -- wildcard-assoc-refが#fを返した時のみ出現。
                  ;; --- つまり、keyはあるが、valueが一致しなかった場合。
                  ;;     この時は、fallbackがあるなら、fallbackを返すべき。
                  (if node
                    (let1 result (resolve-tree node target-alist)
                      (if (eq? result *fallback-is-empty*)
                        fallback
                        result))
                    (resolving next-trunk)))))))))))



(define-method cached-tree-get ((self <alist-table>))
  (cached-tree-of self))


(define-method cached-tree-delete! ((self <alist-table>))
  (set! (cached-tree-of self) #f))


(define-method cached-tree-update! ((self <alist-table>))
  (parameterize ((is-wildcard?-parameter (is-wildcard?-pred-of self)))
    (set!
      (cached-tree-of self)
      (make-cached-tree (entry-list-of self)))))



(define (make-cached-tree entry-list)
  (receive (fallbacks others) (partition
                                (lambda (entry)
                                  (null? (cdr entry)))
                                entry-list)
    (list
      (entry-list->internal-tree others)
      (if (null? fallbacks)
        *fallback-is-empty*
        (caar fallbacks)))))

;; note: keyのsort順は、出現頻度。
;; note: valueのsort順は、そのvalueを通るalistが長い順(但し、wildcardは最後)。
(define (entry-list->internal-tree entry-list)
  (map
    (lambda (key)
      (let1 entry-list/key (filter
                             (lambda (entry)
                               (assoc key (cdr entry)))
                             entry-list)
        (list
          key
          (map
            (lambda (val)
              (cons
                val
                (make-cached-tree
                  (filter-map
                    (lambda (entry)
                      (receive (entity template) (car+cdr entry)
                        (and
                          (member (cons key val) template)
                          (cons
                            entity
                            (alist-delete key template equal?)))))
                    entry-list/key))))
            (uniq-sort-value/wildcard
              key
              (map cdr entry-list/key))))))
    (get-freq-key-list (map cdr entry-list))))


;; alistを長い順にsortし、指定キーのcdrを取り(alist-val)、uniqしたものを返す。
;; 但し、sortする際は、alist-valがwildcardを含むものを一番最後にする。
(define (uniq-sort-value/wildcard key target-alists)
  (define (get-wildcard-level val)
    (let search ((target val)
                 (level 0)
                 )
      (cond
        ((is-wildcard? target) level)
        ((not (pair? target)) #f)
        (else
          (let ((car-level (search (car target) (+ 1 level)))
                (cdr-level (search (cdr target) (+ 1 level))))
            (if (and car-level cdr-level)
              (min car-level cdr-level)
              (or car-level cdr-level)))))))

  (delete-duplicates ; 重複除去
    (map
      cadr ; 値だけを取り出す
      (sort
        ;; 最初に、sortしやすいように、wildcard所持かどうかと、
        ;; そのwildcardの位置の深さを調べておく
        (map
          (lambda (alist)
            (let1 val (assoc-ref alist key)
              (list*
                (get-wildcard-level val)
                val
                alist)))
          target-alists)
        (lambda (x y)
          (let ((x-wildcard-level (car x))
                (y-wildcard-level (car y)))
            (cond
              ((and x-wildcard-level y-wildcard-level)
               ;; wildcard-levelが低い(根本にwildcardがある)方を後にする
               ;; 同じなら、元の順位を保つ
               (<= y-wildcard-level x-wildcard-level))
              ;; wildcardを持つ方を後にする
              (x-wildcard-level #f)
              (y-wildcard-level #t)
              (else
                ;; 長い順に並べる
                ;; 同じなら、元の順位を保つ
                (<= (length y) (length x))))))))))


;; 再帰的に、freq-keyを求めていく
;; (sortされたcached-treeの第一レベルのkeysを生成する)
(define (get-freq-key-list alists)
  (if (null? alists)
    '()
    (let1 freq-key (get-most-freq-key (concatenate alists))
      (if (not freq-key)
        '()
        (cons
          freq-key
          (get-freq-key-list
            (filter
              (lambda (alist)
                (not (assoc freq-key alist)))
              alists)))))))


;; alistの中で一番頻度の高いkeyを返す
(define (get-most-freq-key alist)
  (let loop ((left alist) ; 調査対象
             (stat '())) ; keyがkey、valが出現数の結果alist
    (if (null? left)
      ;; 最後まで検索したなら、statの内で、最もカウントの高いkeyを返す。
      ;; statが空なら#fを返す。
      (car (fold
             (lambda (new old)
               (if (< (cdr new) (cdr old)) old new))
             '(#f . 0) ; fallback
             stat))
      ;; 再帰的にleftを処理する
      (loop
        (cdr left)
        (let1 key (caar left)
          (acons
            key
            (+ (assoc-ref stat key 0) 1)
            (alist-delete key stat equal?)))))))


;; treeの構造図:
;; `(
;;   (
;;    ;; pair ( template-key . child-alist )
;;    (template-key1 (
;;                    ;; pair ( template-value . child-node )
;;                    (template-value1 . ,another-tree-frame1)
;;                    (template-value2 . ,another-tree-frame2)
;;                    (:* . ,another-tree-frame3)
;;                    ))
;;    (template-key2 ( ... ))
;;    (template-key3 ( ... ))
;;    ...
;;    )
;;   ,entity) ; fallback(上のalistからkeyで引けなかった時に使われる)






(define-method alist-table-fold ((self <alist-table>) proc knil)
  (fold
    (lambda (entry r)
      (proc (cdr entry) (car entry) r))
    knil
    (entry-list-of self)))
(define-method alist-table-fold-right ((self <alist-table>) proc knil)
  (fold-right
    (lambda (entry r)
      (proc (cdr entry) (car entry) r))
    knil
    (entry-list-of self)))
(define-method alist-table-map ((self <alist-table>) proc)
  (map
    (lambda (entry)
      (proc (cdr entry) (car entry)))
    (entry-list-of self)))
(define-method alist-table-for-each ((self <alist-table>) proc)
  (for-each
    (lambda (entry)
      (proc (cdr entry) (car entry)))
    (entry-list-of self)))
(define-method alist-table-any ((self <alist-table>) proc)
  (any
    (lambda (entry)
      (proc (cdr entry) (car entry)))
    (entry-list-of self)))


;;; --------
;;; その他の関数


;; alistの子要素を(sortする為に)文字列化してsortして返す。
(define (alist->string&sort alist)
  (sort
    (map
      (cut write-to-string <>)
      alist)))

;; alistを、順序を気にせずに比較する関数
(define (alist-equal? x y)
  (or
    (equal? x y)
    (and
      (list? x)
      (list? y)
      (eqv? (length x) (length y))
      (let loop ((x-left (alist->string&sort x))
                 (y-left (alist->string&sort y)))
        (cond
          ((and (null? x-left) (null? y-left)) #t)
          ((not (equal? (car x-left) (car y-left))) #f)
          (else
            (loop (cdr x-left) (cdr y-left))))))))




;;; ----

(provide "www/cgi/dispatch-tir/alist-table")

