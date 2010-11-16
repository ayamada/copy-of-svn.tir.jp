;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; "camouflage arity" module
;;;
;;;  Copyright (c) 2008 Atsuo Yamada, All rights reserved.
;;;
;;;  Permission is hereby granted, free of charge, to any person
;;;  obtaining a copy of this software and associated documentation
;;;  files (the "Software"), to deal in the Software without restriction,
;;;  including without limitation the rights to use, copy, modify,
;;;  merge, publish, distribute, sublicense, and/or sell copies of
;;;  the Software, and to permit persons to whom the Software is
;;;  furnished to do so, subject to the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;  AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
;;;  OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;;;  IN THE SOFTWARE.


(define-module eval-sv.cf-arity
  (use srfi-1)

  (export
    solve-arity ; TODO: もっと良い名前を考える
    camouflage-arity
    ))
(select-module eval-sv.cf-arity)

;; arityを解釈し、(複数存在する場合は「最も緩い条件」の)arity値を二値で返す。
;; (methodの場合、正しくarityを作成して返す事が困難なので、この仕様とする)
;; TODO: 正しくは、「最も緩い条件」ではなく、複数のarity値がある場合は、
;;       arity値のmergeが必要になる。あとで実装し直す事。
(define (solve-arity proc)
  (define (arity->num+opt a)
    (if (arity-at-least? a)
      (list (arity-at-least-value a) #t)
      (list a #f)))

  (let1 a (arity proc)
    (cond
      ((null? a) (values 0 #t)) ; 一部のmethodで、これがある……dummy値を返す
      ((not (list? a)) (apply values (arity->num+opt a)))
      (else
        (let* ((arities a)
               (num+opts (map arity->num+opt arities)) ; '(num opt)のlist
               (sorted (sort
                         num+opts
                         (lambda (x y)
                           (cond
                             ;; 固定引数部分でまず判定
                             ((< (car x) (car y)) #t)
                             ((< (car y) (car x)) #f)
                             ;; 固定引数部分が同じなら、オプショナル引数で判定
                             ((cadr x) #t)
                             ((cadr y) #f)
                             (else #t)))))
               )
          (apply values (car sorted)))))))

;; procが受け付けるarityを、指定条件のように見えるように偽装する
(define (camouflage-arity arg-num has-optional? proc)
  (let* (
         ;; オリジナルの引数シンボルのリスト(arg:1 arg:2)
         (arg-list (map
                     (lambda (i)
                       (string->symbol
                         (format "arg:~d" i)))
                     (iota arg-num)))
         ;; 末尾にoptional引数をつけたもの(arg:1 arg:2 . opt)
         (arg-list+ (apply list* (append arg-list
                                         (if has-optional? '(opt) '(())))))
         ;; 適用可能な形にしたもの(list* arg:1 arg:2 opt)
         ;; (前述の条件により、optは常にlistなので、不完全listにはならない)
         (arg-list* `(list* ,@arg-list ,(if has-optional? 'opt '())))
         ;; evalする式
         (result `(lambda ,arg-list+
                    (apply ,proc ,arg-list*)))
         )
    (eval result (current-module))))



(provide "eval-sv/cf-arity")

