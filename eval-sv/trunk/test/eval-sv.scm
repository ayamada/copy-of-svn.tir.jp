#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

(use gauche.test)
(use gauche.parameter)
(use srfi-1)
(use util.list)
;;; 普通にtest caseを書く
;;; (問題があるかどうかはあとで考える)

;;; TODO: バージョンアップ時のチェックの為に、
;;;       (map car (hash-table->alist (module-table (find-module 'gauche))))
;;;       等を使って、束縛やsyntax等の過不足が無い事をチェックしたい

(set! *test-report-error* #f)
(debug-print-width *test-report-error*)

(define param-handler
  (make-parameter (lambda args #f)))

;;; ----

(test-start "eval-sv")

(use eval-sv)

(test-module 'eval-sv)

;;; ----

;; メインで使うeval/svを用意する。
;; このeval/svは、param-handlerを変更する事で、
;; ある程度挙動を左右できる。
;; (この挙動は必要に応じて拡張するかも)
(define (default-sv-proc type symbol expr args return except)
  ((param-handler) type symbol expr args return except)
  (apply expr args))
(define-values (eval/sv env) (make-eval/sv :default-sv-proc default-sv-proc
                                           :isolate-port? #f
                                           ))

;; ここでは、特定のS式を普通に評価した場合と、eval/svで評価した場合を
;; 比較する事がメインになるので、それを記述する為のマクロを用意する
(define-macro (test/eval/sv expr)
  (let ((label (format "(eval/sv '~s)" expr))
        (result (guard (e (else *test-error*))
                  (eval expr (current-module))))
        )
    `(test* ,label ',result
       (eval/sv ',expr))))

;;; ---

(test-section "check immediate value")
(test/eval/sv 123)
(test/eval/sv "abc")
(test/eval/sv #t)
(test/eval/sv #f)
(test/eval/sv unbound-symbol)
(test/eval/sv quote) ; eval/svで、実体が中と外で同じものはquote系のみ

(test-section "check quote")
;; quote quasiquote unquote unquote-splicing
(test/eval/sv 'symbol)
(test/eval/sv ''symbol)
(test/eval/sv '(1 2 3 4))
(test/eval/sv '(5 6 ,(+ 7 8) 9))
(test/eval/sv `(5 6 ,(+ 7 8) 9))
(test/eval/sv `(5 6 ,@(list 7 8) 9))
(test/eval/sv ,+)

;; TODO: 全般的に、無限ループを阻止できるかどうかのチェックが無い
;; - 本格的なチェックは停止問題になるので行わないが、
;;   ループ内でsvが実行されているかどうかの確認ぐらいはする必要がある

(test-section "check syntax")
;; lambda
(test/eval/sv ((lambda () (+ 1 2))))
(test/eval/sv ((lambda (a) (+ 1 a)) 3))
;; begin
;; define
(test/eval/sv (define (abc9) 9)); top level define (1)
(test/eval/sv (abc9)) ; top level define (2)
(test/eval/sv (begin (define (abc88) 88) (abc88))) ; with begin
(test/eval/sv ((lambda () (define (abc3) 3) (abc3)))) ; internal define
;; if
;; cond
;; case
;; and
;; or
(test/eval/sv (if #t 1 (not-reached)))
(test/eval/sv (if #f (not-reached) 2))
(test/eval/sv (if #t (not-found) (not-found)))
(test/eval/sv (cond
                (#f (not-reached))
                (#t 'ok)
                (else
                  (not-reached))))
(test/eval/sv (case 3
                ((1 2 4 5) (not-reached))
                ((3 6 9) 'ok)
                (else
                  (not-reached))))
(test/eval/sv (call/cc
                (lambda (return)
                  (and
                    #t
                    (return 'ok)
                    (not-reached)))))
(test/eval/sv (call/cc
                (lambda (return)
                  (or
                    #f
                    (return 'ok)
                    (not-reached)))))
;; let
;; let*
;; letrec
(test/eval/sv (let ((hoge 3)) ((lambda () #t)) hoge))
(test/eval/sv (let next ((data '(1 2 3)))
                (if (null? data)
                  0
                  (+ (car data) (next (cdr data))))))
(test* "named let" "named let"
  (let1 count 10
    (eval/sv '(let loop ()
                (loop)
                'not-reached)
             (lambda (type symbol expr args return except)
               (dec! count)
               (if (<= count 0)
                 (return "named let")
                 (apply expr args))))))
(test/eval/sv (let* ((hoge 3) (hoge2 (+ hoge 1))) ((lambda () #t)) hoge2))
(test/eval/sv (letrec ((a (lambda () b))
                       (b (lambda () a)))
                (eq? b ((((b)))))))
;; set!
;; do
;; TODO: なんとかtest caseとして自動判別できる形に落とし込む事
;; これは、doの繰り返しループ中に、一つも割り込み可能な手続きが無い場合でも、
;; 無限ループが発生する事を防ぐ為に、do自体が割り込みを発生させる事を
;; 確認できればokとする。
(test/eval/sv (begin
                (define aaa 0)
                (do
                  ((i 0 (+ i 1)))
                  ((< 10 i))
                  (set! aaa (+ aaa i)))
                aaa))
;; delay
(test/eval/sv (force (delay (+ 1 3))))

;; define-syntax
;; let-syntax
;; letrec-syntax
(test/eval/sv (begin
                (define-syntax m1
                  (syntax-rules ()
                    ((_ a b) (+ a b))))
                (m1 55 66)))
(test/eval/sv (let-syntax ((m2 (syntax-rules ()
                                 ((_ a b) (+ a b)))))
                (m2 3 7)))
(test/eval/sv (letrec-syntax ((m1 (syntax-rules ()
                                    ((_ a b) (m2 a b a))))
                              (m2 (syntax-rules ()
                                    ((_ a b c) (list a b c))))
                              (m3 (syntax-rules ()
                                    ((_ a b) (m1 b a))))
                              )
                (m3 'first 'second)))


;; 以下は、R5RS外の、Gauche提供のsyntax。まだ未実装
;; define-generic
;; define-constant
;; define-method
;; define-inline
;; receive
;; define-class
;; and-let*
;; unless
;; when
;; define-macro
;; lazy
;(test/eval/sv (force (lazy (+ 1 3))))


(test-section "check procedure migration")

(test* "(lambda () (+ 2 4)) in -> out" ((lambda () (+ 2 4)))
  (let1 thunk (eval/sv '(lambda () (+ 2 4)))
    (thunk)))

(test* "(lambda () (+ 3 6)) out -> in" ((lambda () (+ 3 6)))
  (let1 thunk (lambda () (+ 3 6))
    (eval/sv `(,thunk))))

(test* "(lambda () (+ 4 8)) in -> out -> in" (+ 4 8)
  (let1 thunk (eval/sv '(lambda () (+ 4 8)))
    (eval/sv `(,thunk))))

(test-section "check call/cc")

(test* "call/cc in -> out" 11
  (let/cc return
    (eval/sv `(,return 11))))

(test* "call/cc out -> in" 12
  (let1 executed #f
    (let1 cont/val (eval/sv '(call/cc
                               (lambda (ret)
                                 (call/cc
                                   (lambda (cont)
                                     (ret cont)))
                                 12)))
      (if executed
        cont/val
        (begin
          (set! executed #t)
          (cont/val))))))

(test-section "check importing list")

(test* "compare *dynamic-load-path*" *dynamic-load-path*
  (let1 eval/sv (make-eval/sv :parent-module 'eval-sv.template.r5rs-all-debug)
    (eval/sv '*dynamic-load-path*)))

(test-section "check arity")

(define (arity->num+opt proc)
  (let1 a (arity proc)
    (if (arity-at-least? a)
      (list (arity-at-least-value a) #t)
      (list a #f))))

(test* "arity of +" (arity->num+opt +)
  (eval/sv `(,arity->num+opt +)))

(test* "arity of cons" (arity->num+opt cons)
  (eval/sv `(,arity->num+opt cons)))

(test* "arity of list-ref" (arity->num+opt list-ref)
  (eval/sv `(,arity->num+opt list-ref)))

(test-section "check return / except")

;; TODO: なんとかtest caseとして自動判別できる形に落とし込む事
(test* "(return 123)" 123
  (parameterize ((param-handler (lambda (type symbol expr args return except)
                                  (return 123))))
    (eval/sv `(begin
                (+ 1 3 5)
                (not-reached)))))

(test* "(except 456)" *test-error*
  (parameterize ((param-handler (lambda (type symbol expr args return except)
                                  (except 456))))
    (eval/sv `(begin
                ;; (+ 1 3 5)が普通にエラー例外を投げたら、それを無効化する
                ;; (exceptは継続で脱出してからエラー例外を投げなくてはならない)
                ;; guardはマクロなので、with-error-handlerを代わりに使う
                (,with-error-handler
                  (lambda (e) 1)
                  (lambda ()
                    (+ 1 3 5))
                  :rewind-before #t)
                2))))


(test-section "check circular-list")

(define l (circular-list 1)) ; 単純な循環list
(define l2 ; circular-list?では判別できない、循環tree
  (let ((a (list 1 2))
        (b (list 3 4)))
    (set-car! a b)
    (set-car! b a)
    a))

(test* "write circular-list" (with-output-to-string (cut write/ss l))
  (with-output-to-string
    (lambda ()
      (eval/sv `(write (quote ,l))))))
(test* "display circular-list(1)" *test-error*
  (with-output-to-string
    (lambda ()
      (eval/sv `(display (quote ,l))))))
(test* "display circular-list(2)" *test-error*
  (with-output-to-string
    (lambda ()
      (eval/sv `(display (quote ,l2))))))
;; まだformatがimportされてない
;(test* "format circular-list" (with-output-to-string (cut write/ss l))
;  (with-output-to-string
;    (lambda ()
;      (eval/sv `(display (quote ,l))))))

;; TODO: もっとtest caseを追加する必要がある

(test-section "eval-sv ok")

;;; ----

(test-end)
