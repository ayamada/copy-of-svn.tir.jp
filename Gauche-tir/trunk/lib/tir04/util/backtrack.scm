;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; バックトラックサポートモジュール。
;;; 参照透過性は無いが破壊的更新を起こさないので、
;;; 他のcall/ccと共存可能。

;;; TODO:
;;; 以下のような形のマクロにした方が分かりやすいと思う。
;;; (backtrack-let backtrack ((a 1) (b 2) (c 3))
;;;   ...)

#|
以下のように使う。

(receive (backtrack n m) (backtrack/values 0 20)
  ;; ↑nには0が、mには20が渡される。
  ;;   backtrackを実行すると、ここに戻ってくる。
  ;;   そして、backtrackを実行した際の引数がnとmに新しく渡される。
  ;;   (nとmの値が初期値かどうかで、初回実行かバックトラックかを判別できる)
  ;;   戻ってきた後でも、backtrackは変化しない。
  ;;   (何回でもbacktrackを再利用できる)
  (print n)
  (when (< n m)
    ;; ↓で、receiveのところまで戻る。
    (backtrack (+ 1 n) m))
  (print "done."))
|#


(define-module tir04.util.backtrack
  (export
    backtrack/values
    ))
(select-module tir04.util.backtrack)


(define-syntax backtrack/values
  (syntax-rules ()
    ((_ . initial-values)
     (let1 backtrack-point #f
       (receive backtrack-values (let/cc cont
                                   ;; このset!はletrec用途なので副作用無し
                                   (set! backtrack-point cont)
                                   (values . initial-values))
         (apply values backtrack-point backtrack-values))))))


(provide "tir04/util/backtrack")

