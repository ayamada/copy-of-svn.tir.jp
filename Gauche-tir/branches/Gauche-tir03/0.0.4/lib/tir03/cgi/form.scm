;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; cgiパラメータの要求、取得、ベリファイ等を行うクラス。

;;; 目的:
;;; - htmlフォーム出力、パラメータのサニタイズ、
;;;   (spaceのトリミングや半角全角等の)変換、verify、
;;;   入力値のエラー表示、プレビュー表示、(文字列から数値等への)型変換、
;;;   (input hiddenによる)CGIパラメータの保持。
;;;   これらは「一連のCGIパラメータの操作」という事で一貫しているが、
;;;   通常、あちこちにバラバラに実装される事になる。
;;;   これらを何とかして一箇所にまとめたい。

;;; 仕様:
;;; - text.html-liteを拡張したミニ言語(?)を実装する必要がある。
;;; -- (html:input :type "text" :name "hoge" :value "hoge"
;;;                :x-conv conv-proc
;;;                :x-checker checker-proc
;;;                :x-class <integer>)
;;; - ???


(define-module tir03.cgi.form
  (use gauche.parameter)

  ;(use text.tree)
  ;(use text.html-lite)
  ;(use www.cgi)

  ;(use tir03.cgi)

  (export
    <cgi-form>
    expand-to-form-html ; htmlフォームに展開
    expand-to-confirm-html ; 入力された内容をプレビュー表示htmlに展開
    ;with-??? ; まだ仕様不明
    ;form->hiddens ; フォームの入力内容を、一連のhiddenタグに変換する。
    ;; ToDo: ↑邪魔なデータを除外する手段を提供する必要がある
    ))
(select-module tir03.cgi.form)


;;; --------


(define-class <cgi-form> ()
  (
   (template-html
     :accessor template-html-of
     :init-keyword :template-html
     :init-value #f)

   ;; internal slot
   (parsed-chunk
     :accessor parsed-chunk-of
     :init-value #f)
   ))


(define-method initialize ((self <cgi-form>) initargs)
  (next-method)
  ;; 今のところは無し
  ;; (この時点ではparsed-chunkは生成しない。遅延生成する事)
  )


;;; --------


(define-method expand-to-form-html ((self <cgi-form>) . extra-keywords)
  (error "sorry, not implemented yet"))

(define-method expand-to-confirm-html ((self <cgi-form>) . extra-keywords)
  (error "sorry, not implemented yet"))



;;; --------


(provide "tir03/cgi/form")
