;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; cgi内のformを扱いやすくする為のカプセル化クラス

;;; 概要:
;;; TODO: あとで書く


;;; TODO: XMLパーザは利用しない事にする。
;;;       代わりに、text.html-liteのhtml:form関連を強化した、
;;;       cgi:formのような代替手続きを提供し、
;;;       それを使ってもらうようにする
;;;       (これらの手続きは、cgiパラメータやその初期値等を考慮して
;;;        htmlを吐くようになる)
;;; TODO: formインスタンスは、それぞれ独自にカスタマイズされたクラスになる？
;;;       スロットに値が入る事を考えた場合、その方が分かりやすい？
;;;       それとも分かりにくい？


;;; TODO: これはtir04とは無関係にする？
;;;       もう少し考える必要はある。



;;; 必要なパラメータは？
;;; - フォーム用html片(を出力する手続き)(必須)
;;; - confirm用html片(を出力する手続き)(optional)
;;; - 入力値を検証する為の手続き(optionalだが実質上必須)
;;; - 入力値を矯正(空白除去等)する為の手続き(optional)
;;; - 入力値の初期値(optional?)
;;; - 他には？

;;; 必要なものは？
;;; - formインスタンスをform化するmethod
;;; - formインスタンスをconfirm化するmethod
;;; - formインスタンスにcgiパラメータを渡すmethod
;;; - formインスタンスからcgiパラメータを取り出すmethod
;;; - cgiパラメータを含むformインスタンスの検証手続きを実行し、
;;;   エラー内容(または#f)を取り出すmethod
;;; - 検証手続き/矯正手続き内で使う為のユーティリティ手続き類
;;; -- 年月日の入力フォーム部分等、よく使われるパターン等を生成する手続き類
;;; - cgiパラメータを含むformインスタンスの全値をhidden化するmethod
;;;   (confirm時のボタン用)
;;;   (可能なら、もう少し良いインターフェースを考えたい)

;;; その他の仕様:
;;; - オプショナル引数で、インスタンス内の値をオーバーライドできるとより便利？
;;; - フォーム化する際に、hiddenパラメータは、
;;;   </form>の直前に挿入するようにしたい
;;; - formインスタンスはチェーン化できる必要がある
;;;   (要するに、複数のページにまたがる入力項目を渡せる必要がある)
;;;   この実装も考える必要がある！
;;; -- シンプルに、複数のformインスタンスを使うのが一番簡単。
;;;    但し、この方法だと以前のページで入力したパラメータを
;;;    明示的にhiddenで含ませる必要があり、
;;;    その辺りをいちいち手動で指定していくのは少し面倒かも知れない。

;;; 仕様上の問題点:
;;; - TLのsetForm相当が必要だが、どう実現する？
;;;   真面目にXMLとしてパーズし、真面目に構成を行うか？
;;;   大変そうだが……やるしか無いようだ。
;;; -- 書き換えが必要になるのは、以下のタグ。
;;; --- <form ...> の、action
;;; --- <input type="text" ... /> の、value
;;; --- <input type="password" ... /> の、value
;;; --- <input type="hidden" ... /> の、value
;;; --- <input type="submit" ... /> の、value
;;; --- <input type="radio" ... /> の、checked
;;; --- <input type="checkbox" ... /> の、checked
;;; --- <textarea ...>...</textarea> の、テキスト部分
;;; --- <option ...>...</option> の、selected

#|
<cgi-form>は、
|#

(define-module tir04.cgi.form
  (use srfi-1)
  (use srfi-13)
  (use text.html-lite)
  (use www.cgi)
  (use tir04.cgi.util)

  (export
    <cgi-form>
    <cgi-form/params>
    <cgi-form-dispatch>
    ))
(select-module tir04.cgi.form)


(define-class <cgi-form> ()
  (
   ;; フォームhtmlのソース(文字列 / text-tree / 生成手続き)を保存する
   ;; (実際には、インスタンス生成時にXMLパーズしたものの方のみ利用する)
   (html-form
     :accessor html-form-of
     :init-keyword :html-form
     :init-value #f)

   ;; confirm用のhtmlのソースを保存する(optional)
   ;; (<span id="...">...</span>の要素を書き換えるようにする)
   (html-confirm
     :accessor html-confirm-of
     :init-keyword :html-confirm
     :init-value #f)

   ;; cgi.dispatch風にディスパッチする為の判別用listを保存する。
   ;; このインスタンスでディスパッチしない場合は、#fで構わない。
   (dispatch-alist
     :accessor dispatch-alist-of
     :init-keyword :dispatch-alist
     :init-value #f)
   ;; dispatch-alistでディスパッチした後に返されるべき値。
   ;; (仮。どう使うかは未定)
   (dispatch-result
     :accessor dispatch-result-of
     :init-keyword :dispatch-result
     :init-value #f)

   ;; 以下は、内部スロット
   (parsed-html-form
     :accessor parsed-html-form-of
     :init-value #f)
   (parsed-html-confirm
     :accessor parsed-html-confirm-of
     :init-value #f)
   ))


(define-method initialize ((self <cgi-form>) initargs)
  (next-method)
  ;; TODO: parsed-html-form parsed-html-confirm を生成する事
  )

(define-class <cgi-form/params> ()
  (
   ))

(define-method hoge ((self <cgi-form>) . args)
  #f)


(provide "tir04/cgi/form")
