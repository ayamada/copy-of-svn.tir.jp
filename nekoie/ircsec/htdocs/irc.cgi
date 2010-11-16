#!/usr/local/gauche-0.8.14/bin/gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ircsecと連携して機能する、ircログ表示cgi
;;; 以下の機能が要求される
;;; - 認証されたユーザのみを対象とする
;;; -- 認証方法は、チケット方式
;;; - ircのログを表示する
;;; -- 特定単語の強調機能付き
;;; -- 特定日時帯からの選択機能付き
;;; - 社内で使うので、あんまり真面目に作り込む必要はない

;;; ircsec側でのインターフェースは、以下のようになる予定
;;; - あとで

;;; TODO: インターフェースを決める必要がある
;;; 必要になる情報は以下の通り
;;; (但し、全てをurl経由で渡す必要はなく、内部チケット経由で渡してもよい)
;;; - 認証情報
;;; - 有効期限情報
;;; - 表示させるログ
;;; -- ハードリミットを認証情報で設定し、
;;;    ソフトリミットをcgiパラメータで指定する
;;;    (当然、どこかにソフトリミット変更のフォームを表示させる)
;;; - 強調文字列情報

;;; TODO: file.atomicモジュールを作る
;;; (当初の予定ではwdsmをストレージにする予定だったが、どうも
;;;  この用途には向かない事が判明したので、
;;;  代わりに、file.atomicを使い、この中にqueueを記録して、
;;;  代用とする事にする)


(use gauche.parameter)
(use www.cgi)

(define *base-dir* "/home/nekoie/ircsec")
(define *log-dir* "logs")


;;; ----


;;; ----


(define (get-paths)
  (cgi-get-metavariable "PATH_INFO")
  ...)


(define (main args)
  (parameterize ((cgi-output-character-encoding "Shift_JIS"))
    (cgi-main
      (lambda (params)
        (let1 paths (get-paths)
          ...)))))



