;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; account scheduler

;;; 概要: 仮想os的なcms。

;;; 以前の仕様から変更した箇所:
;;; - 実ファイルシステムのmountを可能にする(以下、仮に、mountdirと呼ぶ)
;;;   (mountdirした配下は、通常のhttpd同様の挙動になる)
;;; -- ここで、%nnの解釈が必要になり、また、それに影響される、
;;;    面倒なセキュリティ上の問題が発生する事に注意する必要がある。
;;; - 特殊ファイル名の変更
;;; -- .content → .as_content
;;; -- .metadata → .as_metadata
;;; - anonymous(非ログイン)ユーザの作成
;;; -- データツリー全体をanonymousユーザから不可視にすれば従来通りの挙動になる
;;; - グループ管理の変更
;;; -- 詳細未定
;;; - ログイン機能もプラグインの一つとする
;;; -- urlは /path/to/as.cgi/login のようになる予定
;;; - directory indexは.as_contentのContent-Typeとプラグインシステムで実現する
;;; - プラグインシステムの詳細詰め
;;; -- プラグインは複数の種類があるものとする。
;;; --- アクションプラグイン。
;;;     直接pathに設置され、実行されるもの。
;;;     .as_contentの一機能として実装される。
;;;     実体はcgiの各機能をdelegateしているだけ。
;;; --- ライブラリプラグイン。
;;;     アクションプラグインからライブラリ的に参照されるプラグイン。
;;;     mountdir類似の、mountlibによって実装される。
;;;     実体はgaucheのモジュールシステムそのもの。
;;; ---- 現仕様では問題点があり、
;;;      「ここがライブラリプラグイン」だという事を
;;;      as本体に通知する必要がある。
;;;      (後付けで複数のライブラリプラグインを追加したような場合を想定)
;;;      どうすれば良い？
;;; --- システムプラグイン。
;;;     osのモジュール機能のように、
;;;     システムそのものに機能を追加する為のプラグイン。
;;;     必須。
;;;     基本的な機能も、これを用いて実装する。
;;;     しかし仕様の詳細はまだ未定。
;;; -- プラグインディレクトリ自体も、このシステムで管理する
;;; --- システム操作を誤るとシステムを破壊してしまう可能性が出るが、
;;;     それは通常のosと同じ、という事で……
;;; --- 一応、根源的なプラグインは、データツリーの外に置き、
;;;     そっちがあるならそっちの方を優先させる、という方針にする。
;;; - データ暗号化機能自体は必須にする事にした。
;;;   (つまり、暗号化keyを必要とする)
;;;   しかし、実装は後回し。
;;;   また、最終的に、デフォルトで有効にするかどうかは未定。
;;;   暗号化はblowfishを採用する予定。
;;;   (これも複数の暗号化方式を選択できるようにすべきか？)


;;; TODO: 基本セットとなる、データツリー生成ユーティリティコマンドを
;;;       配布物に含める必要がある？
;;;       (基本機能の一部がプラグイン化されている為、
;;;        プラグインとしてデータツリー配下に含める必要がある)


;;; TODO: MIME typeを決める。
;;; - Content-Type: application/as; data-type=moge
;;; -- application/as以外の場合は、そのまま出力する。


;;; TODO:
;;; - http://as.tir.jp/one に設置する。
;;; - プラグインシステムを実装する事。
;;; - ジャーナリングログが必要。
;;; -- これはsyslog転送のような、汎用の仕組みにすべきか？
;;; - データ暗号化があるのが好ましいが、必須ではない
;;; - PATH_INFOではなく、dnsのAAAAレコードを使うか？
;;; -- wikiならそれでもいいが、今回はカレンダー機能が軸になる為、
;;;    逆順になるのが好ましくないので、PATH_INFO使用とする。

;;; TODO: 一通り完成したら、再利用可能なように、各部分をモジュールに分解する。
;;; - content-treeを操作/レンダリングする部分
;;; -- rfc822型ファイル操作ユーティリティ
;;; - 各種プラグインシステム
;;; - アカウントマネージャ
;;; - その他

;;; TODO: データのメモリキャッシュの実装

;;; TODO: 同一アカウントに別々の接続元から同時にログインできない仕組みを
;;;       実装する必要がある


;;; ソースコードのモジュール構造:
;;; as.cgi : cgi用インターフェース
;;;          (httpdインターフェース等を作るかもしれないので分けとく)
;;; as.storage : データツリーにアクセスする部分をカプセル化
;;; 他は準備中


(define-module tir04.tmp.20070919.as.cgi
  (use gauche.parameter)

  (use rfc.822)
  (use file.util)
  (use util.list)
  (use dbm)
  (use text.html-lite)
  (use text.tree)
  (use www.cgi)
  (use tir04.cgi.util)
  (use tir04.cgi.ident)
  (use tir04.cgi.session.ident)
  (use tir04.dbm.util)

  (use srfi-19)
  (use gauche.sequence)
  (use srfi-1)

  ;(use wc-frame)
  ;(use tir04.util.calendar)


  (use tir04.tmp.20070919.as.storage)

  (export
    <as-cgi>
    as-cgi-execute
    ))
(select-module tir04.tmp.20070919.as.cgi)


(define-class <as-cgi> ()
  (
   ;; TODO: 情報定義スロットがとりあえず必要
   ;;       (とりあえず、秘密にすべき設定値はスロットで持たせる事)
   (information-keywords
     :accessor information-keywords-of
     :init-keyword :information-keywords
     :init-value '())
   (setting-keywords
     :accessor setting-keywords-of
     :init-keyword :setting-keywords
     :init-value '())
   (cgi-ident
     :accessor cgi-ident-of
     :init-keyword :cgi-ident
     :init-value #f)

   ;; private slot
   (csa
     :accessor csa-of
     :init-value #f)
   (as-storage
     :accessor as-storage-of
     :init-value #f)
   ))


(define-method information-of ((self <as-cgi>) keyword . opt-fallback)
  (apply get-keyword keyword (information-keywords-of self) opt-fallback))

(define-method setting-of ((self <as-cgi>) keyword . opt-fallback)
  (apply get-keyword keyword (setting-keywords-of self) opt-fallback))

(define-method initialize ((self <as-cgi>) initargs)
  (next-method)
  (unless (file-exists? (setting-of self :storage-dir))
    (make-directory* (setting-of self :storage-dir)))
  (set!
    (csa-of self)
    (make
      <cgi-session-ident>
      :dbm-type (setting-of self :dbm-type)
      :dbm-path (string-append
                  (setting-of self :storage-dir)
                  "/"
                  "csa")
      :expire-second (* 1 24 60 60)
      :cgi-ident (cgi-ident-of self)))
  (set!
    (as-storage-of self)
    (make
      <as-storage>
      :dbm-type (setting-of self :dbm-type)
      :storage-dir (setting-of self :storage-dir)
      ;; TODO: ↑サブディレクトリ掘るべき？
      ;; TODO: 他にどんな引数が必要？
      ))
  #t)




;;; ----



;; note: 必要なパラメータは以下の通り。
;; - アクセス者の識別情報(ident)
;; - 要求されたアクセス対象(path)
;; - 対象に対するコマンド及びその詳細(params)
;; 以下は、採用すべきか迷うパラメータ。
;; - 一時的な内部状態を保持する為のセッションバッファ(session)
;;   (これを採用すると、履歴的な状態遷移が崩れる)
;; これらはまとめて一つにしておく。
(define (make-req-info ident path params)
  (list ident path params))
(define (req-info->ident req-info)
  (car req-info))
(define (req-info->path req-info)
  (cadr req-info))
(define (req-info->params req-info)
  (caddr req-info))







(define (as:do-cmd self req-info c)
  ;; TODO: cでのディスパッチに失敗した時は、as:display-pathに移動させる？
  #f)

(define (as:display-path self req-info)
  ;; TODO: as.storageモジュールを使い、
  ;;       データツリーから、
  ;;       ident権限によって、
  ;;       pathに対応するデータを取り出す
  ;;       (権限が無い場合は#fか何かが返る？)
  ;; TODO: 返り値として、以下のような情報が必要になる。
  ;; - pathに対応する何かが存在しなかった
  ;;   (存在しないなら、クライアントに404を返す必要がある)
  ;; - pathに対応する何かは存在するが、アクセス権がなかった
  ;;   (403を返す必要がある)
  ;; - pathは実ファイルか否か
  ;;   (実ファイルなら、httpd風に動作させる)
  ;; - pathはディレクトリであるが、.contentが存在しなかった
  ;;   (？？？どういう挙動が良い？)
  ;; - pathはディレクトリであり、.contentが存在する
  ;;   (更に詳細なパターンに分岐する)
  ;; - .contentの実体はrfc822形式のファイルであるものとし、
  ;;   そのヘッダ部分のalistと、body部分の入った入力portが返されるべき。
  ;; 最後まで到達した場合は、ヘッダ部分を見て動作が行われる。
  ;; 基本的には、そのままcgi出力として返しても問題のない場合が多い。
  ;; - Location: ヘッダがあるなら、リダイレクトを行う。
  ;; - Content-Type: ヘッダがあるなら、それに対応した動作を行う。
  ;;   但し、application/as等が指定されていた場合は、
  ;;   そのまま出力はせずに、指定のフィルタを通してから結果を出力させる。
  ;;   (フィルタによっては、入力を完全に無視して、独自の出力結果を返すのもアリ)
  (let1 content-obj (path->entity
                      (as-storage-of self)
                      (req-info->ident req-info)
                      (req-info->path req-info))
    #f))


(define (req:cgi-get-parameter self req-info key . opt-args)
  (apply
    cgi-get-parameter
    (string-append
      (setting-of self :param-name-prefix "")
      "_"
      key)
    (req-info->params req-info)
    opt-args))

(define (as-cgi-execute:main self req-info)
  (let1 c (req:cgi-get-parameter self req-info "c")
    ;; 付加コマンドの有無によって、挙動を変化させる
    (if c
      (as:do-cmd self req-info c)
      (as:display-path self req-info))))


(define-method as-cgi-execute ((self <as-cgi>))
  (cgi-main
    (lambda (orig-params)
      (with-cgi-session-ident
        (csa-of self)
        orig-params
        (lambda (true-params ident-info session-parameter)
          (with-path-info-keylist
            (lambda (path-info-keylist)
              (let1 req-info (make-req-info
                               ident-info
                               path-info-keylist
                               true-params)
                (as-cgi-execute:main self req-info)))))))
    :on-error cgi-on-error/stack-trace ; for debug
    )
  0)


;;; --------


(provide "tir04/tmp/20070919/as/cgi")

