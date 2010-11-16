;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; cgi用のユーザの識別を行う為のクラスのベースクラス

;;; 概要:
;;; このモジュールは、cgiでユーザの識別を行う為のクラスを作成する為の
;;; 枠組みを提供する。
;;; 実際にそういうクラスを作りたい時は、
;;; このクラスをベースクラスに指定する事になる。
;;;
;;; このクラスを継承した各クラスは、ユーザの識別機能のみを提供する為、別途、
;;; セッションクラスを用意する必要がある(tir04.cgi.session)。
;;; または、セッションとは別に、
;;; 何らかの重要な副作用を持つ行動(データの削除や物品の購買等)を
;;; ユーザが行う際の単体確認用途としても利用可能。

;;; NB: ident-infoが得られる際に副作用が起こり得る為、
;;;     with-cgi-identに渡したprocがident-infoを受け取ったタイミングでは、
;;;     そのままhtmlを出力せずに、一旦リダイレクトを行うようにした方が良い。
;;;     (そうしないと、ブラウザのbackや履歴で再アクセスがあった時に問題になる)

;;; TODO: 上記の制約は分かりにくいので、次バージョンでは、
;;;       「ident-infoが取得できた時のproc」を別途指定するようにし、
;;;       そのprocはリダイレクトすべきurlを返す仕様とする、
;;;       みたいな感じの仕様にする。
;;;       (但し、この仕様そのものではなく、もう少し仕様を練る必要がある。)

#|
(define *cgi-ident*
  (make <cgi-ident-hoge>
        :internal-key-prefix "cgi-ident"
        :description-html ...
        :description-html-keywords '(...)
        :error-html-proc ...
        :error-html-keywords '(...)
        ...))

(cgi-main
  (lambda (orig-params)
    (with-cgi-ident
      *cgi-ident*
      orig-params ; この値によって、with-cgi-identが処理を横取りするかが決まる
      (lambda (true-params ident-info)
        ;; true-paramsには、:internal-key-prefixで始まるkeyが除去された
        ;; paramsが入る。
        ;; 認証が行われ、それが成功した場合のみ、ident-infoに値が入る。
        ;; ident-infoが#f以外だった場合、ユーザがログインボタンを押し、
        ;; with-cgi-identが横取りした処理内で認証処理が行われ、正しく認証された
        ;; という事なので、ident-infoがある場合はparams等のチェックよりも先に
        ;; ログイン後の処理へとディスパッチしなくてはならない。
        ;; また、外部認証の場合、ident-infoの取得は副作用を持つ可能性がある為、
        ;; ident-infoが取得された後は直接コンテンツを表示するのではなく、
        ;; 一旦リダイレクトする事が望ましい(ブラウザのhistory back対策として)。
        ;; 尚、もし認証に失敗した場合は、with-cgi-identが横取りした処理内で
        ;; エラーが表示される筈なので、ここでは気にしなくて良い。
        ;; ident-infoは、以下のようなkeywordsである。
        ;; '(
        ;;   :ident-type ???
        ;;   ;; 'formや'hatenaや'typekey等、認証タイプを特定するシンボル。
        ;;   ;; 基本的には、クラス名がそのまま使われる。
        ;;   :ident-path "..."
        ;;   ;; dbmのpath等、:ident-typeが更に複数の選択肢を持つ場合に、
        ;;   ;; その詳細が指定される。
        ;;   ;; 外部認証等で複数の選択肢が存在しない場合は空文字列。
        ;;   :ident-id #*"..."
        ;;   ;; :ident-idは、認証の結果得られる、uniqueなid文字列。
        ;;   ;; 認証タイプによっては不完全文字列が得られる可能性がある為、
        ;;   ;; 扱いには注意が必要。
        ;;   :uid "..."
        ;;   ;; :ident-typeと:ident-pathと:ident-idを文字列として結合し、
        ;;   ;; そのsha1 digestを取った結果をhexifyした文字列。
        ;;   ;; 複数の認証をまたいでもuniqueな文字列が得られる為、
        ;;   ;; 複数の認証タイプを許可する場合にもuniqueな値が得られる。
        ;;   ;; 通常は、この値をkeyとして利用すべき。
        ;;   ;; (ident-idをそのままkeyとして利用するのは色々と問題がある)
        ;;   ;; 尚、uniqueな事については、sha1の衝突レベルで安全が保証されて
        ;;   ;; いるものの、digestの取り方については秘密鍵のようなものは
        ;;   ;; 特に使っていない為、これをそのままユーザに見せるのは
        ;;   ;; やめておいた方が良い(brute forceで元のident-id等が判明できる)。
        ;;   :x-ident-info '(...)
        ;;   ;; その他の、認証タイプ毎に個別で得られる、各種の情報。
        ;;   ;; rfc.822形式のalistで提供される。
        ;;   ;; keyもvalも(不完全かも知れない)文字列。
        ;;   )
        ;; 認証ページへのurlを生成。
        (make-ident-url *cgi-ident* misc-params)
        ;; 認証ボタン生成
        (make-ident-form *cgi-ident* misc-params html-tree . keywords)
        ... ; 様々な処理を行う
        ))))
|#


(define-module tir04.cgi.ident
  (use srfi-1)
  (use srfi-13)
  (use text.html-lite)
  (use www.cgi)
  (use tir04.cgi.util)

  (export
    <cgi-ident>
    with-cgi-ident
    make-ident-url
    make-ident-form
    ))
(select-module tir04.cgi.ident)


(define (default-error-html-proc error-message)
  (html:p
    (text->inline-html error-message)))
(define *default-error-html-keywords*
  `(:encoding ,(x->string (gauche-character-encoding))
    :http-header '(:pragma "no-cache" :cache-control "private")
    :css-url "http://css.tir.jp/tir.css"
    :robots "NOINDEX,NOFOLLOW"
    :title "error"
    ))

(define-class <cgi-ident> ()
  (
   (internal-key-prefix
     ;; with-cgi-identで処理を横取りする為のcgiパラメータのkey文字列のprefix。
     ;; 実際の処理で使うkeyと被らないようにする事。
     :accessor internal-key-prefix-of
     :init-keyword :internal-key-prefix
     :init-value "tir04-cgi-ident")
   (description-html
     ;; この認証の説明文。
     ;; 但し、認証タイプによっては全く使われないかも知れない。
     :accessor description-html-of
     :init-keyword :description-html
     :init-value #f)
   (description-html-keywords
     ;; 前述の説明文を表示する際に使われるキーワード群。
     ;; キーワードについては、html-tree-makeと同じ。
     ;; description-htmlと同様に、認証タイプによっては全く使われない。
     :accessor description-html-keywords-of
     :init-keyword :description-html-keywords
     :init-value #f)
   (error-html-proc
     ;; エラー時のhtml片を生成するprocを指定する。
     ;; 引数としてエラー内容の説明文がplain textとして渡される。
     ;; (必要ならエスケープや整形を行う必要がある事に注意)
     :accessor error-html-proc-of
     :init-keyword :error-html-proc
     :init-value default-error-html-proc)
   (error-html-keywords
     ;; description-html-keywords-ofのエラー版。
     ;; cgi-tree-makeに渡される。
     :accessor error-html-keywords-of
     :init-keyword :error-html-keywords
     :init-value *default-error-html-keywords*)
   ))


(define-method initialize ((self <cgi-ident>) initargs)
  (next-method)
  )


(define (internal-key self suffix)
  (string-append (internal-key-prefix-of self) "-" suffix))

(define (dispatch-key-of self)
  (internal-key self "cmd"))

;; 外部へと提供するmethods
;; これらは子クラスは上書きする必要は無い。
(define-method with-cgi-ident ((self <cgi-ident>) cgi-params proc)
  ;; まず、cgi-paramsに(internal-key-prefix-of self)が含まれているか確認する
  (let ((dispatch-val (cgi-get-parameter (dispatch-key-of self)
                                         cgi-params))
        (true-params (remove
                       (lambda (key+vals)
                         (string-prefix? (internal-key-prefix-of self)
                                         (car key+vals)))
                       cgi-params)))
    (if (not dispatch-val)
      (proc true-params #f)
      (receive r (let/cc ident-info-cont
                   (values
                     #f
                     (dispatch-or-ident-info self
                                            dispatch-val
                                            cgi-params
                                            true-params
                                            ident-info-cont)))
        ;; ident-infoの取得に成功していたら、
        ;; (get-remove-params-key-list self)のkeyを更に消してからprocに渡す
        (if (car r)
          (proc
            (fold
              (lambda (keyname prev)
                (alist-delete keyname prev equal?))
              true-params
              (get-remove-params-key-list self))
            (car r))
          (cadr r)))))) ; ident-infoの取得はまだなので、生成されたhtmlを返す

(define-method make-ident-url ((self <cgi-ident>) misc-params)
  (append-params-to-url
    (self-url/path-info)
    (list*
      `(,(dispatch-key-of self) "login")
      misc-params)))

(define-method make-ident-form ((self <cgi-ident>)
                               misc-params
                               html-tree . keywords)
  (apply
    make-form
    (self-url/path-info)
    (list*
      `(,(dispatch-key-of self) "login")
      misc-params)
    (or html-tree (html:input
                    :type "submit"
                    :value "login"))
    keywords))


;; 子クラスが実装すべきmethods
(define-method dispatch-or-ident-info ((self <cgi-ident>) dispatch-val
                                                        cgi-params
                                                        fixed-params
                                                        ident-info-cont)
  ;; この手続きは、dispatch-valによって様々な挙動を行い、
  ;; 返り値としてcgi-mainに渡す形式のhtml-treeを返す。
  ;; 但し、目的であるident-infoの生成に成功した場合は、
  ;; html片は返さずに、(ident-info-cont ident-info)を呼び出し、そのまま
  ;; 継続を辿って終了する(継続なので、返り値を返す必要は無い)。
  ;;
  ;; cgi-paramsには、make-ident-urlやmake-ident-form時に指定した
  ;; misc-paramsが追加で渡される。
  ;; ここには、dispatch-valは既に削除されている為、純粋に
  ;; misc-paramsが渡ってきているものとして扱って良い。
  ;; (但し、ユーザが不正にパラメータを捏造している可能性は常にある)
  ;; これらはコールバック時に渡される引数として保存する事が望ましいが、
  ;; 他の用途で使っても良いし、ただ単に捨てても良い。
  ;;
  ;; dispatch-valは、最初にログインurl/ログインボタンが押された段階では、
  ;; "login"となるようになっている。
  ;; それ以外のディスパッチの為の文字列は任意に決めて構わない。
  ;; 尚、ディスパッチ判定に失敗した場合は、"login"と同様の処理を行うように
  ;; しておくのが望ましい。
  ;;
  ;; 生成されたhtmlページのhrefやformのhiddenパラメータに、
  ;; (dispatch-key-of self)のkeyが含まれている限りは、この
  ;; dispatch-or-ident-info手続きが呼び出されるので、
  ;; 生成されたhtmlページのhtmlやformのhiddenには必ず
  ;; (dispatch-key-of self)のkeyを含めるようにする事を忘れないように。
  (error "not implemented"))

(define-method get-remove-params-key-list ((self <cgi-ident>))
  ;; この手続きは、各認証モジュール内部で利用する必要のある、
  ;; 追加パラメータ名のlistを返す。
  ;;
  ;; 例えば、外部認証等で、ログイン名やパスワード情報を
  ;; cgi-paramsに含んでいる場合、これらのパラメータは削除しておきたい。
  ;; 通常、(internal-key-prefix-of self)で始まるkeyは暗黙の内に削除されるが、
  ;; 外部認証の場合、外部認証のシステムで固定のkeyで与えられる場合がある。
  ;; そういう時に、このログイン名やパスワード情報のkey名を
  ;; このlistとして追加すれば良い。
  ;; そうすると、cgi-paramsに、(dispatch-key-of self)が含まれ、
  ;; 認証が完了した段階でのみ、このlistに含まれているパラメータは
  ;; 削除された状態でtrue-paramsとして提供される。
  ;; (この段階以外では削除されない事に注意。)
  ;;
  ;; 特に何も無い場合は'()を返せば良い。
  '())


;; 子クラスの為の、ユーティリティ手続き
(define-method get-error-html ((self <cgi-ident>) error-message)
  (apply
    cgi-tree-make
    :body ((error-html-proc-of self) error-message)
    (error-html-keywords-of self)))




(provide "tir04/cgi/ident")
