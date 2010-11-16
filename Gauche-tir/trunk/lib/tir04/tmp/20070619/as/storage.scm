;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; data storager of account scheduler

;;; TODO: 将来はプロセス分離して、socketによって
;;;       データを入出力できるようにする必要がある
;;;       (が、今はやらない)

;;; TODO: ファイルのキャッシングが必要。以下の仕様とする。
;;; - ファイルの内容を、ファイルのタイムスタンプ(statで取得できるもの)と
;;;   関連付けて記憶する。
;;; - 但し、普通に上記のようにしてしまうと、一秒以内に複数回更新された際に
;;;   実際のファイルとキャッシュ内容に違いが出てしまう可能性がある為、
;;;   ファイルに書き込む際に、元のファイルのタイムスタンプが既に現在の
;;;   epochと同じかそれより未来だった場合は、更新後、touchを使って、
;;;   元のファイルのタイムスタンプよりも1秒だけ進めた秒数での更新だったように
;;;   見せかける事。
;;; - 古いファイルのGCはあとで実装する事。今はGC無しで良い。
;;;   (しかし、あとでGC追加しやすいように実装する事)

;;; 仕様:
;;; - :storage-dir内に、特定書式のファイル群を収め、それを管理するモジュール
;;; - 基本的に、ディレクトリのみで構成される。
;;; -- ディレクトリ名に利用可能な文字は、A-Za-z0-9_のみとする
;;; - ディレクトリ内には、以下のドットファイルが設置される可能性がある。
;;; -- .content。該当pathのデータ実体がrfc822形式で保存される。
;;; -- .metadata。該当pathの詳細設定値及びグループ情報。書式未定。
;;;    旧仕様の.configと.groupと.privを統合したもの。
;;;    尚、ディレクトリ階層が深い場合は、再帰的に検索/適用される。
;;;    また、以下の情報要素を持つものとする。
;;; --- owner。該当ディレクトリの所有者。
;;;     特に指定されていない場合は、上位ディレクトリの所有者が継承される。
;;; --- group。
;;;     unixのgroupと同じ。
;;; --- permission。
;;;     unixのchmodと同じ。owner, group, otherに対して、
;;;     aclを設定する。
;;;     設定可能な項目は、r, w, a, t, Sとする。xは今のところ無し。
;;;     (下位ディレクトリへの参照の可不可にはrを使う)
;;;     aはappend。wの追記のみ版。
;;;     tはsticky bit。wの権限の内、自分自身が記述したもののみ編集可能とする。
;;;     (ownerはsticky bitの影響を受けず、常に全編集可能とする。
;;;      また、t時のheaderの編集はownerのみとする？？？)
;;; ---- contentのheaderとbodyで、aclを分けるべき？
;;;      bodyはいじらせたいが、headerはいじらせたくない、という場合がありそう。
;;; -- .metadataは、内部では以下のようなlistとして保持する。
;;;    '(; 仕様未定
;;;      )
;;; -- .content及び.metadataのファイル書式に異常がある場合は、
;;;    どうやって通知する？？？
;;; --- とりあえず仮に、stderrにエラー内容を流し、
;;;     返り値等は可能な限りそれっぽく捏造する
;;; - 基本的には、pathのnodeに対して、各種の操作コマンドを実行する事になる。
;;;   以下のコマンドが用意されていれば何とかなると思われる。
;;; -- ？？？？
;;; -- ？？？？
;;; -- ？？？？
;;; -- ？？？？
;;; - contentの内容は、Content-Typeによっては整形する必要があるが、
;;;   それはこのモジュールではない、別のモジュールが面倒を見るものとする。
;;; - pathのnodeに対する操作は、権限によって挙動を禁止すべき場合があるが、
;;;   その区分をどこで持つべきか。
;;; -- 最終的には、プロセスを分離し、socketによって操作する事になる為、
;;;    この時点で既に権限管理してよいと思われる。
;;;    つまり、各種の操作コマンドは、引数として
;;;    「誰のアカウントの権限で実行するか」を必要とするものとする。


;;; TODO: 今のところ、権限はread, write, appendの三つのみだが、
;;;       将来はより複雑な権限を追加する可能性が高いので、
;;;       追加可能な構造にしておく事


(define-module tir04.tmp.20070619.as.storage
  (use srfi-1)
  (use rfc.822)
  (use file.util)
  (use dbm)
  (use tir04.dbm.util)

  (export
    <as-storage>
    as-storage:open
    as-storage:close
    as-storage:fetch
    as-storage:store
    ))
(select-module tir04.tmp.20070619.as.storage)


(define-class <as-storage> ()
  (
   (storage-dir
     :accessor storage-dir-of
     :init-keyword :storage-dir
     :init-value #f)
   (dbm-type
     :accessor dbm-type-of
     :init-keyword :dbm-type
     :init-value #f)

   ;; internal slots
   (status
     :accessor status-of
     :init-value 'made)
   ))

(define-method initialize ((self <as-storage>) initargs)
  (next-method)
  (unless (file-exists? (storage-dir-of self))
    (make-directory* (storage-dir-of self)))
  (unless (dbm-type-of self)
    (errorf "~s must be need to :dbm-type" self))
  #t)



;;; ----


;; TODO: ロックすべきか考え、すべきなら実装する事
(define-method as-storage:open ((self <as-storage>) . opt-args)
  (when (eq? (status-of self) 'booted)
    (error "already booted" self))
  ;; TODO: あとで色々と追加する
  (set! (status-of self) 'booted)
  #t)

(define-method as-storage:close ((self <as-storage>) . opt-args)
  (unless (eq? (status-of self) 'booted)
    (error "not booted" self))
  ;; TODO: あとで色々と追加する
  (set! (status-of self) 'shutdowned)
  #t)


;; おそらく、HTTPに近い存在になるのではないかと思われるので、
;; ある程度HTTPを参考にして、入出力インターフェースを考える。
;; - .metadataの内部形式listを返す必要がある
;; - .contentの何かを返す必要がある
;; -- .contentのpathそのもの
;; -- .contentの全体が入った入力port
;; -- .contentのヘッダ部分+残りが入った入力port

;; TODO: このmethodが返す、返り値の型のパターンは？
;; - 正常時は、解決されたデータを多値で返す
;; - pathが途中から(または最初から)存在しなくなった場合は？？？
;; - .metadataで指定されるグループ属性によって、アクセス権がなかった
;; - その他のエラー時は？？？
;; このmethodは、何らかの問題を、エラー例外によって通知する為、
;; 適切にguardしておく事。
;; 返り値として、以下の値が多値で返される。
;; - マージされた.metadataの内部形式list
;; - .contentのヘッダ
;; - .contentのbodyが入った入力port
(define-method as-storage:fetch ((self <as-storage>) perm paths . opt-args)
  ;; TODO: キャッシングの仕組みが必要
  ;; とりあえずこの辺から作っていく事
  ;; まず、今のところ、permは見ずに、pathsだけを参照してどうにかするように
  ;; 実装してみる
  ;; まず、複数の/は一つにまとめる(fs仕様)
  (let1 true-paths (delete "" paths equal?)
    ;; 次に、true-pathsに不正な文字が含まれていない事を確認する
    ;; ……と思ったが、やっぱりこれは再帰的に一つずつ検証していく事にする
    (let next ((rest true-paths)
               ;; 現在位置を文字列で
               (current-path (storage-dir-of self))
               (merged-metadata '())
               )
      ;; まず、現在のディレクトリの.metadataを調べ、マージする
      ;; TODO: この部分で.metadataのキャッシングが必要になる
      ;;       (非常によくアクセスされる)
      (let1 new-merged-metadata '() ; TODO: 仕様未定。あとで
        ;; TODO: 権限が無い場合の返り値はどうやって返す？
        ;; TODO: .contentはキャッシングが必要/可能か？要検討。
        (if (null? rest)
          (let1 content-path (string-append current-path "/.content")
            ;; TODO: 更に処理は続く。あとで
            ...)
          (begin
            ;; まだ処理すべきpathが残っている。
            ;; TODO: .metadata以外に、子要素への継承が必要なものがあるなら、
            ;;       ここで行う事
            ;; TODO: (car rest)が不正でない事をチェックする必要がある
            (next (cdr rest)
                  (string-append current-path "/" (car rest))
                  new-merged-metadata)))))))

(define-method as-storage:store ((self <as-storage>) perm paths . opt-args)
  ;; TODO: キャッシングの仕組みが必要
  #f)


;;; ----


(provide "tir04/tmp/20070619/as/storage")

