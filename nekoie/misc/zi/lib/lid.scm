;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; local id (lid) module
;;; - 「特定領域内id」モジュール

;;; 概要:
;;; このモジュールは、特定領域内にて、固有のidとして機能するオブジェクト及び
;;; そのオブジェクトのクラスを生成する為に機構を提供する。
;;; 要は「特定領域内でのみ通用する(gensym)」のようなものを提供する。
;;; このlidは、write/read invarianceが保たれている。
;;; - 「単に、特定prefixで始まるsymbolを使う」でも実用上の問題は無いと思うが、
;;;   万が一重複する可能性を排除したいので、このような仕組みになった。

;;; 使い方:
;;; (define-lid-class <lid-sample>
;;;   :gensym gensym-thunk
;;;   :getter getter-proc
;;;   :setter setter-proc
;;;   :initializer init-proc
;;;   )
;;; で、lidクラスを定義する。引数は全て省略可能。
;;; - :gensym には、(とりあえずこのクラス内で)重複しないシンボルを返す
;;;   thunkを設定する。省略時には、適当に日時等から、重複しないシンボルが
;;;   生成される。
;;; - :getter 及び :setter は、object-applyで
;;;   「lidインスタンスの実体」にアクセスする為のprocを設定する。
;;;   :getterの引数は自分自身。実体の取り出しに失敗した場合は、
;;;   エラー例外を投げてもよいし、適当なfallback値を返してもよい。
;;;   :setterの引数は自分自身と新しい値の二値。
;;;   これらのデフォルトは#f。
;;;   この時は、object-applyが実行されるとエラー例外を投げる。
;;;   (逆に言うと、object-apply手続きを使わないなら省略しても問題ない)
;;; - :initializer は、このクラスのインスタンスが生成された時に呼ばれる
;;;   procを設定する。これは主に、lidを生成すると同時に、ストレージ内に
;;;   空のエントリを生成したい時に使う。
;;;   (ストレージによっては、「エントリの挿入」と「エントリの更新」を
;;;    厳密に区別して操作する必要がある為に、この仕組みを用意した。
;;;    そうでないストレージであれば、この手続きは設定せずに、
;;;    普通にmakeしてからすぐに更新手続きを呼ぶだけでよい。)
;;;
;;; lidクラスが定義されたら、以下のようにしてlidインスタンスを生成する。
;;; (let1 lid (make <lid-sample>)
;;;   ...)
;;; このlidインスタンスはwrite/read invarianceが保たれている為、
;;; 直接、ストレージに保存したり、ストレージから読み込んだりする事が可能。
;;; (但し、読み込む前に、define-lid-classが実行されている必要がある。
;;;  この中でリーダーマクロ等が定義されている為。)
;;; この時の文字列表現は、この例でいくと、
;;; #,(<lid-sample> ???)
;;; となる(???のところには、uniqueなsymbolが入る)
;;;
;;; クラス定義時に:getter :setterを定義しているなら、以下のユーティリティ
;;; 手続きが使用可能になる。
;;; (lid) => lidに結び付けられた実体を取り出す(:getterを呼び出す)。
;;;          エントリが存在しない場合の処理は、:getter依存。
;;;          (エラー例外を投げてもいいし、適当なfallback値を返してもよい)
;;; (lid new-val) => lidに結び付けられた実体を更新する(:setterを呼び出す)。
;;;                  この「更新」の実操作については、:setter依存。
;;;                  (破壊的更新を行ってもいいし、new-val等の条件によって
;;;                   実際には何も更新をしないような挙動にしてもよい)


(define-module lid
  (use util.match)

  (export
    <lid-meta> ; 各lidのクラスを生成する為のメタクラス
    <lid-abstract> ; 生成された各lidの共通methodを提供する為の抽象クラス
    define-lid-class
    ))
(select-module lid)


;;; ----


;; fallback-gensym-thunk用の内部値
(define *internal-counter* 0)


;;; ----
;;; クラス、コンストラクタ、reader、writer


;; 各種の<lid>を生成する為のメタクラス
(define-class <lid-meta> (<class>)
  (
   (symbol :init-keyword :symbol ; readerで使うシンボル
           :init-form (error "must be need :symbol"))
   (gensym :init-keyword :gensym ; gensym的なthunkを登録。無くてもok
           :init-value #f)       ; (その場合はtimestamp等から生成される)
   (getter :init-keyword :getter ; dbm-get的なprocを登録。無くてもok
           :init-value #f)       ; (その場合はobject-applyは使用不可)
   (setter :init-keyword :setter ; dbm-set!的なprocを登録。無くてもok
           :init-value #f)       ; (その場合はobject-applyは使用不可)
   (initializer :init-keyword :initializer :init-value #f)
   ))


;; 生成された各lidの共通methodを提供する為の抽象クラス
(define-class <lid-abstract> ()
  ((key))) ; (gensym)相当のkeyの実体(uniqueなシンボル)
(define-method initialize ((self <lid-abstract>) initargs)
  (receive (k args) (if (null? initargs)
                      (values (generate-key self) '())
                      (values (car initargs) (cdr initargs)))
    (next-method self args)
    (slot-set! self 'key k)
    (let1 initializer (slot-ref (class-of self) 'initializer)
      (when initializer
        (initializer self)))))

(define-method write-object ((self <lid-abstract>) out)
  (format out "#,~s"
          (list
            (slot-ref (class-of self) 'symbol)
            (slot-ref self 'key))))

(define-method object-equal? ((obj1 <lid-abstract>) (obj2 <lid-abstract>))
  (equal?
    (slot-ref obj1 'key)
    (slot-ref obj2 'key)))

(define-method object-hash ((self <lid-abstract>))
  (hash (slot-ref self 'key)))


(define (fallback-gensym-thunk)
  (string->symbol
    (receive (epoch micro) (sys-gettimeofday)
      (let1 result (format "lid_~d_~d_~d_~d"
                           epoch
                           micro
                           (sys-getpid)
                           *internal-counter*)
        (set! *internal-counter* (+ 1 *internal-counter*))
        result))))

(define (generate-key self)
  (let1 gensym-thunk (or
                       (slot-ref (class-of self) 'gensym)
                       fallback-gensym-thunk)
    (gensym-thunk)))

#|
;; 以下のようなコードで、<lid>を生成する
(define-class <lid-sample> (<lid-abstract>)
  ()
  :metaclass <lid-meta>
  :symbol '<lid-sample>
  :gensym (lambda () ...)
  :getter (lambda (key fallback) ...)
  :setter (lambda (key val) ...)
  )
(define-reader-ctor '<lid-sample>
  (lambda (key)
    (make <lid-sample> key)))
;; そして、以下のようなコードで、lidを好きなだけ生成できる
(define lid (make <lid-sample>))
;; これをマクロ化する
(define-lid-class <lid-sample>
                  :gensym thunk
                  :getter proc1
                  :setter proc2
                  :initializer proc3
                  )
|#
(define-macro (define-lid-class symbol . keywords)
  ;; NB: keywordsは本来なら、evalする必要がある
  ;;     しかし、evalする必要があるのはkeywordsのvalue部だけで、
  ;;     その部分は展開後のdefine-classのキーワード部に
  ;;     直接展開されるので、問題無い筈
  (let-keywords keywords ((gensym #f)
                          (getter #f)
                          (setter #f)
                          (initializer #f)
                          )
    `(begin
       (define-class ,symbol (<lid-abstract>)
         ()
         :metaclass <lid-meta>
         :symbol ',symbol
         :gensym ,gensym
         :getter ,getter
         :setter ,setter
         :initializer ,initializer
         )
       (define-reader-ctor ',symbol
         (lambda (key)
           (make ,symbol key))))))



;;; ----
;;; ユーティリティ手続き


;;; object-apply
(define-method object-apply ((self <lid-abstract>))
  (let1 getter (slot-ref (class-of self) 'getter)
    (if getter
      (getter self)
      (error "has not getter" (class-of self) self))))
(define-method object-apply ((self <lid-abstract>) new-value)
  (let1 setter (slot-ref (class-of self) 'setter)
    (if setter
      (setter self new-value)
      (error "has not setter" (class-of self) self))))


;;; ----

(provide "lid")

