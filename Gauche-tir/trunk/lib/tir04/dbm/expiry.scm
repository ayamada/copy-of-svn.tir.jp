;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; 概要:
;;; dbmに時間経過によるgcを付与する。
;;; 時間取得は(sys-time)による為、新たな環境依存を起こす事に注意する事。

;;; 保存形式:
;;; dbmのkeyとしてsidを、valとして'(data timestamp-epoch)を保存する。
;;; よって、内部構造的には、val自体は#fにはならない。
;;; また、この保存形式の為、元のdbm種別と互換性が無くなる。
;;; (つまり、混合して使えなくなる。)

;;; 実装:
;;; あらゆるdbmクラスに対応する為に、委譲による実装とする。

;;; ToDo: <fsdbm>の場合のみ、実際のファイルタイムスタンプを見る別実装とする。


(define-module tir04.dbm.expiry
  (use srfi-1)
  (use srfi-2)
  (extend dbm)

  (export
    <dbm-expiry>
    dbm-get! ; 値を取得すると同時にタイムスタンプを更新する
    dbm-touch! ; タイムスタンプのみを更新する。keyが存在しない場合は#fを返す
    dbm-gc! ; 強制的にgcを呼ぶ
    ))
(select-module tir04.dbm.expiry)


(define *not-found* (gensym))


(define-class <dbm-expiry-meta> (<dbm-meta>)
  ())


(define-class <dbm-expiry> (<dbm>)
  (
   ;; public
   (dbm-type
     :accessor dbm-type-of
     :init-keyword :dbm-type
     :init-value #f)
   (expire-second
     :accessor expire-second-of
     :init-keyword :expire-second
     :init-value (* 1 24 60 60))

   ;; private
   (dbm
     :accessor dbm-of
     :init-value #f)
   )
  :metaclass <dbm-expiry-meta>)


(define-method initialize ((self <dbm-expiry>) initargs)
  (next-method)
  (unless (dbm-type-of self)
    (error ":dbm-type must be required"))
  (set!
    (dbm-of self)
    (apply make (dbm-type-of self) (rewrite-to-initargs initargs)))
  #t)

(define (rewrite-to-initargs initargs)
  ;; 常に書き込みモードかつS式保存を行う必要がある
  (list*
    :rw-mode :write
    :value-convert #t
    (delete-keyword :dbm-type
                    (delete-keyword :rw-mode
                                    (delete-keyword :value-convert
                                                    initargs)))))


(define-method dbm-open ((self <dbm-expiry>))
  (next-method) ; <dbm>から継承したスロットを埋める為に必要。
  (dbm-open (dbm-of self)))


(define-method dbm-close ((self <dbm-expiry>))
  (next-method)
  (dbm-close (dbm-of self)))


(define-method dbm-closed? ((self <dbm-expiry>))
  (next-method)
  (dbm-closed? (dbm-of self)))


(define-method dbm-db-exists? ((class <dbm-expiry-meta>) name)
  (next-method)
  (dbm-db-exists? (dbm-of self) name))


(define-method dbm-db-remove ((class <dbm-expiry-meta>) name)
  (next-method)
  (dbm-db-remove (dbm-of self) name))


;;; 以下のmethodは、expire処理が追加で実行される。


(define-method dbm-put! ((self <dbm-expiry>) key val)
  (next-method)
  (begin0
    (dbm-put! (dbm-of self) key (list val (sys-time)))
    (gc-check! self)))


(define-method dbm-get ((self <dbm-expiry>) key . args)
  (next-method)
  (let1 val (if (null? args)
              (dbm-get (dbm-of self) key)
              (dbm-get (dbm-of self) key *not-found*))
    (if (eq? val *not-found*)
      (car args)
      (if (< (+ (cadr val) (expire-second-of self)) (sys-time))
        (begin
          (dbm-delete! self key)
          (car args))
        (car val)))))


(define-method dbm-exists? ((self <dbm-expiry>) key)
  (next-method)
  (not
    (eq? *not-found* (dbm-get (dbm-of self) key *not-found*))))


(define-method dbm-delete! ((self <dbm-expiry>) key)
  (next-method)
  (dbm-delete! (dbm-of self) key))


(define-method dbm-fold ((self <dbm-expiry>) proc seed)
  (next-method)
  (dbm-gc! self)
  (dbm-fold
    (dbm-of self)
    (lambda (key val prev)
      (proc key (car val) prev))
    seed))


;;; 以下のmethodは、<dbm-expiry>固有のもの。


(define-method dbm-get! ((self <dbm-expiry>) key . args)
  (if (null? args)
    (begin0
      (dbm-get self key args)
      (dbm-touch! self key))
    (let1 result (apply dbm-get self key *not-found*)
      (if (eq? result *not-found*)
        (car args)
        (begin
          (dbm-touch! self key)
          result)))))


(define-method dbm-touch! ((self <dbm-expiry>) key)
  ;; NB: このmethodのみ、タイムスタンプが切れていても、
  ;;     それを気にせずに更新する必要がある。
  ;;     (dbm-getしてから何らかの処理を行い、最後にdbm-touch!する場面が
  ;;      考えられ、その間に有効期限が切れてしまうと
  ;;      トランザクションが失われてしまう為、このような仕様とした。
  ;;      尚、dbm-put!の方はエントリが無い(gcで失われた)場合は
  ;;      新規に作成される為、特に問題は起こらない。)
  (and-let* ((val+timestamp (dbm-get (dbm-of self) key #f)))
    (dbm-put! self key (car val+timestamp))))


(define-method dbm-gc! ((self <dbm-expiry>))
  (let1 timeout-keys (filter
                       (lambda (x) x)
                       (dbm-map
                         (dbm-of self)
                         (lambda (key val)
                           (if (< (+ (cadr val) (expire-second-of self))
                                  (sys-time))
                             key
                             #f))))
    (for-each
      (lambda (key)
        (dbm-delete! (dbm-of self) key))
      timeout-keys)))




(define (gc-check! self)
  ;; ToDo: ここは改善の余地がある
  (when (= 0 (remainder (values-ref (sys-gettimeofday) 1) 16))
    (dbm-gc!)))

;;; --------


(provide "tir04/dbm/expiry")
