;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; 概要:
;;; このモジュールは、セッション保持を行う為のクラスを作成する為の
;;; 枠組みを提供する。
;;; 実際にそういうクラスを作りたい時は、
;;; このクラスをベースクラスに指定する事になる。

;;; - セッションテーブルのエントリのkeyをsidと呼ぶ。
;;;   具体的には、sha1ダイジェスト値の16進文字列化された32文字の文字列になる。
;;; - セッションテーブルのエントリのvalueには、大抵の場合、あらゆる
;;;   read/write invarianceを満たすオブジェクトを保存する事が出来る。
;;; -- モジュールによっては、それ以外のオブジェクトを保存できる場合もある。

;;; このモジュールが提供するセッションの概要:
;;; - セッションとは、タイムアウトによるGCもどきが付いた、
;;;   (永続)テーブルのようなものとする。
;;; -- つまり、sidによってvalueを保存し、GCされない限りは取り出せるものとする。
;;; -- sidはcreate-session!によってのみ生成できる。
;;;    これはsha1ダイジェストを16進文字列化した32文字の文字列になる。
;;;    セキュリティ上の観点より、任意の文字列をkeyに使う手段は無いものとする。
;;; -- GCに回収されたsid/valueは、最初から存在しなかったものとして扱われる。
;;; --- 要するに、タイムアウトなのか最初から存在しなかったのか、
;;;     区別する手段は無いという事。

;;; 実際にセッションを使う場合の状況について:
;;; - セッションは通常、ログイン等を行い、ユーザの認証を行った後に、
;;;   その認証状態を保持しているという証の割符として利用される。
;;;   よって、(正しくvalueに対応する)sidを持つ=認証が行われた状態、となる為、
;;;   不正な手段によってセッションを発行できないようにする必要がある。
;;;   具体的には、ログイン完了時以外に新規セッションを作成できてはいけない。
;;; - また、このセッションの性格上、処理パターンは大体、以下の通りになる。
;;; -- sidを持たない。anonymousユーザとして処理を行う。
;;;    (必要最小限の権限しか与えない、または、ログイン以外の権限を全く与えない)
;;; -- ログイン手続きを行い、sidを受け取る最中のanonymousユーザ。
;;; -- sidを持っている。ログイン済ユーザとして処理を行う。
;;;    (必要に応じて権限を与える)
;;; - よって、大きく分けて、(正しくvalueに対応する)sid持ちと、それ以外の場合で
;;;   処理が大きく変わる為、以下のような方式で管理するのが良いと考えられる。

#|
(with-session
  session-obj
  sid ; 正しいsidまたは#f
  (lambda (session-data) ; sidが不正、または、セッションが存在しない場合は#f
    ;; 新規にセッションを作成する
    ;; (session-dataが#fの時のみ実行可能。それ以外の場合はエラー例外を投げる)
    (let1 new-sid (create-session! new-session-data)
      ;; セッションにデータを保存し、そのsidを受け取る
      ...)

    ;; セッションに保存したデータを更新する
    (update-session! new-session-data)

    ;; セッションを削除する(ログアウト処理等で使う)
    (remove-session!)

    ;; 尚、このprocが終了する際、
    ;; 内部でupdate-session!が行われていない場合であっても、
    ;; セッションへのアクセスが有効である事を示す為に、
    ;; 自動的にセッションのライフタイムが更新される。
    ...) ; このprocの返り値は、with-sessionの返り値になる
|#

;;; その他の仕様:
;;; - セッションに#fを保存しようとすると、セッションは削除される。
;;; -- しかし、セッションが存在しない場合はwith-sessionのprocには
;;;    #fが渡されるので、挙動的には問題は無い筈。
;;; - 全く別の系列に属する複数のセッションマネージャは同時に利用可能だが、
;;;   (但し、その場合にmethodを呼ぶ際には明示的に
;;;    セッションマネージャを指定する必要がある)
;;;   一つのセッションマネージャに含まれる複数のsidとその実データを
;;;   同時に利用する事はできないものとする。
;;; -- 複数同時に利用できる事は、悪用と混乱しかもたらさないように思える為。
;;; -- しかし、途中でsidを変更する機能(セッション内のデータは同一)は、
;;;    あってもいいかも知れない。
;;; --- とは言え、頻繁に使う機能ではないので、今は実装しない事にする。


(define-module tir04.session
  (use gauche.parameter)
  (use rfc.sha1)
  (use util.digest)
  (use math.mt-random)

  (export
    <session>
    with-session
    create-session!
    update-session!
    remove-session!
    ))
(select-module tir04.session)

;; with-session時の引数省略可能用パラメータ
(define session (make-parameter #f))


;; 子クラスは、このクラスを継承する事。

(define-class <session> ()
  (
   ;; sessionの寿命
   (expire-second
     :accessor expire-second-of
     :init-keyword :expire-second
     :init-value (* 1 24 60 60)) ; 仮に一日としておく
   ;; gcの為のカウンタ
   (gc-counter
     :accessor gc-counter-of
     :init-value 0)
   ;; 一時データを収めるスロット
   (now-sid
     :accessor now-sid-of
     :init-form (make-parameter #f))
   (now-status
     ;; with-session内で、セッションのデータが操作された際に変更されるフラグ。
     ;; 以下の値を取る。
     ;; - 'not-modified
     ;; - 'created
     ;; - 'updated
     ;; - 'removed
     :accessor now-status-of
     :init-form (make-parameter #f))
   ))

(define-method initialize ((self <session>) initargs)
  (next-method)
  ;; このクラス自体には、初期化が必要な処理は無し。
  ;; 子クラスは、もし必要なら、別途自分で用意する事。
  #t)

;; 子クラスは、以下のmethodを用意しなくてはならない。

(define-method inner:with-session-prehandler ((self <session>) sid)
  ;; このmethodは、with-session実行前に呼ばれる。
  ;; 何か処理が必要なら追加してよい。
  #f)

(define-method inner:with-session-posthandler ((self <session>) sid)
  ;; このmethodは、with-session実行後に呼ばれる。
  ;; 何か処理が必要なら追加してよい。
  ;; 尚、dynamic-windによって、エラー例外が投げられた際にも呼ばれる。
  ;; また、sidは、inner:with-session-prehandlerが呼ばれた時と同じとは限らない。
  ;; (create-session!やremove-session!で生成削除される可能性がある為)
  #f)

(define-method inner:gc-session! ((self <session>))
  ;; このmethodは、with-session実行時に時々実行される。
  ;; このmethodが呼ばれたら、セッションマネージャが管理しているセッションを
  ;; 一通りチェックし、タイムアウト状態になっているものがあれば破棄する事。
  ;; 尚、いちいちチェックしなくても勝手にgcされるような仕組みで
  ;; セッションを管理している場合は、この手続きは何も行わなくても構わない。
  #f)

(define-method inner:get-session ((self <session>) sid fallback)
  ;; このmethodは、セッションのデータストレージから、sidに対応するデータを
  ;; 取り出して返す事。
  ;; 該当するsidが存在しない場合や、取り出されようとしたsidのライフタイムの
  ;; 有効期限が切れていた場合は、返り値としてfallbackとして渡された値を返す事。
  ;; このmethodでは、データストレージからデータを取り出す際に
  ;; ライフタイムの更新をする必要は無い。
  ;; しかし、ライフタイムの更新を同時に行ってはいけない訳ではない。
  (error "not implemented"))

(define-method inner:touch-session! ((self <session>) sid)
  ;; このmethodは、指定されたsidのライフタイムの更新を行う事。
  ;; ライフタイムの実装は、ただ単に更新された日時を何らかの方法で
  ;; 保持するのが最も簡単だと思われる。
  ;; (有効期限切れかどうかを判定する際には、(expire-second-of self)が使える)
  ;; このmethodが呼び出された段階でライフタイムが切れていた場合は、
  ;; まだライフタイムが有効であったものとして、更新を行うのが望ましい。
  ;; もし、それが無理な場合は、inner:get-session時にもライフタイムの更新を
  ;; 行うように実装する必要がある。
  (error "not implemented"))

(define-method inner:put-session! ((self <session>) sid session-data)
  ;; このmethodは、指定されたsidとsession-dataをkeyとvalueとして、
  ;; ライフタイムを更新しつつ、データストレージに記憶する事。
  ;; 指定されたsidがまだ存在しない場合は、新たなエントリを生成して記憶する事。
  (error "not implemented"))

(define-method inner:delete-session! ((self <session>) sid)
  ;; このmethodは、指定されたsidに対応するエントリを削除する事。
  ;; 指定されたエントリが存在しない場合は、何も行わなくてよい。
  (error "not implemented"))







;;; 子クラスが表に提供すべきmethod群
;;; これらは、inner:*の各methodを呼び出す為、子が用意する必要は無い。
;;; 但し、モジュールがuseされたら、これらのmethodをexportする事。

(define-method with-session ((self <session>) sid proc)
  (parameterize ((session self)
                 ((now-sid-of self) sid)
                 ((now-status-of self) 'not-modified))
    (dynamic-wind
      (lambda ()
        (inner:with-session-prehandler self ((now-sid-of self))))
      (lambda ()
        (let1 session-data (and sid (inner:get-session self sid #f))
          (receive result (proc session-data)
            (cond
              ((eq? ((now-status-of self)) 'not-modified)
               ;; セッションが存在している時のみライフタイムを更新
               (when session-data
                 (inner:touch-session! self sid)))
              ((eq? ((now-status-of self)) 'created)
               #f) ; 今のところ、何かを行う必要性は無さそう
              ((eq? ((now-status-of self)) 'updated)
               #f) ; 今のところ、何かを行う必要性は無さそう
              ((eq? ((now-status-of self)) 'removed)
               #f) ; 今のところ、何かを行う必要性は無さそう
              (else
                (error "assertion")))
            (when (= 0 (mt-random-integer *mt* 2))
              (let1 gc-counter (+ (gc-counter-of self) 1)
                (if (< 16 gc-counter) ; 大体32回に一回ぐらいgcが呼ばれる
                  (begin
                    (inner:gc-session! self)
                    (set! (gc-counter-of self) 0))
                  (set! (gc-counter-of self) gc-counter))))
            (values result))))
      (lambda ()
        (inner:with-session-posthandler self ((now-sid-of self)))))))

(define-method create-session! ((self <session>) session-data)
  ;;(when ((now-sid-of self))
  ;;  (error "session is already exists"))
  ;; 既セッションの有無はチェックせずに、常に新規のセッションを作るものとする
  ;; (理由は、セッション削除を行っても、それはセッション内データの削除を
  ;;  行うだけなので、依然として外部ではsidを保持しており、
  ;;  セッション作成→セッション削除→セッション作成と行った際に、
  ;;  二回目のセッション作成の部分でsidを既に持っている為に、
  ;;  引っかかってしまう。)
  (let1 sid (make-sid self)
    (inner:put-session! self sid session-data)
    ((now-sid-of self) sid)
    ((now-status-of self) 'created)
    sid))
(define-method create-session! (session-data)
  (unless (session)
    (error "cannot found session-manager object"))
  (create-session! (session) session-data))

(define-method update-session! ((self <session>) session-data)
  (unless ((now-sid-of self))
    (error "have not sid"))
  (if session-data
    (begin
      (inner:put-session! self ((now-sid-of self)) session-data)
      ((now-status-of self) 'updated)
      #t)
    (remove-session! self))) ; #fが保存されようとしたなら削除する
(define-method update-session! (session-data)
  (unless (session)
    (error "cannot found session-manager object"))
  (update-session! (session) session-data))

(define-method remove-session! ((self <session>))
  (unless ((now-sid-of self))
    (error "have not sid"))
  (inner:delete-session! self ((now-sid-of self)))
  ((now-sid-of self) #f)
  ((now-status-of self) 'removed)
  #t)
(define-method remove-session! ()
  (unless (session)
    (error "cannot found session-manager object"))
  (remove-session! (session)))






;; sid生成用手続き等
(define *mt*
  (make <mersenne-twister>
        :seed (receive (epoch micro) (sys-gettimeofday)
                (receive (q r) (quotient&remainder epoch (+ (sys-getpid) 1))
                  (+ q r micro)))))

(define-method make-sid ((self <session>))
  (let1 new-sid (digest-hexify
                  (sha1-digest-string
                    (x->string
                      (mt-random-real0 *mt*))))
    ;; 現在のセッションテーブルに既にこのsidが無い事を確認する必要がある
    ;; ロックしている訳ではないので、race conditionが発生する可能性はあるが、
    ;; そもそもsha1 digestを取っているので、その可能性は充分低いので、諦める。
    ;; もし、inner:get-sessionのコストが結構大きいようなら、
    ;; この確認作業も省略して構わないかも知れない。
    ;(if (inner:get-session self new-sid #f)
    ;  (make-sid self)
    ;  new-sid)
    ;; とりあえず省略する事にした
    new-sid))





;;; --------


(provide "tir04/session")
