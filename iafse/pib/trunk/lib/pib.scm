;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; plain irc bot (pib) module

;;; basis on http://homepage3.nifty.com/oatu/gauche/try.html#ircbot

;;; RFC: http://www.haun.org/kent/lib/rfc1459-irc-ja.html
;;; どうも、最近のircサーバは拡張されていて、rfc1459よりも広い範囲で
;;; 設定が可能なようだが、このスクリプトではrfc1459準拠とする
;;; (nickにアンダーバーが使えない等の制約を含む)

;;; usage:
;;; (with-irc
;;;   irc-server-ip ; ircサーバのipまたはドメイン名を文字列で
;;;   irc-server-port ; ircサーバのportを数値で
;;;   :irc-server-encoding "iso-2022-jp" ; デフォルトは"utf-8"
;;;   :irc-server-pass "..." ; デフォルトは#f
;;;   :base-nick "..." ; nickが重複していた際には、自動的に変名される
;;;   :logging-handler (make-basic-irc-logger "log") ; デフォルトは#f
;;;   ;; ↑procを指定すると、eventの送受信時に、ロギングの為に、
;;;   ;; ↑このハンドラが呼ばれる。返り値は捨てられる。
;;;   ;; ↑このハンドラは送信/受信スレッドで実行される事に要注意！
;;;   ;; ↑(make-basic-irc-logger log-dir)で、簡易ロガー手続きが生成できる
;;;   (lambda ()
;;;     (irc-send! '(JOIN "#channel")) ; joinコマンドを実行
;;;     (let loop ()
;;;       (let1 event (irc-recv-event! 1)
;;;         ;; ↑timeout 1秒でmessage受信
;;;         ;;   (ここでtimeoutに#fを指定していないのは、#fを指定してしまうと
;;;         ;;    シグナルを処理できなくなってしまう為)
;;;         (when event
;;;           (write event)
;;;           (newline))
;;;         (loop)))))
;;; - irc-send-event! 及び irc-recv-event! で扱うircプロトコルのメッセージは、
;;;   「event」書式で与えられ、受け取る事になる。
;;;   「event」書式は以下のようなlistである。
;;;   '(timestamp mode prefix command . params)
;;; -- timestampは、送信/受信日時を特定する文字列。
;;;    送信前の(つまり、irc-send-event!に渡す)eventでは、これは#fにしておく。
;;; -- modeは、送信か受信かを特定するシンボル。'sendまたは'recv。
;;;    これも送信前のeventでは、#fにしておく。
;;; -- prefixは、ircサーバによって付与される、該当messageの関連者を示す文字列。
;;;    これも送信前のeventでは、#fにしておく。
;;; -- commandは、ircコマンドを示すシンボル、または、ircレスポンスを示す
;;;    最大三桁の数値。
;;; -- paramsは、commandに与えられる引数。
;;; - (irc-send-event! event)を実行する事で、eventをircサーバに送信できる。
;;;   送信はasyncに実行される為、送信が成功したかどうかに関わらず、
;;;   この手続きは常に#tを返す。
;;; - (irc-send! arg)は、(irc-send-event! (list* #f #f #f arg))と等価。
;;;   記述を短くする為のもの。
;;; - (irc-recv-event! timeout)を実行する事で、ircサーバから送られ、
;;;   既にキューにたまっているeventを一つ取得できる。
;;;   キューにeventが無く、timeout秒待っても何も送られてこなかった場合は、
;;;   この手続きは#fを返す。
;;;   timeoutはオプショナル引数で、デフォルトは0。
;;;   timeoutに#fを与える事で、メッセージが来るまで待つ事ができるが、
;;;   gauche.pthreadsの仕様より、待っている間はシグナル処理が行われない事に
;;;   注意しなくてはならない。
;;; - (get-recv-cv)を実行する事で、受信キュー監視用のcondition-variableが
;;;   取得できる。
;;;   このcvは、受信キューにeventが追加される毎にcondition-variable-broadcast!
;;;   が実行されるので、適当なmutexを用意し、mutex-unlockの引数にcvを渡す事で
;;;   irc-recv-event!を使わずに、受信キュー待ちを実現できる。
;;;   このcvを他の用途に流用し、複合条件の待機を行う事も可能。
;;;
;;; 注意点:
;;; - with-ircの内部からの継続/エラー脱出は可能ですが、
;;;   内部への継続を辿る事には対応していません。
;;; - irc-recv-event!やirc-send-event!でブロックしている最中は、
;;;   シグナルが処理されません。
;;;   (つまり、ctrl+cやSIGTERMで終了させられなくなるという事です)
;;;   可能なら、非ブロックモードにするか、短か目のtimeoutを設定して
;;;   繰り返した方が安全です。

;;; TODO: rfcの、コマンド数値を文字列に変換するテーブルを提供したいところ

;;; TODO: JOIN/NICK時のprefixを見る事で、自分自身を指す文字列を取れる。
;;;       スロットを追加してこれを保存しておき、ユーザが参照できるようにする？

;;; TODO: 終了処理時に、親スレッドがsocket-input-portをcloseしたりsocketを
;;;       終了させても、受信スレッドのselectが反応していないっぽい。
;;;       とりあえず、selectのままなら、そのままthread-terminate!しても
;;;       問題ない筈なので、このままにしておく。
;;;       が、どうにかして解決可能なら解決しておきたい。あとで。

;;; TODO: 可能なら、値が適正かどうかのチェックは、slotにset!する際に行いたい

;;; TODO: :logging-handlerが子スレッドで実行されるのはちょっと微妙。
;;;       何か良い方法は無いか？

;;; TODO: cthreads対応
;;; 対応手順は以下のようになる
;;; - cthreadsをuseするマクロをguard付きで実行(cthreadsが存在しない時の為に)
;;; - pibをパラメータ変数に保存させるようにする
;;; - thread, mutex, cv関連の手続きを呼んでいる部分を、以下のように修正する。
;;; -- 例えば、thread-start!なら、
;;;    (thread-wrapper 'thread-start! ...)
;;;    のようになる
;;; - thread-wrapper手続きを定義する
;;; -- (thread-type-of (pib))を見て、gauche.threadsの方か、cthreadsの方かの
;;;    どちらかを実行するようにする
;;; -- thread-select!のみ、cthreads固有の手続きなので、
;;;    gauche.threads時に特別扱いが必要になる事に注意

;;; 現在の仕様の問題点:
;;; - with-irc内から継続を保存して抜け、継続から再開した場合、
;;;   NICKとUSERは再実行されるが、JOINとMODEが再実行されない。
;;;   これを再実行されるようにする為には、以下の実装方法がある。
;;;   (しかし、どちらも、完璧な解決方法とは言い難い)
;;; -- join-channel!等の手続きを用意して、それを使ってもらうようにする
;;;    (事で、オブジェクト内でJOINしたチャンネル等を記憶しておき、
;;;     dynamic-windの開始thunkで、再度JOIN等するようにする)
;;; --- この方法の問題点は、ユーザが手でJOINコマンドを実行したりすると、
;;;     整合性が取れなくなる点にある。
;;; -- irc-send-event!内にて、コマンドを監視し、JOIN/PART/MODE時は
;;;    そのパラメータを記憶しておくようにする
;;;    (事で、オブジェクト内でJOINしたチャンネル等を記憶しておき、
;;;     dynamic-windの開始thunkで、再度JOIN等するようにする)
;;; --- この方法の利点は、ユーザが意識する必要が無い点にある。
;;;     また、change-nick!の実装もこれと共通化可能になる。
;;; --- この方法の問題点は、実行したコマンドがircサーバ側でエラーと判定され、
;;;     実際には反映されなかった場合でも、それに対処しづらいという点にある。
;;;     また、KICK等の、ircサーバ側からの操作にも対応する必要がある(多分)。
;;; - とりあえず、前者の解決方法は取らない事にする。
;;;   対応するとしたら後者の方法で。
;;; - 「継続による再突入を行ってはならない」が一番シンプルな解だが……
;;; -- とりあえず、今のところは、これでいく(実装不要な為)

;;; メモ:
;;; - 送信スレッドは、送信キューにデータが入ってくるのを待つ為に、
;;;   送信用cvでmutexをアンロックする(タイムアウト無し)
;;; - 親スレッド(及び受信スレッド)は、送信キューにデータを入れると同時に
;;;   送信用cvにcondition-variable-broadcast!を行う
;;; - 親スレッドは、受信キューにデータが入ってくるのを待つ為に、
;;;   受信用cvでmutexをアンロックする(タイムアウトは任意値)
;;; - 受信スレッドは、データを受信してキューに入れると同時に
;;;   受信用cvにcondition-variable-broadcast!を行う
;;;
;;; - 自分自身のnickの管理方法
;;; -- 送信スレッドがNICKコマンドを送った際に、base-nickを更新する
;;; -- 受信スレッドがNICKコマンドを受信した際に、そのprefixのnick部分が
;;;    current-nickと一致した場合、current-nickを更新する
;;; -- 但し、USERコマンド実行前は、NICKコマンドを送信しても、
;;;    それに対応するNICKコマンドは受信されない為、
;;;    特殊な扱いにする必要がある。
;;;    それを管理するのがirc-user-command-stateスロット。
;;; -- 送信スレッドがUSERコマンド実行前にNICKコマンドを送った際は、
;;;    base-nickだけではなく、current-nickも同時に更新するようにする

;;; NB: rfcでは、messageの長さが512byteのものは許容されるようだが、
;;;     一応念の為、このモジュールでは、512byteはアウトという事にする

;;; 引数としてチャンネル名またはnickを取るコマンド一覧:
;;; (末尾に(s)が付いているものは、コンマ区切りで複数指定可能なもの)
;;; - JOIN channel(s) ...
;;; - PART channel(s)
;;; - MODE channel-or-nick ...
;;; - TOPIC channel ...
;;; - INVITE nick channel
;;; - KICK channel(s) user(s) ...
;;; - PRIVMSG channel-or-nick(s) ...
;;; - NOTICE channel-or-nick ...
;;; - LIST, NAMES (引数無しが有り得る)
;;; - WHO, WHOIS, WHOWAS (特殊)

(define-module pib
  (use srfi-1)
  (use srfi-13)
  (use srfi-19)
  (use gauche.net)
  (use gauche.parameter)
  (use gauche.charconv)
  (use gauche.threads)
  (use gauche.selector)
  (use file.util)
  (use util.match)
  (use util.list)
  (use util.queue)
  (use text.tree)

  (export
    with-irc
    ;; 以下は、with-irc内部でのみ使用可能な手続き
    irc-send-event! ; event送信(512byteを越える分は切り捨てられる)
    irc-recv-event! ; event受信
    irc-send! ; 送信ユーティリティ手続き
    get-recv-cv ; 受信キューのcv取得
    get-current-nick ; 現在の自分自身のnick取得
    event->reply-to ; 受信eventから、返信先のchannelまたはnickを取り出す
    ;; 返信先が存在しない場合は#fを返す
    send-event-split-last-param
    ;; eventをmessageに変換しencoding変換した後に512byteを越えるなら、
    ;; (values 512byteに収まるように修正されたevent 切り捨てられた部分)
    ;; が返される。但し、切り捨て対象になるのは、eventのparamsの最後の要素のみ
    ;; とする(params最後の要素を全て捨てても512byteを越える場合はエラー例外)
    ;; 512byteを越えないなら、(values 元のevent #f)が返される
    ;; 尚、eventが不正な場合もエラー例外を投げる
    ;; この手続きはマルチバイト文字安全とする((gauche-character-encoding)想定)
    ;; この手続きはやや不正確で、ギリギリ限界よりもやや小さく切り取る事がある
    ;; (512byte以上になるポイントで切り取る事は無い)

    ;; event書式に対するアクセサ
    event->timestamp
    event->mode
    event->prefix
    event->command
    event->params

    ;; その他のユーティリティ手続き
    message->event ; 変換手続き(encoding変換は行わない点に注意)
    event->message ; 同上(オプショナル引数有り)
    valid-nick? ; nickがvalidかどうか確認する述語
    make-basic-irc-logger ; 簡易ロガー生成高階手続き
    make-thread/report-error ; スレッドがエラー例外で終了した際に
                             ; (standard-error-port)にスタックトレース表示
                             ; する以外はmake-threadと同じ

    ;; TODO: 他にも、rfcに近い部分は提供した方が良い
    ))
(select-module pib)



;; アクセサ
(define event->timestamp car)
(define event->mode cadr)
(define event->prefix caddr)
(define event->command cadddr)
(define event->params cddddr)


(define param:pib (make-parameter #f))

(define (irc-send! command+params . opt-sync)
  (apply irc-send-event! (list* #f #f #f command+params) opt-sync))
(define (irc-send-event! event . opt-sync)
  (unless (param:pib)
    (error "must be into with-irc"))
  ;; 後方互換性の為に、eventのcommandが文字列だった場合、この段階で
  ;; こっそりsymbolに変換しておく
  (let1 event-fixed (if (not (string? (event->command event)))
                      event
                      (list*
                        (car event)
                        (cadr event)
                        (caddr event)
                        (string->symbol (cadddr event))
                        (cddddr event)))
    (apply %irc-send-event! (param:pib) event-fixed opt-sync)))
(define (irc-recv-event! . opts)
  (unless (param:pib)
    (error "must be into with-irc"))
  (apply %irc-recv-event! (param:pib) opts))
(define (get-recv-cv)
  (unless (param:pib)
    (error "must be into with-irc"))
  (irc-recv-cv-of (param:pib)))
(define (get-current-nick)
  (unless (param:pib)
    (error "must be into with-irc"))
  (current-nick-of (param:pib)))


(define (event->reply-to event)
  (unless (param:pib)
    (error "must be into with-irc"))
  (let/cc return
    ;; 送信イベントの場合には無効とする
    (when (eq? 'send (event->mode event))
      (return #f))
    (let1 command (event->command event)
      ;; commandが数値のものは除外する
      (when (number? command)
        (return #f))
      ;; 特定のコマンドのみ処理を行う
      ;; 具体的に、処理を行う必要があるコマンドは以下の通り
      ;; KICK PRIVMSG NOTICE MODE INVITE
      ;; この内、INVITEのみ特殊で、(car params)ではなく、
      ;; (cadr params)が返信先になる(carの方は自分自身になる為)
      ;; また、ここではコンマ区切りの複数指定の事は考慮しなくても良い
      ;; (ほとんどの複数指定可能コマンドは送信イベント時のみで、
      ;;  受信イベントで来る事は無い筈)
      (let1 params (event->params event)
        (case command
          ((KICK PRIVMSG NOTICE MODE) #t) ; 続行
          ((INVITE) (return (cadr params)))
          (else (return #f)))
        (let1 target (car params)
          ;; targetがチャンネル名書式(#または&はじまり)なら、
          ;; このtargetが返信先になる
          (when (#/^(?:\#|\&)/ target)
            (return target))
          ;; このeventは自分宛てなので、prefixから返信先を調査する
          ;; prefixから、このmessageの送信元を割り出し、それを返す
          ;; prefixの書式は、"nick!~username@hostname"
          ;; よって、!より前の部分を取り出すだけで良い
          (and-let* ((prefix (event->prefix event))
                     (matched (#/\!/ prefix)))
            (matched 'before)))))))



;; NB: ircサーバで使われるencodingはutf-8またはjisが一般的で、
;;     これらを考慮して安全に特定サイズで切り取るのは微妙に困難。
;;     ここでは、正確さよりも速度を優先する。
(define (send-event-split-last-param event-orig)
  (define (conv-enc str)
    (ces-convert str
                 (gauche-character-encoding)
                 (irc-server-encoding-of (param:pib))))
  (define (event->converted-size event)
    (string-size (conv-enc (event->message event #t))))
  (define (get-size-of-converted-last-param last-param)
    (string-size (conv-enc last-param)))
  (define (find-suitable-index require-size str)
    (define (get-size idx)
      (get-size-of-converted-last-param (string-take str idx)))

    ;; 変換時にどれぐらいサイズが変化するかを簡単に求められない為、
    ;; 二分法を使って地道に求めていく
    ;; TODO: counterの初期値は、require-sizeとstrのlengthの二分比で求めるべき
    (let retry ((counter 8) ; 処理回数
                (start-idx 0)
                (end-idx (- (string-length str) 1)))
      ;; 規定回数以上処理したら、start-idxを返す
      (if (not (positive? counter))
        start-idx
        (let1 middle-idx (quotient (+ start-idx end-idx) 2)
          (cond
            ((= start-idx middle-idx) start-idx)
            ((= end-idx middle-idx) middle-idx)
            (else
              ;; middle-idxで切った際の変換結果が、要求されたsizeよりも
              ;; 大きいか小さいかで次の判定を変更する。
              ;; (同じ時は、idxが小さくなる方を選ぶ事にする)
              (let* ((middle-size (get-size middle-idx))
                     (next-start/end-idx (if (< middle-size require-size)
                                           (list middle-idx end-idx)
                                           (list start-idx middle-idx))))
                (apply retry (- counter 1) next-start/end-idx))))))))

  (unless (param:pib)
    (error "must be into with-irc"))
  (let/cc return
    ;; まず最初に、512byteを越えているかの簡単なチェックを行う
    (let1 converted-size-orig (event->converted-size event-orig)
      ;; 越えていないなら、そのまま終了
      (when (< converted-size-orig 512)
        (return event-orig #f))
      ;; 越えているなら、越えている量に応じて処理を行う必要がある
      ;; まず、必要な値を先に求めておく
      (let* ((event-without-last-param (drop-right event-orig 1))
             (params-orig (event->params event-orig))
             (last-param (if (null? params-orig)
                           (error "params not found, but over 512 byte")
                           (last params-orig)))
             (least-converted-size (event->converted-size
                                     `(,@event-without-last-param "")))
             (level-value (- 512 least-converted-size))
             )
        (when (<= 512 least-converted-size)
          ;; TODO: エラー内容の文章が変なので直す事
          (error "too big without last-param"))
        ;; あとは、last-paramのsizeがlevel-value未満になるギリギリのsizeを
        ;; 求めればよい
        (let* ((result-index (find-suitable-index level-value
                                                  last-param))
               (result-event
                 `(,@event-without-last-param ,(string-take last-param
                                                            result-index)))
               (result-remainder (string-drop last-param result-index))
               )
          ;; 念の為、最終チェックを行う
          (when (<= 512 (event->converted-size result-event))
            (error "assertion occurred" event-orig))
          (values result-event result-remainder))))))


(define-syntax ignore-error
  (syntax-rules ()
    ((ignore-error fallback . bodies)
     (guard (e (else fallback)) . bodies))))


(define-class <pib> ()
  (
   ;; 設定に関するスロット
   (irc-server-ip
     :accessor irc-server-ip-of
     :init-keyword :irc-server-ip
     :init-form (error "must be need irc-server-ip"))
   (irc-server-port
     :accessor irc-server-port-of
     :init-keyword :irc-server-port
     :init-form (error "must be need irc-server-port"))
   (irc-server-encoding
     :accessor irc-server-encoding-of
     :init-keyword :irc-server-encoding
     :init-value "utf-8")
   (irc-server-pass
     :accessor irc-server-pass-of
     :init-keyword :irc-server-pass
     :init-value #f)
   (thread-type
     :accessor thread-type-of
     :init-keyword :thread-type
     :init-value 'gauche.threads)
   (base-nick
     :accessor base-nick-of
     :init-keyword :base-nick
     :init-value "pib")
   (username
     :accessor username-of
     :init-keyword :username
     :init-value #f)
   (realname
     :accessor realname-of
     :init-keyword :realname
     :init-value #f)
   (main-thunk
     :accessor main-thunk-of
     :init-keyword :main-thunk
     :init-form (error "must be need main-thunk"))
   (flood-protection-interval
     :accessor flood-protection-interval-of
     :init-keyword :flood-protection-interval
     :init-value 900000)
   (logging-handler
     :accessor logging-handler-of
     :init-keyword :logging-handler
     :init-value #f)
   ;; 内部状態スロット(全て、あとで設定される)
   (current-nick
     :accessor current-nick-of)
   (irc-socket
     :accessor irc-socket-of)
   (irc-input-port
     :accessor irc-input-port-of)
   (irc-output-port
     :accessor irc-output-port-of)
   (irc-recv-thread
     :accessor irc-recv-thread-of)
   (irc-send-thread
     :accessor irc-send-thread-of)
   (irc-pong-thread
     :accessor irc-pong-thread-of)
   (irc-recv-queue
     :accessor irc-recv-queue-of)
   (irc-send-queue
     :accessor irc-send-queue-of)
   (irc-recv-queue-mutex
     :accessor irc-recv-queue-mutex-of)
   (irc-send-queue-mutex
     :accessor irc-send-queue-mutex-of)
   (irc-recv-cv
     :accessor irc-recv-cv-of)
   (irc-send-cv
     :accessor irc-send-cv-of)
   (irc-send-laststatus
     :accessor irc-send-laststatus-of)
   (irc-send-last-microsec
     :accessor irc-send-last-microsec-of)
   (irc-logger-mutex
     :accessor irc-logger-mutex-of)
   (irc-registered?
     :accessor irc-registered?-of)
   (irc-finished?
     :accessor irc-finished?-of)
   (pong-target
     :accessor pong-target-of)
   ))

(define-method initialize ((self <pib>) initargs)
  (next-method)
  #t)


;; eventは、以下のようなlistとする。
;; '(timestamp mode prefix command . params)
;; - timestampは日時を特定できる文字列。送信時には無視される
;; - modeは'sendまたは'recvのシンボル。送信時には無視される
;; - prefixは、このmessageのsenderを特定できる文字列。送信時には無視される
;; - commandは、'PRIVMSG等のシンボル、または三桁の数値
;; - paramsは、規定の書式を満たす文字列のlist(書式については、rfc参照)
(define-method %irc-send-event! ((self <pib>) event . opt-sync)
  ;; - opt-syncが#fなら、送信はasyncに実行され、送信が成功したかは分からない
  ;;   (デフォルトの動作)
  ;; - opt-syncが#tなら、送信はsyncに実行され、返り値として送信が成功したかが
  ;;   真偽値として返ってくるが、送信が完全に完了するまで待たされる
  ;;   (尚、これは「送信」が成功したかどうかであり、「コマンド実行」が
  ;;    成功したかどうかではない点に注意する事。そして「送信」が失敗する原因は
  ;;    通常、通信断以外には存在しない)
  ;;   このチェックの実装方法は未定。
  ;;   (ちゃんと実装しようとすると、かなりややっこしい。
  ;;    race condition時に無限ループする可能性が残る)
  (irc-send-enqueue! self event)
  (if (get-optional opt-sync #f)
    (error "not implement yet") ; TODO: あとで実装する
    #t))
(define-method %irc-recv-event! ((self <pib>) . opts)
  (let-optionals* opts ((timeout 0))
    (let loop ()
      (mutex-lock! (irc-recv-queue-mutex-of self))
      (let1 event (irc-recv-dequeue! self)
        (if (or event (equal? timeout 0))
          ;; キューに値が入ってきた or timeoutが0だった。
          ;; 処理して普通にアンロックして終了
          (begin
            (mutex-unlock! (irc-recv-queue-mutex-of self))
            event)
          ;; eventは#fだった。つまり、キューは空だった
          (let1 start-usec (gettimeofday-usec)
            ;; 受信スレッドが既にeofを受け取って終了していたなら、
            ;; ここでエラー例外を投げる必要がある
            (when (irc-finished?-of self)
              (mutex-unlock! (irc-recv-queue-mutex-of self))
              (error "irc socket already closed"))
            ;; キューが空なので、タイムアウト有りでcvシグナルを待つ
            (if (not (mutex-unlock! (irc-recv-queue-mutex-of self)
                                    (irc-recv-cv-of self)
                                    timeout))
              ;; タイムアウトした
              #f
              ;; cvシグナル受信。キューチェックからやり直す
              ;; 但し、シグナルが受信キューにアイテムが入った以外の通知の
              ;; 可能性もある為、それに備えて、
              ;; timeoutを待った時間に応じて減らす
              ;; (これはtimeoutが1秒単位の為不正確だが、大体合ってればokとする)
              (begin
                (when timeout
                  (let* ((go-on-usec (- (gettimeofday-usec) start-usec))
                         (go-on-sec (x->integer go-on-usec))
                         (delta-timeout (- timeout go-on-usec))
                         (new-timeout (if (positive? delta-timeout)
                                        delta-timeout
                                        0)))
                    (set! timeout new-timeout)))
                (loop)))))))))



;; NICKに失敗した際に、次に試行するNICKを生成する手続き
(define (generate-next-nick nick)
  (define (nick-chop nick)
    ;; マルチバイト文字が存在し得る為、一文字ずつ切り詰めていく必要がある
    (if (< *max-nick-length* (string-size nick))
      (nick-chop (string-drop-right nick 1))
      nick))
  (define (increment-nick nick)
    (define (increment-rnc rnc)
      (if (null? rnc)
        '() ; 本来なら、一文字追加する必要があるが、incrementの性質上、
            ; これが実行され得るのは「}}}}}}}}}」から「----------」になる時
            ; のみなので、これでokとする
        (let* ((target-char (car rnc))
               (remains (cdr rnc))
               (target-char-idx (hash-table-get
                                  *nickchar->nicknum* target-char -1))
               (next-idx (+ target-char-idx 1))
               (carry-flag (not (zero? (quotient next-idx *nicknum-max*))))
               (next-idx-true (remainder next-idx *nicknum-max*))
               (next-char (vector-ref *chars-can-use-nick* next-idx-true))
               )
          (cons
            next-char
            (if carry-flag
              (increment-rnc remains)
              remains)))))

    (let* ((reverse-nick-chars (reverse (string->list nick)))
           (incremented (increment-rnc reverse-nick-chars)))
      (list->string (reverse incremented))))

  (let/cc return
    ;; nickが9byteに達していないなら、末尾に一文字追加するのみでok
    (when (< (string-size nick) *max-nick-length*)
      (return
        (string-append nick (string (vector-ref *chars-can-use-nick* 0)))))
    ;; nickが9byteより長いなら、まず先に9byteに切り詰める
    (let1 chopped-nick (nick-chop nick)
      ;; nickをインクリメントする
      (let1 result-nick (nick-chop (increment-nick chopped-nick))
        ;; 新しいnickはvalid-nick?に通す必要は無い
        ;; (nickにrfc非準拠文字が混ざっている場合、それは
        ;;  ユーザが「分かっていて」やっている筈なので)
        ;; が、「nickの一文字目は<letter>である」という条件だけは
        ;; 厳守させる事にする
        ;; (increment-nickの仕様上、これが守られていず、しかも
        ;;  ircサーバ側が厳守していた場合、延々とNICKの変更が
        ;;  行われてしまう事になる為)
        (if (char-alphabetic? (string-ref result-nick 0))
          result-nick
          (nick-chop
            (string-append "A" result-nick)))))))

(define *max-nick-length* 9)
(define *chars-can-use-nick*
  (list->vector
    (string->list
      (string-append
        "-"
        "0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "[\\]^`"
        "abcdefghijklmnopqrstuvwxyz"
        "{}"
        ))))
#|
'a' ... 'z' | 'A' ... 'Z'
'0' ... '9'
'-' | '[' | ']' | '\' | '`' | '^' | '{' | '}'
|#
(define *nicknum-max*
  (vector-length *chars-can-use-nick*))
(define *nickchar->nicknum*
  (let1 table (make-hash-table 'eq?)
    (guard (e (else table))
      (let loop ((idx 0))
        (hash-table-put! table (vector-ref *chars-can-use-nick* idx) idx)
        (loop (+ 1 idx))))))


#|
<message>  ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
<prefix>   ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
<command>  ::= <letter> { <letter> } | <number> <number> <number>
<SPACE>    ::= ' ' { ' ' }
<params>   ::= <SPACE> [ ':' <trailing> | <middle> <params> ]

<middle>   ::= <先頭が':'ではなく,SPACE,NUL,CR,CFを含まない、空でないオクテットの列>
<trailing> ::= <SPACE,NUL,CR,CFを含まないオクテットの列(空のオクッテトの列も可)>
<crlf>     ::= CR LF
|#
;; ircのmessage(一行の文字列)を受け取り、それを解析し、
;; S式にして返す手続き
;; 正規表現については、以下を参照
;; http://www.haun.org/kent/lib/rfc1459-irc-ja.html#c2.3.1
;; messageが不正な場合はエラー例外を投げる。
(define (message->event message . opt-send?)
  (define (parse-params params)
    (let* ((m:params (or
                       (#/^ / params)
                       (error "invalid params format" message params)))
           (params-right (m:params 'after)))
      (cond
        ((string=? params-right "") '())
        ((#/^\:/ params-right) => (lambda (m)
                                    (list (m 'after))))
        ((not (#/ / params-right)) (list params-right))
        (else
          (let* ((m:params2 (or
                              (#/^(.+?)( .*)$/ params-right)
                              (error "invalid params format"
                                     message params-right)))
                 (middle (m:params2 1))
                 (next-params (m:params2 2))
                 )
            (cons
              middle
              (parse-params next-params)))))))

  (let* ((message-chomp (string-trim-right message #[\r\n]))
         (m:message (or
                      (#/^(?:\:(.*?) )?(\w+)( .*)$/ message-chomp)
                      (error "invalid message format" message-chomp)))
         (prefix (m:message 1)) ; or #f
         (command (m:message 2))
         (params (m:message 3))
         (prefix-true prefix)
         (command-true (if (#/^\d\d\d$/ command)
                         (x->number command)
                         (string->symbol command)))
         (params-true (parse-params params))
         (mode (if (get-optional opt-send? #f) 'send 'recv))
         )
    (list* (date->string (current-date))
           mode
           prefix-true
           command-true
           params-true)))

;; event形式(S式)のデータをircのmessage書式の文字列に変換する。
;; eventが不正な形式だった場合はエラー例外を投げる。
(define (event->message event . opt-sendmode)
  (match-let1 (timestamp mode prefix command . params) event
    ;; - timestampは#f、または日時を示す文字列
    ;; - modeは'recvまたは'sendのシンボル、または#f
    ;; - prefixは#f、または何者かを特定する為の文字列
    ;; - commandはシンボル、または(最大三桁の)数値
    ;; - paramsは、'()、または規定の制約を守った文字列のlist
    (let* ((sendmode (get-optional opt-sendmode #f))
           (prefix-for-insert (if sendmode '() (list prefix)))
           (command-true (if (integer? command)
                           (format "~3,'0d" command)
                           (symbol->string command)))
           (params-middles (if (null? params)
                             '()
                             (drop-right params 1)))
           (params-trailing (if (null? params)
                              '()
                              (list
                                (string-append ":" (last params)))))
           )
      (tree->string
        (list
          (intersperse " " `(,@prefix-for-insert
                              ,command-true
                              ,@params-middles
                              ,@params-trailing))
          "\r\n")))))


#|
<nick>       ::= <letter> { <letter> | <number> | <special> }
<letter>     ::= 'a' ... 'z' | 'A' ... 'Z'
<number>     ::= '0' ... '9'
<special>    ::= '-' | '[' | ']' | '\' | '`' | '^' | '{' | '}'
|#
(define (valid-nick? nick)
  (let1 nick-list (string->list nick)
    (and
      ;; 長さチェック
      (<= 1 (length nick-list) *max-nick-length*)
      ;; 最初の一文字目だけletter固定
      (char-alphabetic? (car nick-list))
      ;; 他の文字をチェック
      (every
        (lambda (char)
          (guard (e (else #f))
            (hash-table-get *nickchar->nicknum* char)))
        (cdr nick-list))
      ;; 全部に通ったので#tを返す
      #t)))


(define-method %irc-main ((self <pib>))
  ;; ここに来た段階で、とりあえずircサーバに接続した状態になっている
  ;; あとは、main-thunkを実行して、返り値が戻る(またはエラーになる)のを待つだけ
  ((main-thunk-of self)))


;; キューが空の時は#fを返す(キューに#fが入っている事は無いものとする)
(define-method irc-send-dequeue! ((self <pib>))
  (let1 state (mutex-state (irc-send-queue-mutex-of self))
    (unless (eq? state (current-thread))
      (error "assertion failed: irc-send-queue-mutex is not locked by the caller:" state))
    (if (queue-empty? (irc-send-queue-of self))
        #f
        (dequeue! (irc-send-queue-of self)))))
(define-method irc-recv-dequeue! ((self <pib>))
  (let1 state (mutex-state (irc-recv-queue-mutex-of self))
    (unless (eq? state (current-thread))
      (error "assertion failed: irc-recv-queue-mutex is not locked by the caller:" state))
    (if (queue-empty? (irc-recv-queue-of self))
        #f
        (dequeue! (irc-recv-queue-of self)))))

(define-method irc-send-enqueue! ((self <pib>) event)
  ;; 送信キューは、この時点で一旦変換を行い、エラーが出ない事を確認する
  (event->message event #t)
  ;; キューにeventをつっこむ
  (with-locking-mutex
    (irc-send-queue-mutex-of self)
    (lambda ()
      (enqueue! (irc-send-queue-of self) event)))
  (condition-variable-broadcast! (irc-send-cv-of self)))
(define-method irc-recv-enqueue! ((self <pib>) event)
  ;; 受信キューは、特にチェックを行う必要は無い
  (with-locking-mutex
    (irc-recv-queue-mutex-of self)
    (lambda ()
      (enqueue! (irc-recv-queue-of self) event)))
  (condition-variable-broadcast! (irc-recv-cv-of self)))


;; この手続きの仕様等:
;; - socket自身の切断などは親スレッドまたは接続先サーバが行う為、
;;   portに対する送受信時にエラー例外が発生したなら、それを確認し、
;;   もしそうなら、エラーではなく接続断としてキューにイベントを入れる事。
;;   そして、その原因が親スレッドであっても接続先サーバであっても、
;;   以降まともに送受信する事は出来ない筈なので、スレッドは
;;   自分自身を終了させる事。
(define-method %irc-recv-thread ((self <pib>))
  (define (fetch-event)
    ;; eventはeof、エラー、受信データ(S式に変換済)のどれか
    (guard (e
             ;; エラーがportがcloseされている事に起因するなら、
             ;; 代わりにeofを返す
             ((condition-has-type? e <io-closed-error>) (read-from-string ""))
             ;; その他のioエラーも、一律でeofを返す事にする(仮)
             ((condition-has-type? e <io-error>) (read-from-string ""))
             ;; それ以外の場合は、そのままエラーオブジェクトを返す
             ;; (messageの途中で通信断が発生した場合は、
             ;;  message->eventがエラーオブジェクトを返す(または、
             ;;  たまたまエラーにならずに正規の(不完全な)eventが返す)のを
             ;;  受け取り、それがキューに入った後、
             ;;  引き続いてeofがキューに入る事になる)
             (else e))
      (let1 message (read-line (irc-input-port-of self))
        (if (eof-object? message)
          message ; eof
          (message->event ; すぐにevent変換する
            (string-incomplete->complete
              (ces-convert message
                           (irc-server-encoding-of self)
                           (gauche-character-encoding))
              :omit))))))

  ;; このselectorはportが読み取れるまで待つ為だけのものなので、
  ;; ハンドラ自体は何もしなくてもよい
  (let1 selector (make <selector>)
    (selector-add! selector (irc-input-port-of self) (lambda (p f) #t) '(r x))
    (let loop ((exit? #f))
      ;; 今受信可能なデータは全て受信
      (with-port-locking
        (irc-input-port-of self)
        (lambda ()
          (let next ()
            (if (not (byte-ready? (irc-input-port-of self)))
              #f
              (let1 event (fetch-event)
                ;; eventがPINGや433の場合、キューには保存せずに、
                ;; ここで自動的に応答を行う必要がある
                ;; (尚、受信による自動応答時は、laststatusは変更しない事)
                (logging self event) ; ログは全てのeventに対して取る
                (cond
                  ((eof-object? event) ; eof
                   (set! (irc-finished?-of self) #t)
                   (irc-recv-enqueue! self event)
                   (set! exit? #t)) ; 終了
                  ((condition? event) ; エラーオブジェクト
                   (irc-recv-enqueue! self event)
                   (next)) ; 続行
                  (else ; 通常message
                    (when (recv-update-pib! self event)
                      (irc-recv-enqueue! self event))
                    (next)))))))) ; 続行
      ;; この時点でexitフラグが立っているなら終了
      (unless exit?
        ;; 次の受信データをselectで待つ
        ;; TODO: ここは将来、cthreadsに対応した際に、thread-select!及び、
        ;;       それと同等の操作に置き換えられる
        (selector-select selector)
        ;; selectorが反応したら再実行
        (loop #f)))))

(define-method %irc-send-thread ((self <pib>))
  (let loop ()
    (mutex-lock! (irc-send-queue-mutex-of self))
    (let* ((event (and-let* ((e (irc-send-dequeue! self)))
                    (send-event-split-last-param e)))
           (message (and
                      event ; eventが#fならmessageも#fにする
                      (ces-convert (event->message event #t)
                                   (gauche-character-encoding)
                                   (irc-server-encoding-of self))))
           (sent-usec #f) ; あとで設定される
           )
      (cond
        ;((eq? message 'shutdown) #f) ; 終了(旧コード)
        ((not message) ; キューが空だった(cvシグナルを待つ)
         (mutex-unlock! (irc-send-queue-mutex-of self) (irc-send-cv-of self))
         (loop)) ; cvシグナル受信。キューチェックの段階から再実行する
        (else ; 通常messageだった
          ;; まず、(irc-send-last-microsec-of self)をチェックし、
          ;; 一定時間以内なら待つ
          (when (flood-protection-interval-of self)
            (let1 remain (-
                           (+
                             (irc-send-last-microsec-of self)
                             (flood-protection-interval-of self))
                           (gettimeofday-usec))
              (when (positive? remain)
                (dynamic-wind
                  (lambda ()
                    ;; 一旦アンロックする
                    (mutex-unlock! (irc-send-queue-mutex-of self)))
                  (lambda ()
                    ;; 待つ
                    ;; TODO: ここは将来、cthreadsに対応した際に、
                    ;;       thread-select!及び、
                    ;;       それと同等の操作に置き換えられる？
                    (selector-select (make <selector>) remain))
                  (lambda ()
                    ;; 再度ロックする
                    (mutex-lock! (irc-send-queue-mutex-of self)))))))
          ;; 送信する
          (guard (e (else
                      (set! (irc-send-laststatus-of self) 'error)
                      (set! exit? #t)
                      (set! sent-usec (gettimeofday-usec))))
            (with-port-locking
              (irc-output-port-of self)
              (lambda ()
                ;; 送信直前に、pibオブジェクト内部に変更を及ぼすcommandなら、
                ;; それらしく変更を行う必要がある
                (send-update-pib! self event)
                ;; 既に変換済なので、そのまま送れる
                (display message (irc-output-port-of self))
                (flush (irc-output-port-of self))))
            (let1 usec (gettimeofday-usec)
              ;; その他の項目を設定する
              (set! (irc-send-last-microsec-of self) usec)
              (set! (irc-send-laststatus-of self) 'ok)
              (set! sent-usec usec)))
          ;; ログを取る
          (let1 sent-event (list* (usec->timestamp sent-usec)
                                  'send
                                  #f
                                  (cdddr event))
            (logging self sent-event))
          ;; アンロックする
          (mutex-unlock! (irc-send-queue-mutex-of self))
          (loop))))))

(define-method %irc-pong-thread ((self <pib>))
  (let loop ()
    ;; TODO: この値はあとで動的設定可能にする事
    (thread-sleep! (* 60 60))
    ;; TODO: 「何もしない」か「PONG 接続先」か「TIME」か選べるようにする
    ;;       (TIMEの場合は、返り値を無視するように直す)
    ;; TODO: PINGに応答した時にタイムスタンプを取っておいて、
    ;;       一定時間PINGが来てない時のみ送信するようにする？
    ;; TODO: 続きはあとで
    (cond
      ((not (irc-registered?-of self)) (loop))
      ((irc-finished?-of self) #t)
      (else
        ;; TODO: ロックしてないのでrace conditionが発生する余地有り
        ;;       (しかし、起こってもそれほど問題にならないので、
        ;;        ここでは放置する)
        (irc-send-enqueue!
          self `(#f #f #f PONG ,(or
                                  (pong-target-of self)
                                  (irc-server-ip-of self))))
        (loop)))))

;; TODO: 名前が適切でない。あとで考え直す事
;; この手続きは、pibのスロットを更新しつつ、enqueueすべきかどうかを真偽値で返す
(define (recv-update-pib! self event)
  (let1 command (event->command event)
    ;; 433, 451以外を受信し始めたら、ircサーバからregisteredされたものとする
    ;; (実際は、register前にNOTICEで通知してくるircサーバ等あるが、この辺りの
    ;;  初期メッセージは、with-irc内のdynamic-windのbefore thunk内の
    ;;  初期化部分で一旦全部受け取ってから、
    ;;  その後にフラグを一旦クリアしている事で、
    ;;  擬似的に回避している。ちょっと分かりにくい)
    (when (and
            (not (irc-registered?-of self))
            (not (eqv? command 433))
            (not (eqv? command 451)))
      (set! (irc-registered?-of self) #t))
    (cond
      ((eq? command 'NICK)
       ;; prefixが自分宛てなら、current-nickを変更する
       ;; channelが(get-current-nick)と同じ、つまり自分自身宛てなら、
       ;; prefixから返信先を求める
       ;; prefixの書式は、"nick!~username@hostname"
       ;; よって、!より前の部分を取り出すだけで良い
       (when (equal?
               (current-nick-of self)
               (and-let* ((prefix (event->prefix event))
                          (matched (#/\!/ prefix)))
                 (matched 'before)))
         (guard (e (else e))
           (set! (current-nick-of self) (car (event->params event)))))
       #t) ; enqueueする
      ((eq? command 'PING) ; PING/自動応答
       (set! (pong-target-of self) (car (event->params event)))
       (irc-send-enqueue!
         self `(#f #f #f PONG ,@(event->params event)))
       #f) ; enqueueしない
      ((eqv? command 433) ; NICK変更失敗/自動応答
       (let1 next-nick (generate-next-nick (base-nick-of self))
         (irc-send-enqueue!
           self `(#f #f #f NICK ,next-nick)))
       #f) ; enqueueしない
      (else
        #t)))) ; enqueueする

(define (send-update-pib! self event)
  ;; 今のところ、変更が必要なのはNICKのみ
  (let1 command (event->command event)
    (cond
      ((eq? command 'NICK)
       (guard (e (else e))
         (let1 nick (car (event->params event))
           (unless (irc-registered?-of self)
             (set! (current-nick-of self) nick))
           (set! (base-nick-of self) nick))))
      (else
        #t))))


(define (usec->timestamp usec)
  (date->string
    (time-utc->date
      (seconds->time
        (quotient usec 1000000)))))

(define (gettimeofday-usec)
  (receive (sec usec) (sys-gettimeofday)
    (+ (* sec 1000000) usec)))


(define-method logging ((self <pib>) event)
  (when (logging-handler-of self)
    (with-locking-mutex
      (irc-logger-mutex-of self)
      (lambda ()
        ((logging-handler-of self) event)))))


(define (make-thread/report-error thunk)
  (make-thread
    (lambda ()
      (guard (e (else
                  (display
                    (call-with-output-string
                      (lambda (p)
                        (with-error-to-port p (cut report-error e))))
                    (standard-error-port))
                  (raise e)))
        (thunk)))))


(define (with-irc irc-server-ip ; "111.222.33.44" 等の文字列
                  irc-server-port ; 6667 等の数値
                  . keywords+main-thunk)
  (let ((keywords (drop-right keywords+main-thunk 1))
        (main-thunk (last keywords+main-thunk)))
    (let-keywords keywords ((irc-server-encoding "utf-8")
                            (irc-server-pass #f)
                            (thread-type 'gauche.threads)
                            (base-nick "pib") ; 最長9文字らしい
                            (username #f)
                            (realname #f)
                            (flood-protection-interval 900000)
                            (logging-handler #f)
                            )
      ;; 引数が未指定の際の簡単な自動設定
      (unless username
        (set! username base-nick))
      (unless realname
        (set! realname username))
      ;; 引数の簡単なチェック
      (unless (valid-nick? base-nick)
        (error "invalid nick" base-nick))
      ;; TODO: 他の値もチェックする事

      (let1 pib (make <pib>
                      :irc-server-ip irc-server-ip
                      :irc-server-port irc-server-port
                      :irc-server-encoding irc-server-encoding
                      :irc-server-pass irc-server-pass
                      :thread-type thread-type
                      :base-nick base-nick
                      :username username
                      :realname realname
                      :main-thunk main-thunk
                      :flood-protection-interval flood-protection-interval
                      :logging-handler logging-handler
                      )
        (parameterize ((param:pib pib))
          (dynamic-wind
            (lambda ()
              ;; pibの内部用スロットに値を設定する
              (set! (current-nick-of pib) base-nick)
              (set! (irc-socket-of pib)
                (make-client-socket 'inet irc-server-ip irc-server-port))
              (set! (irc-input-port-of pib)
                (socket-input-port (irc-socket-of pib) :buffering :modest))
              (set! (irc-output-port-of pib)
                (socket-output-port (irc-socket-of pib) :buffering :line))
              (set! (irc-recv-thread-of pib)
                (make-thread/report-error (lambda ()
                                            (%irc-recv-thread pib))))
              (set! (irc-send-thread-of pib)
                (make-thread/report-error (lambda ()
                                            (%irc-send-thread pib))))
              (set! (irc-pong-thread-of pib)
                (make-thread/report-error (lambda ()
                                            (%irc-pong-thread pib))))
              (set! (irc-recv-queue-of pib) (make-queue))
              (set! (irc-send-queue-of pib) (make-queue))
              (set! (irc-recv-queue-mutex-of pib)
                (make-mutex "recv-queue"))
              (set! (irc-send-queue-mutex-of pib)
                (make-mutex "send-queue"))
              (set! (irc-recv-cv-of pib)
                (make-condition-variable "recv"))
              (set! (irc-send-cv-of pib)
                (make-condition-variable "send"))
              (set! (irc-send-laststatus-of pib) 'ok)
              (set! (irc-send-last-microsec-of pib) 0)
              (set! (irc-logger-mutex-of pib)
                (make-mutex "logger"))
              (set! (irc-registered?-of pib) #f)
              (set! (irc-finished?-of pib) #f)
              (set! (pong-target-of pib) #f)
              ;; その他の初期化処理を行う
              (thread-start! (irc-recv-thread-of pib))
              (thread-start! (irc-send-thread-of pib))
              (thread-start! (irc-pong-thread-of pib))
              ;; 接続直後に送られてきたメッセージを一通り受信するのを待つ
              (thread-sleep! 2)
              ;; 一通り受信したら、一旦irc-registered?を#fに戻す
              (set! (irc-registered?-of pib) #f)
              ;; PASS, NICK, USERコマンドを通す
              (when (irc-server-pass-of pib)
                (%irc-send-event! pib
                                  `(#f #f #f PASS ,(irc-server-pass-of pib))))
              (%irc-send-event! pib `(#f #f #f NICK ,(base-nick-of pib)))
              (%irc-send-event! pib `(#f
                                      #f
                                      #f
                                      USER
                                      ,(username-of pib)
                                      "0.0.0.0"
                                      "0.0.0.0"
                                      ,(realname-of pib)))
              ;; TODO: joinしているチャンネルも、ここで統一的に扱えた方が良い？
              #t)
            (lambda ()
              (let loop ()
                (unless (irc-registered?-of pib)
                  (thread-sleep! 1)
                  (loop)))
              (%irc-main pib))
            (lambda ()
              ;; ソケットの停止処理を行う
              ;; (受信スレッドに停止を通知する動作も兼ねている)
              (with-port-locking
                (irc-input-port-of pib)
                (lambda ()
                  (with-port-locking
                    (irc-output-port-of pib)
                    (lambda ()
                      (ignore-error #f (close-input-port
                                         (irc-input-port-of pib)))
                      (ignore-error #f (close-output-port
                                         (irc-output-port-of pib)))
                      (ignore-error #f (socket-shutdown (irc-socket-of pib) 2))
                      (ignore-error #f (socket-close (irc-socket-of pib)))))))
              ;; 子スレッドの停止処理を行う
              (ignore-error #f (thread-join! (irc-pong-thread-of pib) 1))
              (ignore-error #f (thread-terminate! (irc-pong-thread-of pib)))
              (ignore-error #f (thread-join! (irc-send-thread-of pib) 1))
              (ignore-error #f (thread-terminate! (irc-send-thread-of pib)))
              (ignore-error #f (thread-join! (irc-recv-thread-of pib) 1))
              (ignore-error #f (thread-terminate! (irc-recv-thread-of pib)))
              ;; TODO: 他にも行うべき処理があるのでは？
              ;; 念の為、スロットを解放しておく
              (set! (current-nick-of pib) #f)
              (set! (irc-socket-of pib) #f)
              (set! (irc-input-port-of pib) #f)
              (set! (irc-output-port-of pib) #f)
              (set! (irc-recv-thread-of pib) #f)
              (set! (irc-send-thread-of pib) #f)
              (set! (irc-pong-thread-of pib) #f)
              (set! (irc-recv-queue-of pib) #f)
              (set! (irc-send-queue-of pib) #f)
              (set! (irc-recv-queue-mutex-of pib) #f)
              (set! (irc-send-queue-mutex-of pib) #f)
              (set! (irc-recv-cv-of pib) #f)
              (set! (irc-send-cv-of pib) #f)
              (set! (irc-send-laststatus-of pib) #f)
              (set! (irc-send-last-microsec-of pib) #f)
              (set! (irc-logger-mutex-of pib) #f)
              (set! (irc-registered?-of pib) #f)
              (set! (irc-finished?-of pib) #f)
              (set! (pong-target-of pib) #f)
              #t)))))))



;;; ----


;; NB: 送信eventと受信eventで、ロギング先が違うものが存在する？
;; NB: 返り値がlistである事に注意する事(#fの時をのぞいて)
(define (event->logging-names event)
  (let ((prefix (event->prefix event))
        (mode (event->mode event))
        (command (event->command event))
        (params (event->params event))
        )
    (let/cc return
      (define (channel? entity)
        (#/^(?:\#|\&)/ entity))
      (define (split-channels entity)
        (string-split entity #\,))
      (define (find-channel-from-params)
        (or
          (find
            (cut channel? <>)
            params)
          (return #f)))

      (let1 entity (case command
                     ((PRIVMSG NOTICE) ; channelまたはnickを取る
                      ;; 受信eventかつ対象がchannelではなくnickだった場合、
                      ;; それは自分自身となってしまうので、その時は
                      ;; prefixから対象を取る必要がある
                      (let1 target (car params)
                        (if (or
                              (eq? mode 'send)
                              (channel? target))
                          target
                          (or
                            (and-let* ((prefix (event->prefix event))
                                       (matched (#/\!/ prefix)))
                              (matched 'before))
                            (return #f)))))
                     ((JOIN PART TOPIC KICK) ; channelを取る
                      (car params))
                     ((INVITE) ; INVITEは特別扱いする
                      ;; NB: INVITEでcarを取らない理由:
                      ;; - carを取る場合は、ログ名=アクション実行者名という解釈
                      ;; - cadrを取る場合は、ログ名=where相当という解釈
                      ;; PRIVMSGによるnick直通の会話が、sendもrecvも
                      ;; 相手nickのところにロギングする方針になっている為、
                      ;; INVITEもこれに合わせて、後者の解釈を取るものとする。
                      (cadr params))
                     ((MODE) ; channelまたはnickを取る
                      (let1 target (car params)
                        ;; targetがchannelなら、そのまま採用してよい
                        ;; targetがnickなら、#fを返す(自分自身のみの筈なので)
                        (unless (channel? target)
                          (return #f))
                        target))
                     ((NAMES LIST)
                      ;; 考えた結果、NAMES, LISTは#fを返す事にした
                      (return #f))
                     ((QUIT)
                      ;; TODO: QUITが送られてきた場合、そのnickがjoinしている
                      ;;       channel全てにログを残すのがbetter。
                      ;;       しかし、今のところ参加者個別のchannel参加情報は
                      ;;       扱っていないので、それはできない。
                      ;;       もし将来に扱うようになったら、実装してもよい
                      (return #f))
                     ;; 以下、数値reply対応
                     ;; TODO: 他にも追加すべき数値reply多数
                     ;; nick返す系
                     ((301) (car params)) ; RPL_AWAY
                     ;; channel返す系
                     ((321 322 323) ; RPL_LIST*
                      (find-channel-from-params))
                     ((324) ; RPL_CHANNELMODEIS
                      (find-channel-from-params))
                     ((331 332) ; RPL_NOTOPIC, RPL_TOPIC
                      (find-channel-from-params))
                     ((341 342) ; RPL_INVITING, RPL_SUMMONING
                      (find-channel-from-params))
                     ((353 366) ; RPL_NAMEREPLY, RPL_ENDOFNAME
                      (find-channel-from-params))
                     (else
                       (return #f)))
        ;; entityには、channel(s)またはnickが入る
        ;; channel(s)の場合は分割する必要がある
        (if (channel? entity)
          (split-channels entity)
          (list entity)))))) ; 返り値はlistにする必要がある

;; with-ircで扱うevent形式のイベントをロギングするユーティリティ手続き
;; (効率は悪い)
;; チャンネル別のディレクトリが作られ、その中に日別のファイルが作られ、そこに
;; (write event)が一行ずつ追記される
;; 引数の詳細については以下の通り
;; - log-dirはログファイルを生成するディレクトリ(要ファイル生成権限)
;; - eventは送信eventまたは受信event
;; - global-dirnameは、MOTD等の、チャンネル指定等の無いeventを保存する
;;   ディレクトリを指定する(デフォルトは"#")
(define (make-basic-irc-logger log-dir . opt-global-dirname)
  (let1 global-dirname (get-optional opt-global-dirname "#")
    ;; イベントを受け取り、ロギングを行う手続きを返す
    (lambda (event)
      (let1 target-dirs (guard (e (else (list global-dirname)))
                          ;; eventがevent書式ではなく、eofやerrorなら、
                          ;; globalに記録させる
                          (unless (list? event)
                            (error "!"))
                          ;; 特定のcommandなら、ロギング自体を行わない
                          ;(when (eq? (event->command event) 'PASS) '())
                          ;; USERコマンド通過前は常にglobal-dirnameとする
                          (unless (irc-registered?-of (param:pib))
                            (error "!"))
                          ;; targetを取得する
                          ;; (取得できなかった場合はglobal-dirnameにする)
                          (let1 targets (or
                                          (event->logging-names event)
                                          (error "!"))
                            (map
                              (lambda (t)
                                ;; 小文字化する
                                (string-downcase
                                  ;; 万が一空文字列なら変名する
                                  (if (equal? t "")
                                    "_"
                                    ;; "."、"/"を含まないように置換
                                    (regexp-replace #/\.|\// t "_"))))
                              targets)))
        ;; target-dirsの数だけ処理を行う
        (for-each
          (lambda (target-dir)
            (let* ((dir (build-path log-dir target-dir))
                   (date (guard (e (else (current-date)))
                           (string->date (event->timestamp event))))
                   (file (date->string date "~Y-~m-~d.log"))
                   (path (build-path dir file))
                   )
              (make-directory* dir)
              (sys-unlink (build-path dir "latest"))
              (sys-symlink file (build-path dir "latest"))
              (with-output-to-file
                path
                (lambda ()
                  (write event)
                  (newline))
                :if-exists :append
                :buffering :full)))
          target-dirs)))))

(provide "pib")

