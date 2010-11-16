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
;;;   (lambda (pib)
;;;     (send-message! pib '(#f "JOIN" "#channel")) ; joinコマンドを実行
;;;     (let loop ()
;;;       (let1 event (receive-message! pib 1) ; timeoutを1秒でmessage受信
;;;         (when event
;;;           (write event)
;;;           (newline))
;;;         (loop)))))
;;; - あとで、send-message!とreceive-message!の引数詳細を書く
;;; 注意点:
;;; - with-ircの内部からの継続/エラー脱出は可能ですが、
;;;   内部への継続を辿る事には対応していません。
;;; - receive-message!やsend-message!でブロックしている最中は、
;;;   シグナルが処理されません。
;;;   (つまり、ctrl+cやSIGTERMで終了させられなくなるという事です)
;;;   可能なら、非ブロックモードにしたり、短か目のtimeoutを設定して
;;;   繰り返した方が安全です。


;;; TODO: encoding変換portがcloseされた時に、
;;;       selectorにイベント通知が行ってない疑惑がある

;;; TODO: IRCリクエストは最大510文字+CRLFらしい。あとで制約をつける事

;;; TODO: 可能なら、値が適正かどうかのチェックは、slotにset!する際に行いたい

;;; TODO: flood protection ("Excess Flood")

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
;;; -- send-message!内にて、コマンドを監視し、JOIN/PART/MODE時は
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
;;;   送信用cvにcondition-variable-signal!を行う
;;; - 親スレッドは、受信キューにデータが入ってくるのを待つ為に、
;;;   受信用cvでmutexをアンロックする(タイムアウトは任意値)
;;; - 受信スレッドは、データを受信してキューに入れると同時に
;;;   受信用cvにcondition-variable-signal!を行う


(define-module pib
  (use srfi-1)
  (use srfi-13)
  (use gauche.net)
  (use gauche.charconv)
  (use gauche.threads)
  (use gauche.selector)
  (use util.list)
  (use util.queue)
  (use text.tree)

  (export
    with-irc
    <pib>
    send-message!
    receive-message!

    ;; 以下は、ユーティリティ手続きとして提供(仮)
    message->event
    event->message-for-send
    ))
(select-module pib)


(define-syntax ignore-error
  (syntax-rules ()
    ((ignore-error fallback . bodies)
     (guard (e (else fallback)) . bodies))))


(define (%with-port-locking port thunk)
  ;; どうも、with-port-lockingがデッドロックしてしまうようなので、
  ;; ロックしないようにしてみる
  ;; TODO: あとで対策を考える事
  (thunk))


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
   (main-proc
     :accessor main-proc-of
     :init-keyword :main-proc
     :init-form (error "must be need main-proc"))
   ;; 内部状態スロット(全て、あとで設定される)
   (current-nick
     :accessor current-nick-of)
   (irc-socket
     :accessor irc-socket-of)
   (irc-input-port
     :accessor irc-input-port-of)
   (irc-output-port
     :accessor irc-output-port-of)
   (irc-receive-thread
     :accessor irc-receive-thread-of)
   (irc-send-thread
     :accessor irc-send-thread-of)
   (irc-receive-queue
     :accessor irc-receive-queue-of)
   (irc-send-queue
     :accessor irc-send-queue-of)
   (irc-receive-queue-mutex
     :accessor irc-receive-queue-mutex-of)
   (irc-send-queue-mutex
     :accessor irc-send-queue-mutex-of)
   (irc-receive-cv
     :accessor irc-receive-cv-of)
   (irc-send-cv
     :accessor irc-send-cv-of)
   (irc-receive-cv-mutex
     :accessor irc-receive-cv-mutex-of)
   (irc-send-cv-mutex
     :accessor irc-send-cv-mutex-of)
   (irc-send-laststatus
     :accessor irc-send-laststatus-of)
   ;; TODO: flood対策用スロットをあとで追加する事
   ))

(define-method initialize ((self <pib>) initargs)
  (next-method)
  #t)


;; eventは、以下のようなlistとする。
;; '(prefix command . params)
;; - prefixは、このmessageのsenderを特定できる文字列。送信時には無視される
;; - commandは、"PRIVMSG"等の文字列、または三桁の数値
;; - paramsは、規定の書式を満たす文字列のlist(書式については、rfc参照)
(define-method send-message! ((self <pib>) event . opt-sync)
  ;; - opt-syncが#fなら、送信はasyncに実行され、送信が成功したかは分からない
  ;;   (デフォルトの動作)
  ;; - opt-syncが#tなら、送信はsyncに実行され、返り値として送信が成功したかが
  ;;   真偽値として返ってくるが、送信が完全に完了するまで待たされる
  ;;   (尚、これは「送信」が成功したかどうかであり、「コマンド実行」が
  ;;    成功したかどうかではない点に注意する事。そして「送信」が失敗する原因は
  ;;    通常、通信断以外には存在しない)
  ;;   このチェックの実装方法は未定。
  ;;   (ちゃんと実装しようとすると、かなりややっこしい。busy loopだと
  ;;    race condition時に無限ループする可能性が残る)
  (irc-send-enqueue! self event)
  (if (get-optional opt-sync #f)
    (error "not implement yet") ; TODO: あとで実装する
    #t))
(define-method receive-message! ((self <pib>) . opts)
  ;; NB: cvは本来なら、条件に一致しない場合にも
  ;;     アンロックされる可能性がある為、timeout判定は複雑になるが、
  ;;     今回はアンロックされる要因は一つだけなので、
  ;;     単純にtimeout判定を行ってもよい(多分)
  (let-optionals* opts ((timeout #f))
    (let loop ()
      (mutex-lock! (irc-receive-cv-mutex-of self))
      (let1 event (irc-receive-dequeue! self)
        (if (or event (equal? timeout 0))
          (begin
            ;; キューに値が入ってきた or timeoutが0だった。
            ;; 処理して普通にアンロックして終了
            (mutex-unlock! (irc-receive-cv-mutex-of self))
            event)
          (begin
            ;; キューが空なので、タイムアウト有りでcvシグナルを待つ
            (if (mutex-unlock! (irc-receive-cv-mutex-of self)
                               (irc-receive-cv-of self)
                               timeout)
              (loop) ; cvシグナル受信。再取得して値を返す(※↑のNB参照)
              #f))))))) ; タイムアウトした。



;; NICKに失敗した際に、次に試行するNICKを生成する手続き
(define (generate-next-nick nick)
  (define (nick->num nick)
    (let1 reverse-nick-chars (reverse (string->list nick))
      (let loop ((idx 0)
                 (acc 0))
        (if (<= (length reverse-nick-chars) idx)
          acc
          (let* ((current-char (list-ref reverse-nick-chars idx))
                 (current-char-num (hash-table-get *nickchar->nicknum*
                                                   current-char))
                 (current-figure (expt (vector-length *chars-can-use-nick*)
                                       idx))
                 (delta (* current-char-num current-figure))
                 )
            (loop (+ 1 idx)
                  (+ acc delta)))))))
  (define (num->nick num)
    (let loop ((idx 8)
               (restnum num)
               (reverse-nick-chars '()))
      (if (<= 0 idx)
        (let* ((figure-threshold (expt (vector-length *chars-can-use-nick*)
                                       idx))
               (figure (quotient restnum figure-threshold))
               (char (vector-ref *chars-can-use-nick* figure))
               (next-restnum (remainder restnum figure-threshold))
               )
          (loop (- idx 1)
                next-restnum
                (cons char reverse-nick-chars)))
        (list->string (reverse reverse-nick-chars)))))

  (if (< (string-size nick) 9)
    ;; nickが9文字に達していないなら、末尾に一文字追加するのみ
    (string-append nick (string (vector-ref *chars-can-use-nick* 0)))
    ;; nickが9文字に達しているなら、一旦分解してインクリメントして再構築する
    (num->nick (remainder (+ 1 (nick->num nick)) *nicknum-max*))))

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
  (expt (vector-length *chars-can-use-nick*) 9))
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
;;
(define (message->event message)
  (guard (e (else e))
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

    ;; prefixは、更に細かく分解してもいいかも知れない
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
                           command))
           (params-true (parse-params params))
           )
      (list* prefix-true
             command-true
             params-true))))

;; event形式(S式)のデータをircのmessage書式の文字列に変換する。
;; 尚、これはクライアントが送信時に使うものという前提の為、
;; この手続きで変換する元eventにvalidなprefixが含まれていても、
;; messageは、prefixを含んでいないものが常に生成されるものとする。
;; (rfcより、prefixを含んだmessageをクライアントからサーバに送っても、
;;  無視されてしまう為)
;; 尚、この手続きは、eventの内容が書式に適正かどうかはチェックしていないので、
;; もし必要なら、この手続きに渡す前に内容の検証を行う必要がある。
;; (このチェックは親スレッドで行った方が良いので、
;;  キューに入れる辺りで行うのが良い)
(define (event->message-for-send event)
  (let* ((prefix (car event))
         (command (cadr event))
         (params (cddr event))
         (params-middles (drop-right params 1))
         (params-trailing (string-append ":" (last params)))
         )
    (tree->string
      (list
        (intersperse " " `(,command ,@params-middles ,params-trailing))
        "\r\n"))))


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
      (<= 1 (length nick-list) 9)
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
  ;; あとは、main-procを実行して、返り値が戻る(またはエラーになる)のを待つだけ
  ((main-proc-of self) self))



;; キューが空の時は#fを返す(キューに#fが入っている事は無いものとする)
(define-method irc-send-dequeue! ((self <pib>))
  (with-locking-mutex
    (irc-send-queue-mutex-of self)
    (lambda ()
      (if (queue-empty? (irc-send-queue-of self))
        #f
        (dequeue! (irc-send-queue-of self))))))
(define-method irc-receive-dequeue! ((self <pib>))
  (with-locking-mutex
    (irc-receive-queue-mutex-of self)
    (lambda ()
      (if (queue-empty? (irc-receive-queue-of self))
        #f
        (dequeue! (irc-receive-queue-of self))))))

(define-method irc-send-enqueue! ((self <pib>) event)
  (with-locking-mutex
    (irc-send-queue-mutex-of self)
    (lambda ()
      (enqueue! (irc-send-queue-of self) event)))
  (condition-variable-signal! (irc-send-cv-of self)))
(define-method irc-receive-enqueue! ((self <pib>) event)
  (with-locking-mutex
    (irc-receive-queue-mutex-of self)
    (lambda ()
      (enqueue! (irc-receive-queue-of self) event)))
  (condition-variable-signal! (irc-receive-cv-of self)))


;; この手続きの仕様等:
;; - socket自身の切断などは親スレッドまたは接続先サーバが行う為、
;;   portに対する送受信時にエラー例外が発生したなら、それを確認し、
;;   もしそうなら、エラーではなく接続断としてキューにイベントを入れる事。
;;   そして、その原因が親スレッドであっても接続先サーバであっても、
;;   以降まともに送受信する事は出来ない筈なので、スレッドは
;;   自分自身を終了させる事。
;;   (返り値を返す、またはthread-terminate!を実行する)
(define-method %irc-receive-thread ((self <pib>))
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
        (cond
          ((eof-object? message) message) ; eof
          ((string-incomplete? message)
           (message->event
             (string-incomplete->complete message :omit)))
          (else
            (message->event message))))))

  ;; このselectorはportが読み取れるまで待つ為だけのものなので、
  ;; ハンドラ自体は何もしなくてもよい
  (let1 selector (make <selector>)
    (selector-add! selector (irc-input-port-of self) (lambda (p f) #t) '(r x))
    (let loop ((exit? #f))
      ;; 今受信可能なデータは全て受信
      (%with-port-locking
        (irc-input-port-of self)
        (lambda ()
          (let next ()
            (if (not (char-ready? (irc-input-port-of self)))
              #f
              (let1 event (fetch-event)
                ;; eventがPINGや433の場合、キューには保存せずに、
                ;; ここで自動的に応答を行う必要がある
                ;; (尚、受信による自動応答時は、laststatusは変更しない事)
                (cond
                  ((eof-object? event) ; eof
                   (irc-receive-enqueue! self event)
                   (set! exit? #t)) ; 終了
                  ((condition? event) ; エラーオブジェクト
                   (irc-receive-enqueue! self event)
                   (next)) ; 続行
                  ((equal? (cadr event) "PING") ; PING/自動応答
                   (irc-send-enqueue! self `(#f "PONG" ,(caddr event)))
                   (next)) ; 続行
                  ((eqv? (cadr event) 433) ; NICK変更失敗/自動応答
                   (set! (current-nick-of self)
                     (generate-next-nick (current-nick-of self)))
                   (irc-send-enqueue! self
                                      `(#f "NICK" ,(current-nick-of self)))
                   (next)) ; 続行
                  (else ; 通常message
                    (irc-receive-enqueue! self event)
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
  ;; TODO: flood対策
  (let loop ()
    (mutex-lock! (irc-send-cv-mutex-of self))
    (let1 event (irc-send-dequeue! self)
      (cond
        ((eq? event 'shutdown) #f) ; 終了
        ((not event) ; キューが空だった(cvシグナルを待つ)
         (mutex-unlock! (irc-send-cv-mutex-of self) (irc-send-cv-of self))
         (loop)) ; cvシグナル受信。再実行する(receive-message!のコメント参照)
        (else ; 通常eventだった
          ;; 送信する
          (guard (e (else
                      (set! (irc-send-laststatus-of self) 'error)
                      (set! exit? #t)))
            (%with-port-locking
              (irc-output-port-of self)
              (lambda ()
                (display
                  (event->message-for-send event)
                  (irc-output-port-of self))
                (flush (irc-output-port-of self))))
            (set! (irc-send-laststatus-of self) 'ok))
          ;; アンロックする
          (mutex-unlock! (irc-send-cv-mutex-of self))
          (loop))))))



(define (with-irc irc-server-ip ; "111.222.33.44" 等の文字列
                  irc-server-port ; 6667 等の数値
                  . keywords+main-proc)
  (let ((keywords (drop-right keywords+main-proc 1))
        (main-proc (last keywords+main-proc)))
    (let-keywords keywords ((irc-server-encoding "utf-8")
                            (irc-server-pass #f)
                            (thread-type 'gauche.threads)
                            (base-nick "pib") ; 最長9文字らしい
                            (username #f)
                            (realname #f)
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
                      :main-proc main-proc
                      )
        (dynamic-wind
          (lambda ()
            ;; pibの内部用スロットに値を設定する
            (set! (current-nick-of pib) base-nick)
            (set! (irc-socket-of pib)
              (make-client-socket 'inet irc-server-ip irc-server-port))
            (set! (irc-input-port-of pib)
              (open-input-conversion-port
                (socket-input-port (irc-socket-of pib) :buffering :modest)
                irc-server-encoding
                :to-code (gauche-character-encoding)
                :owner? #t))
            (set! (irc-output-port-of pib)
              (open-output-conversion-port
                (socket-output-port (irc-socket-of pib) :buffering :line)
                irc-server-encoding
                :from-code (gauche-character-encoding)
                :owner? #t))
            (set! (irc-receive-thread-of pib)
              (make-thread (lambda ()
                             (%irc-receive-thread pib))))
            (set! (irc-send-thread-of pib)
              (make-thread (lambda ()
                             (%irc-send-thread pib))))
            (set! (irc-receive-queue-of pib) (make-queue))
            (set! (irc-send-queue-of pib) (make-queue))
            (set! (irc-receive-queue-mutex-of pib)
              (make-mutex "receive-queue"))
            (set! (irc-send-queue-mutex-of pib)
              (make-mutex "send-queue"))
            (set! (irc-receive-cv-of pib)
              (make-condition-variable "receive-cv"))
            (set! (irc-send-cv-of pib)
              (make-condition-variable "send-cv"))
            (set! (irc-receive-cv-mutex-of pib)
              (make-mutex "receive-cv"))
            (set! (irc-send-cv-mutex-of pib)
              (make-mutex "send-cv"))
            (set! (irc-send-laststatus-of pib) 'ok)
            ;; その他の初期化処理を行う
            (thread-start! (irc-receive-thread-of pib))
            (thread-start! (irc-send-thread-of pib))
            ;; まず最初に、NICKコマンドとUSERコマンドを通しておく必要がある
            (send-message! pib `(#f "NICK" ,(current-nick-of pib)))
            (thread-sleep! 2)
            (send-message! pib `(#f "USER"
                                 ,(username-of pib)
                                 "0.0.0.0"
                                 "0.0.0.0"
                                 ,(realname-of pib)))
            ;; TODO: joinしているチャンネルも、ここで統一的に扱えた方が良い？
            #t)
          (lambda ()
            (%irc-main pib))
          (lambda ()
            ;; ソケットの停止処理を行う
            ;; (受信スレッドに停止を通知する動作も兼ねている)
            (%with-port-locking
              (irc-input-port-of pib)
              (lambda ()
                (%with-port-locking
                  (irc-output-port-of pib)
                  (lambda ()
                    (ignore-error #f (close-input-port
                                       (irc-input-port-of pib)))
                    (ignore-error #f (close-output-port
                                       (irc-output-port-of pib)))
                    (ignore-error #f (socket-shutdown (irc-socket-of pib) 2))
                    (ignore-error #f (socket-close (irc-socket-of pib)))))))
            ;; 送信スレッドのキューに、終了を通知する
            (send-message! pib 'shutdown)
            ;; 子スレッドの停止処理を行う
            ;; (通常は、↑の処理で子スレッドは停止する。
            ;;  このコードはあくまでもfailsafe的な意味のもの)
            (ignore-error #f (thread-join! (irc-receive-thread-of pib) 1))
            (ignore-error #f (thread-terminate! (irc-receive-thread-of pib)))
            (ignore-error #f (thread-join! (irc-send-thread-of pib) 1))
            (ignore-error #f (thread-terminate! (irc-send-thread-of pib)))
            ;; TODO: 他にも行うべき処理があるのでは？
            ;; 念の為、スロットを解放しておく
            (set! (current-nick-of pib) #f)
            (set! (irc-socket-of pib) #f)
            (set! (irc-input-port-of pib) #f)
            (set! (irc-output-port-of pib) #f)
            (set! (irc-receive-thread-of pib) #f)
            (set! (irc-send-thread-of pib) #f)
            (set! (irc-receive-queue-of pib) #f)
            (set! (irc-send-queue-of pib) #f)
            (set! (irc-receive-queue-mutex-of pib) #f)
            (set! (irc-send-queue-mutex-of pib) #f)
            (set! (irc-receive-cv-of pib) #f)
            (set! (irc-send-cv-of pib) #f)
            (set! (irc-receive-cv-mutex-of pib) #f)
            (set! (irc-send-cv-mutex-of pib) #f)
            (set! (irc-send-laststatus-of pib) #f)
            #t))))))



(provide "pib")

