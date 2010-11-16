;#!/usr/local/gauche/bin/gosh
;#!/usr/local/gauche/bin/speedygosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; S式バトラーcui

;;; 仕様:
;;; - sys-setrlimitする
;;; - sbattle.scm check < ...
;;;   で、stdinに流されたS式のデータの簡易チェックのみ行う。
;;;   stdinからは、以下のようなkeywordsを渡す事。
;;;   '(:p1name "プレイヤー1の名前"
;;;     :p2name 同上
;;;     :p1sexpr プレイヤー1のS式を文字列で与える
;;;     :p2sexpr 同上
;;;     :p1hp プレイヤー1の初期HP
;;;     :p2hp 同上
;;;     )
;;;   返り値は、stdoutにS式として返される。
;;;   正常時は、`(ok)が返る。
;;;   異常時は、`(error ,errors)が返る。errorsはエラー内容の文字列のlist。
;;; - sbattle.scm < ...
;;;   で、前述の簡易チェックを行ってから、実際の実行を行う。
;;;   stdinはcheckと同様。
;;;   返り値は、stdoutにS式として返される。
;;;   正常時は、`(done ,winner ,logs)が返る。winnerは1または2。
;;;   異常時は、`(error ,errors)が返る。errorsはエラー内容の文字列のlist。


;;; TODO: ゲーム的に、提供してもよい、他の手続き
;;; - 敵の名前を文字列として取り出す
;;; - 経過ステップ数を得る


(use text.tree)
(use srfi-1)
(use util.list)
(use gauche.parameter)
(use gauche.charconv)
(use rfc.sha1)
(use util.digest)

(add-load-path "lib")
(use eval-sv)
(use eval-cu-lite)


;;; ----
;;; グローバル値の定義
(define-constant *draw-step* 9999)
(define-constant *debug* #f)

;;; ----
;;; 戦闘用パラメータ及び束縛

;; infoは以下のようなlistになる
;; '(hp action wait bonus-flag)
(define p:player1-info (make-parameter #f))
(define p:player2-info (make-parameter #f))
(define p:player-turn-is (make-parameter #f)) ; 1または2
(define p:bat-first-is (make-parameter #f)) ; どっちが先攻だったかを1か2で
(define p:logger (make-parameter #f)) ; ログ出力手続き
(define p:player1-cont (make-parameter #f))
(define p:player2-cont (make-parameter #f))
(define p:finish-cont (make-parameter #f)) ; 決着が着いた時に呼ぶ継続

;; '(hp action wait bonus-flag)
(define info->hp car)
(define info->action cadr)
(define info->wait caddr)
(define info->bonus-flag cadddr)

;; wait後に継続を相手に渡す為の手続き
(define pass
  (enfold-entity
    'pass
    (lambda args #t)))

(define (logging self? logobj)
  ((p:logger) (cons
                (if self?
                  (p:player-turn-is)
                  (+ (logxor (- (p:player-turn-is) 1) 1) 1)) ; 反転
                logobj)))

(define (action-g)
  ;; 構えwait
  (unless (eq? (info->action (%self-info)) 'g)
    (%wait 20)
    (pass)
    (logging #t '(ready g))
    (%update-self-info! ; 構え更新
      (lambda (hp action wait bonus-flag)
        (values hp 'g wait bonus-flag))))
  ;; 行動前wait
  (%wait 60)
  (pass)
  ;; 殴る
  (logging #t '(attack g))
  (let1 enemy-action (info->action (%enemy-info))
    (cond
      ((eq? enemy-action 'p)
       (logging #f '(parry))
       (logging #t '(stun))
       (%update-self-info!
         (lambda (hp action wait bonus-flag)
           (values hp #f (+ wait 100) #f))))
      (else
        (%update-enemy-info!
          (lambda (hp action wait bonus-flag)
            (logging #f '(damage 10))
            (logging #f `(hp ,(- hp 10)))
            (values (- hp 10) action wait bonus-flag)))
        (when (<= (info->hp (%enemy-info)) 0)
          ((p:finish-cont) (p:player-turn-is))))))
  (pass)
  ;; 行動後wait
  (%wait 20)
  (pass)
  (%update-self-info! ; bonus-flag解除
    (lambda (hp action wait bonus-flag)
      (values hp action wait #f)))
  (pass))
(define (action-c)
  ;; 構えwait
  (unless (eq? (info->action (%self-info)) 'c)
    (%wait 10)
    (pass)
    (logging #t '(ready c))
    (%update-self-info! ; 構え更新
      (lambda (hp action wait bonus-flag)
        (values hp 'c wait bonus-flag))))
  ;; 行動前wait
  (%wait 20)
  (pass)
  ;; 殴る
  (logging #t '(attack c))
  (let1 enemy-action (info->action (%enemy-info))
    (cond
      ((eq? enemy-action 'g)
       (logging #f '(guard)))
      ;((eq? action 'c)
      ; (logging #f '(guard)))
      (else
        (%update-enemy-info!
          (lambda (hp action wait bonus-flag)
            (logging #f '(damage 6))
            (logging #f `(hp ,(- hp 6)))
            (values (- hp 6) action wait bonus-flag)))
        (when (<= (info->hp (%enemy-info)) 0)
          ((p:finish-cont) (p:player-turn-is))))))
  (pass)
  ;; 行動後wait
  (%wait 40)
  (pass)
  (%update-self-info! ; bonus-flag解除
    (lambda (hp action wait bonus-flag)
      (values hp action wait #f)))
  (pass))
(define (action-p)
  ;; 構えwait
  (unless (eq? (info->action (%self-info)) 'p)
    (%wait 30)
    (pass)
    (logging #t '(ready p))
    (%update-self-info! ; 構え更新
      (lambda (hp action wait bonus-flag)
        (values hp 'p wait bonus-flag))))
  ;; 行動前/行動後wait
  (%wait 10) ; これは、(action-p)連続使用対応の為に用意
  (%update-self-info! ; bonus-flag解除
    (lambda (hp action wait bonus-flag)
      (values hp action wait #f)))
  (pass))

(define (action-off . opt-wait-step)
  ;; 構えwait
  (unless (eq? (info->action (%self-info)) #f)
    ;; 構えwaitは0なので不要
    (%update-self-info!
      (lambda (hp action wait bonus-flag)
        (logging #t '(ready #f))
        (values hp
                #f
                (+ wait (get-optional opt-wait-step 0))
                bonus-flag))))
  (pass))

(define (wait step)
  ;(logging #t '(wait))
  (%wait step))
(define (self-info)
  (list-copy (%self-info)))
(define (enemy-info)
  (list-copy (%enemy-info)))

(define (say message . opt-prior)
  (let ((prior (get-optional opt-prior 3))
        (message-true (if (list? message)
                        (with-output-to-string
                          (lambda ()
                            (write/ss message)))
                        (x->string message))))
    (logging #t `(say ,message-true ,prior))))

;; 以下は、内部用手続き
(define (set-self-info! new-val)
  ((get-accessor #t) new-val))
(define (set-enemy-info! new-val)
  ((get-accessor #f) new-val))

(define (%self-info)
  ((get-accessor #t)))
(define (%enemy-info)
  ((get-accessor #f)))
(define (%wait step)
  (unless (is-a? step <integer>)
    (error "step must be <integer>" step))
  (when (< step 0)
    (error "step must be positive" step))
  (%update-self-info!
    (lambda (hp action wait bonus-flag)
      (values hp
              action
              (+ wait (if bonus-flag
                        (x->integer (* 0.8 step))
                        step))
              bonus-flag))))

(define (get-accessor self?) ; 自分なら#t、敵なら#f
  (cond
    ((eqv? (p:player-turn-is) 1) (if self? p:player1-info p:player2-info))
    ((eqv? (p:player-turn-is) 2) (if self? p:player2-info p:player1-info))
    (else
      (error "assertion"))))

(define (%update-self-info! proc)
  (update-info! #t proc))
(define (%update-enemy-info! proc)
  (update-info! #f proc))
(define (update-info! self? proc)
  ;; (define (proc hp action wait bonus-flag)
  ;;   ...
  ;;   (values hp action wait bonus-flag))
  (let1 accessor (get-accessor self?)
    (receive r (apply proc (accessor))
      (accessor r))))









(define (get-bat-first p1sexpr p2sexpr)
  (define (get-digest-odd? sexpr)
    (let1 str (write-to-string sexpr)
      (#/(1|3|5|7|9|b|d|f)$/ (digest-hexify
                               (sha1-digest-string str)))))

  (let ((p1odd? (get-digest-odd? p1sexpr))
        (p2odd? (get-digest-odd? p2sexpr)))
    (if (xor p1odd? p2odd?) 2 1)))

(define (xor a b)
  (cond
    ((and a b) #f)
    ((or a b) #t)
    (else #f)))

(define *bind-alist*
  `(
    (action-g   ,action-g)
    (action-c   ,action-c)
    (action-p   ,action-p)
    (action-off ,action-off)
    (wait       ,wait)
    (self-info  ,self-info)
    (enemy-info ,enemy-info)
    (say        ,say)
    (info->hp         ,info->hp)
    (info->action     ,info->action)
    (info->wait       ,info->wait)
    (info->bonus-flag ,info->bonus-flag)
    ))
(define (get-eval/sv)
  (make-eval/sv
    :bind-alist *bind-alist*
    :isolate-port? (not *debug*)
    ))

(define (get-eval/cu)
  (make-eval/cu
    :default-threshold 256
    :macro-threshold   256
    :loop-threshold    256
    :bind-alist *bind-alist*
    :isolate-port? (not *debug*)
    ))

(define *continued* (gensym))
(define *sexpr-battle-main*
  ;; すぐ抜け無限ループ対策として、初回に一回監視を呼んでおく
  ;; TODO: 将来に、lambda内に'loop監視がつけば、不要になる？？？
  `(begin
     (,pass)
     (battle-main)))

;; この手続きは、戦闘の共通部分を実行する
;; 返り値は、どちらが勝ったかと戦闘ログの二値
;; エラー例外を投げるかも知れない事に注意
(define (battle:start p1name p1hp p1sexpr
                      p2name p2hp p2sexpr)
  ;; 各種の変数を用意する
  (let ((bat-first (get-bat-first p1sexpr p2sexpr)) ; 1または2
        (total-steps 0) ; 実行ステップカウンタ
        (r-logs '()) ; ここにログを記録する
        (p1eval/sv (get-eval/sv))
        (p2eval/sv (get-eval/sv))
        )
    (define (num->player-name num)
      (cond
        ((eqv? 1 num) p1name)
        ((eqv? 2 num) p2name)
        (else #f)))

    ;; eval/sv内にbattle-mainを設定する(sexprは既に安全な事が確認されている事!)
    (p1eval/sv p1sexpr)
    (p2eval/sv p2sexpr)
    ;; parameterを設定する
    (parameterize ((p:player1-info `(,p1hp #f 0 #f)) ; hp action wait bonus
                   (p:player2-info `(,p2hp #f 0 #f))
                   (p:player1-cont #f) ; 初期値は空
                   (p:player2-cont #f) ; 初期値は空
                   (p:player-turn-is bat-first) ; 1または2
                   (p:bat-first-is bat-first) ; 1または2
                   (p:logger (lambda (logobj)
                               (let1 logobj2 (cons
                                               (num->player-name (car logobj))
                                               (cdr logobj))
                                 (set! r-logs
                                   (cons
                                     logobj2
                                     r-logs)))))
                   (p:finish-cont #f)
                   )
      (define (supervisor type symbol expr args return except)
        ;; 継続の入れ換えを行う
        (let/cc cont ; 続行の継続を作る
          ;; 続行の継続を自分自身のparameterに保存する
          (cond
            ((eqv? 1 (p:player-turn-is)) (p:player1-cont cont))
            ((eqv? 2 (p:player-turn-is)) (p:player2-cont cont))
            (else
              (error "assertion")))
          ;; とりあえず抜ける
          (return *continued*))
        ;; ステップ数をカウントする
        (inc! total-steps)
        ;; ステップ毎に"1000ステップ経過"のようなログを残す
        (when (zero? (modulo total-steps 1000))
          ((p:logger) (list #f 'elapse total-steps)))
        (when (< *draw-step* total-steps)
          ((p:logger) (list #f 'draw))
          (cond
            ((< (info->hp (p:player2-info)) (info->hp (p:player1-info)))
             ((p:finish-cont) 1))
            ((< (info->hp (p:player1-info)) (info->hp (p:player2-info)))
             ((p:finish-cont) 2))
            (else
              ((p:finish-cont) (+ (logxor (- (p:bat-first-is) 1) 1) ; 反転
                                  1)))))
        ;; waitがあるなら、waitを減らして抜ける
        ;; (副作用実行なので、contを更新する必要は無い)
        (when (< 0 (info->wait (%self-info)))
          (%update-self-info!
            (lambda (hp action wait bonus-flag)
              (values hp action (- wait 1) bonus-flag)))
          (return *continued*))
        ;; プロセスを実行する
        (apply expr args))

      ;; 戦闘を開始する
      ;; rには、1か2が入る
      (let1 r (let/cc complete
                (p:finish-cont complete)
                (while #t
                  ;; まず、初回継続を生成する
                  (cond
                    ((eqv? (p:player-turn-is) 1)
                     (when (p:player1-cont)
                       ((p:player1-cont))) ; contがあるなら、実行して戻らない
                     ;; contが無いなら、battle-mainを起動する
                     (guard (e (else
                                 ;; 実行時エラーが発生
                                 ((p:logger) (list (p:player-turn-is) 'fumble))
                                 (%update-self-info!
                                   (lambda (hp action wait bonus-flag)
                                     (values hp action (+ wait 120) #f)))
                                 (p:player1-cont #f))) ; 次はbattle-mainから
                       (unless (eq? *continued*
                                    (p1eval/sv *sexpr-battle-main* supervisor))
                         ;; 正常終了(ペナルティ与えて再起動)
                         ((p:logger) (list (p:player-turn-is) 'fumble))
                         (%update-self-info!
                           (lambda (hp action wait bonus-flag)
                             (values hp action (+ wait 120) #f)))
                         (p:player1-cont #f))) ; 次はbattle-mainから
                     (p:player-turn-is 2)) ; 相手のターン
                    ((eqv? (p:player-turn-is) 2)
                     (when (p:player2-cont)
                       ((p:player2-cont))) ; contがあるなら、実行して戻らない
                     ;; contが無いなら、battle-mainを起動する
                     (guard (e (else
                                 ;; 実行時エラーが発生
                                 ((p:logger) (list (p:player-turn-is) 'fumble))
                                 (%update-self-info!
                                   (lambda (hp action wait bonus-flag)
                                     (values hp action (+ wait 120) #f)))
                                 (p:player2-cont #f))) ; 次はbattle-mainから
                       (unless (eq? *continued*
                                    (p2eval/sv *sexpr-battle-main* supervisor))
                         ;; 正常終了(ペナルティ与えて再起動)
                         ((p:logger) (list (p:player-turn-is) 'fumble))
                         (%update-self-info!
                           (lambda (hp action wait bonus-flag)
                             (values hp action (+ wait 120) #f)))
                         (p:player2-cont #f))) ; 次はbattle-mainから
                     (p:player-turn-is 1)) ; 相手のターン
                    (else
                      (error "assertion" (p:player-turn-is))))))
        ;; 最後に、最終戦闘結果をロギングする
        ((p:logger) (list r 'won))
        ;; 結果を返す
        (values r (reverse r-logs))))))



;;; ----


(define (main:battle keywords)
  (let-keywords keywords ((p1name #f) ; 文字列
                          (p2name #f) ; 文字列
                          (p1sexpr #f) ; 文字列
                          (p2sexpr #f) ; 文字列
                          (p1hp 100) ; integer
                          (p2hp 100) ; integer
                          )
    (receive (r logs) (battle:start p1name p1hp (sexpr-str->sexpr p1sexpr)
                                    p2name p2hp (sexpr-str->sexpr p2sexpr))
      `(done ,r ,logs))))

(define (main:checkonly keywords)
  ;; ここに来た段階で、既に検証は完了しているので、結果を返すだけでよい
  '(ok))

(define (sexpr-is-not-valid? sexpr)
  (guard (e (else (ref e 'message)))
    (receive (eval/cu env) (get-eval/cu)
      (eval/cu sexpr)
      ;; battle-mainの束縛が存在する事も確認する
      (unless (global-variable-bound? env 'battle-main)
        (error "battle-main手続きが定義されていません"))
      #f)))

(define (sexpr-str->sexpr sexpr-str)
  (call-with-input-string
    sexpr-str
    (lambda (port)
      (list*
        'begin
        (let1 s (read port)
          (when (eof-object? s) ; 最低一個は読む必要がある
            (error "S式が空です"))
          s)
        (let next ()
          (let1 s (read port)
            (if (eof-object? s)
              '()
              (cons s (next)))))))))

(define (check-sexpr-string sexpr-string target-str)
  ;; S式のreadチェックを行う
  (let1 sexpr (guard (e (else
                          (error
                            (string-append
                              target-str
                              "のS式のread時にエラーが発生しました"
                              "\n"
                              (ref e 'message)))))
                (sexpr-str->sexpr sexpr-string))
    ;; 実際にeval/svに通し、正常に終了する事を確認する
    (let1 sexpr-error (sexpr-is-not-valid? sexpr)
      (when sexpr-error
        (error
          (string-append
            target-str
            "のS式のeval時にエラーが発生しました"
            "\n"
            sexpr-error)))
      #f)))


(define (check-keywords keywords)
  (let-keywords keywords ((p1name #f) ; 文字列
                          (p2name #f) ; 文字列
                          (p1sexpr #f) ; 文字列
                          (p2sexpr #f) ; 文字列
                          (p1hp 100) ; integer
                          (p2hp 100) ; integer
                          )
    (unless p1name
      (error "プレイヤー1の名前が指定されていません"))
    (unless p2name
      (error "プレイヤー2の名前が指定されていません"))
    (unless p1sexpr
      (error "プレイヤー1のS式が指定されていません"))
    (unless p2sexpr
      (error "プレイヤー2のS式が指定されていません"))
    (when (equal? "" p1name)
      (error "キャラクタ1の名前が空です"))
    (when (equal? "" p2name)
      (error "キャラクタ2の名前が空です"))
    (when (< 64 (string-size p1name))
      (error "キャラクタ1の名前が長すぎます"))
    (when (< 64 (string-size p2name))
      (error "キャラクタ2の名前が長すぎます"))
    (when (equal? p1name p2name)
      (error "キャラクタ1とキャラクタ2の名前が同じです"))
    (when (equal? "" p1sexpr)
      (error "キャラクタ1のS式が空です"))
    (when (equal? "" p2sexpr)
      (error "キャラクタ2のS式が空です"))
    (when (< 2048 (string-size p1sexpr))
      (error "キャラクタ1のS式が長すぎます"))
    (when (< 2048 (string-size p2sexpr))
      (error "キャラクタ2のS式が長すぎます"))
    ;; S式のreadチェックを行う
    (check-sexpr-string p1sexpr "キャラクタ1")
    (check-sexpr-string p2sexpr "キャラクタ2")
    ;; エラー例外を出さずにチェックに通った
    #t))

;; 制限が必要なのを確認してから、それらしく値を設定していく
(define *rlimit-alist*
  (list
    (cons RLIMIT_AS 0.5) ; address space (virtual memory) limit
    ;(cons RLIMIT_CORE #f)
    (cons RLIMIT_CPU 20) ; CPU time in seconds
    (cons RLIMIT_DATA 0.5) ; max data size
    ;(cons RLIMIT_FSIZE #f)
    ;(cons RLIMIT_LOCKS #f)
    ;(cons RLIMIT_MEMLOCK #f)
    ;(cons RLIMIT_MSGQUEUE #f)
    ;(cons RLIMIT_NICE #f)
    ;(cons RLIMIT_NOFILE #f)
    ;(cons RLIMIT_NPROC #f)
    (cons RLIMIT_RSS 0.5) ; max resident set size
    ;(cons RLIMIT_RTPRIO #f)
    ;(cons RLIMIT_SIGPENDING #f)
    ;(cons RLIMIT_SBSIZE #f)
    (cons RLIMIT_STACK 0.5) ; max stack size
    ;(cons RLIMIT_OFILE #f)
    ))
(define (set-rlimit!)
  (for-each
    (lambda (res.val)
      (let ((resource (car res.val))
            (value (cdr res.val)))
        (when value
          (let/cc cancel
            (let1 old-rlimit (sys-getrlimit resource)
              (when (< 0 value 1)
                ;; 0から1の間の値の場合は、割合と解釈するようにする
                ;; 但し、RLIM_INFINITYの時は何もしない
                ;; (無限に割合を掛けても無限のまま)
                (when (equal? old-rlimit RLIM_INFINITY)
                  (cancel))
                (let1 new-value (x->integer (* value old-rlimit))
                  (set! value new-value)))
              ;; 設定を反映する
              (unless (equal? old-rlimit value)
                (sys-setrlimit resource value)))))))
    *rlimit-alist*))

(define (main argv)
  (define (main-thunk)
    ;; argvを使って引数を受けるか、
    ;; それともstdin経由にするか、どっちか決める事
    ;; 実行モードはargvで決めて、
    ;; S式はstdinで受け取るのがいい気がする
    ;; stdinはkeywordsなlistで受け取るのがいい気がする
    (guard (e (else
                ;; エラーを結果として返す
                (parameterize ((debug-print-width #t))
                  `(error (,(ref e 'message))))))
      (let1 checkonly (cond
                        ((null? (cdr argv)) #f) ; 引数無し
                        ((equal? (cadr argv) "check") #t) ; 第一引数がcheck
                        (else ; それ以外
                          (errorf "invalid argv ~s" argv)))
        ;; まず、stdinからkeywordsを読み込む
        ;; (ここのencodingは(gauche-character-encoding)を期待してokとする)
        (let1 keywords (read)
          ;; keywordsの検証を行う。
          ;; 問題があった場合は、そのままエラー例外を投げてよい
          (check-keywords keywords)
          (if checkonly
            (main:checkonly keywords)
            (main:battle keywords))))))

  ;; rlimitを再設定する
  (set-rlimit!)

  ;; 返り値は常にS式で返すので、cgi-main類似のportのラッピングを行う
  ;; stdoutは一時的にstderrに流すようにする
  ;; (間違って何か出力しても安全にする目的)
  (let1 result (with-output-to-port (current-error-port) main-thunk)
    (write result)
    (newline)
    (flush)
    0)) ;; 常に正常終了でいいのかは謎



