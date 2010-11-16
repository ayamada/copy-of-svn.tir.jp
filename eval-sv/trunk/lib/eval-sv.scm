;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; "eval with supervise" module
;;;
;;;  Copyright (c) 2008 Atsuo Yamada, All rights reserved.
;;;
;;;  Permission is hereby granted, free of charge, to any person
;;;  obtaining a copy of this software and associated documentation
;;;  files (the "Software"), to deal in the Software without restriction,
;;;  including without limitation the rights to use, copy, modify,
;;;  merge, publish, distribute, sublicense, and/or sell copies of
;;;  the Software, and to permit persons to whom the Software is
;;;  furnished to do so, subject to the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;  AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
;;;  OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;;;  IN THE SOFTWARE.


;;; このモジュールは、ステップ監視付きの評価器(のジェネレータ)を提供する。

;;; TODO: 全体的なドキュメントの修正

;;; TODO: import/svやenfold-entity時に、
;;;       既にenfold済かどうかのチェックをした方がいいかも知れない
;;;       (二重enfold対策)
;;;       しかし、どうやって実現するかが微妙

;;; 現バージョンの重大な制約:
;;; - eval/sv内でdefine-syntaxやdefine-macroを使ってマクロを生成できません。
;;; -- 外部で生成したマクロを持ち込む事は可能ですが、監視が機能していない為、
;;;    マクロ展開が停止する事を保証してください。


;;; 利用目的:
;;; - evalの実行時に(無限ループやブロック停止等の)いわゆる「ハマり問題」
;;;   を回避できるように、ブロック停止等の起こり得る要素を除外した上で、
;;;   ハマり問題が起こり得る可能性のある場面で割り込み手続きを実行し、
;;;   任意の段階で、evalを続行すべきかどうかの判断を可能とする。
;;; - また、割り込み時の処理を横取りする事が可能なので、
;;;   他の用途にも転用できるかも知れない。
;;; - 当モジュールでは、リソース使い過ぎによるDoSを防ぐ事は出来ないので、
;;;   もし必要な場合は別途、リソース方面の対策も行う必要がある。

;;; 概要:
;;; (define eval/sv (make-eval/sv ...))
;;; または
;;; (define-values (eval/sv env) (make-eval/sv ...))
;;; のように、eval/sv手続きを生成する。
;;; eval/svは、
;;; (eval/sv '(+ 1 2) sv-proc)
;;; のように、sv-procを指定して実行する事になる。
;;; (この時のeval時の環境は、同時に生成されたenvが自動的に採用される。)
;;; また、昔ながらの環境指定無しevalのように、
;;; (eval/sv '(+ 1 2))
;;; のようにしても式を評価できる。
;;; (この時、make-eval/sv時のキーワード引数:default-sv-procがあると、
;;;  それが指定されたものとして評価される。)
;;; eval/svによる評価では、以下のタイミングでsv-procが割り込み実行される。
;;; - (明示的に割り込み除外指定しているもの以外の)手続き実行時
;;; - (明示的に割り込み除外指定しているもの以外の)マクロ展開一回毎
;;; - (明示的に割り込み除外指定しているもの以外の)syntax実行時
;;; - (無限ループする可能性のある)特定syntax/手続きのループ内実行一回毎
;;; このsv-procには、(手続き/macro/syntax)実行用の情報が渡されるので、
;;; 普通に実行してよいと判断したなら、自分で実行を行う必要がある。
;;; もし、引数を変更してから実行したいと判断したり、
;;; これは実行したくないと判断したなら、そのようにしてもよい。
;;; また、sv-proc呼び出し時には、eval/sv呼び出し元へ一気に戻る為の
;;; 継続も渡されるので、それを使ってevalを終了してもいいし、
;;; proc内で継続を生成して、疑似スレッド動作のような事も可能となる。
;;;
;;; この評価時の環境状態は、make-eval/sv時に設定できる。
;;; デフォルトでは、R5RSベースに少し改変を加えたモジュールを継承した、
;;; make-eval/sv毎に独立した無名モジュールとなる。
;;; この継承前モジュールはカスタマイズする事が可能だが、
;;; 割り込み実行を行えるようにする為に、特定のルールに従う必要がある。
;;; また、make-eval/sv後に生成されるenvに対してimport/svを使う事で、
;;; 後付けで手続き/マクロ/syntax/変数を束縛する事も出来る。

;;; 重要な問題点:
;;; このモジュールの実装には、Gauche固有の(R5RS標準ではない)、
;;; 「evalに渡すS式に、シンボルではない(既にコンパイル済の)実体を渡す」
;;; 機能を使っている。
;;; とりあえずportableではないという点に注意。

;;; 注意点:
;;; - 親モジュールの制約に注意する事。
;;; - 実行速度は望めない。
;;; - methodのarityが正しくなくなる(method以外の手続きのarityは正しい筈)
;;; - eval/sv内から手続きを持ち出して実行した場合、
;;;   監視は行われない。
;;;   持ち出した手続きを監視付きで実行したい時は、
;;;   またeval/sv内に入れて実行すればよい。
;;;   (let1 proc (eval/sv '(lambda (a) (+ 1 a)) sv-proc)
;;;     (proc 2) ; これではsv-procは実行されない
;;;     (eval/sv `(,proc 2) sv-proc) ; これならsv-procは実行される
;;; - 外の手続き/マクロ/special formをeval/sv内に持ち込む際には、
;;;   以下の点に注意する事。
;;; -- ※(手続きの場合)手続きの内部で無限ループ等が発生しない事を保証する事。
;;; --- 標準のテンプレートモジュールで提供されている手続きは、
;;;     無限ループ等が発生しないように、一部の手続きが差し換えられている。
;;; -- ※(マクロの場合)マクロ展開後に出現するsymbolがsyntaxの場合、
;;;      そのsyntaxの束縛が既にeval/sv環境にある必要がある。
;;;      (syntax以外ではeval/sv環境内にある必要は無い)
;;; --- 特に、通常は提供されている、特定のspecial form/マクロ/手続きを
;;;     (安全の為に)提供しなかった際に、Gauche組み込みのマクロであっても
;;;     この状態になる可能性がある事に注意。
;;; ---- letを提供していないのにlet1マクロをimportした場合など。
;;; ---- 例えば、useマクロ(use hoge)は、
;;;      (begin (with-module gauche (require "hoge")) (import hoge))
;;;      に展開されるので、begin with-module importの内
;;;      どれか一つでも欠けている環境では正常に機能しない。
;;; -- ※持ち込んだ段階で、「enfoldされた」状態になるようにする事。
;;; --- 「enfoldされた」とは、持ち込もうとしているsyntaxや手続きに、
;;;     監視手続きが組み込まれる事を指している。
;;; --- enfoldするには、次のどれかを行えばよい。
;;; ---- make-eval/sv時の:bind-alist経由でグローバルに束縛する
;;; ---- importer手続き経由でグローバルに束縛する
;;; ---- 自分自身でenfold-entityを呼び出し、enfoldする
;;;      (グローバルに束縛したくない時用)
;;; --- もし、enfoldされていない手続き/syntaxが存在する場合、
;;;     その手続き絡みで無限ループ等が発生し、
;;;     無限ループ中に監視ポイントが含まれていなければ、
;;;     自力では止められなくなる。
;;;     (外部からシグナルを送る等で止める事は通常通り可能だが……)
;;; -- ※手続き/マクロ/special formを持ち込む際には、必ず一つずつ持ち込む事。
;;;    list/vector/hash-table等の中に入れて持ち込んだ場合、
;;;    たとえ:bind-alistやimporterやenfold-entity経由であっても、
;;;    監視手続きが組み込まれないので、トラブルの原因になる。
;;; - apply可能なobjectを持ち込む場合は、object-applyもimportする必要がある。
;;; -- 尚、gauche.parameterもobject-applyによる実装になっている

;;; セキュリティ上の問題点:
;;; - 当モジュールでは、リソース使用過多によるDoSを根本的に防ぐ事は出来ない。
;;; -- 一応、define等の副作用系処理を禁止したり、
;;;    毎回make-eval/svしなおしてclean upする事で、
;;;    リソース使用状況の蓄積を防ぐ事は可能だが……。


;; * make-eval/svについて
;; (define-values (eval/sv env)
;;   (make-eval/sv :default-sv-proc #f
;;                 ;; ↑デフォルトの監視手続き。デフォルト値は#f。
;;                 ;; ↑ここで設定せずに、eval/sv実行時に指定してもよい
;;                 ;; ↑(その方が分かりやすい)
;;                 :isolate-port? bool
;;                 ;; ↑current-*-portをダミーに差し替えるフラグ。
;;                 ;; デフォルト値は#t。
;;                 ;; (current-*-portがブロックデバイスだった場合、
;;                 ;;  ブロッキング停止が発生する可能性がある為、
;;                 ;;  安全性を確保する為に、#tにしておいた方が良い。
;;                 ;;  自前でcurrent-*-portでブロッキング停止が
;;                 ;;  発生しない保証をしているなら、#fにしてもよい。)
;;                 :parent-module 'module-name
;;                 ;; ↑make-eval/svがextendする、
;;                 ;; 無名モジュールの親モジュール名。
;;                 ;; シンボルで指定する事(無名モジュール不可)。
;;                 ;; 親モジュールは、規定の方法で初期化されている事。
;;                 :bind-alist `((symbol ,expr) ...)
;;                 ;; ↑追加束縛のalist。
;;                 ;; 自動的に(import/sv env symbol expr)される。
;;                 ))
;; (eval/sv expr sv-proc)
;;
;; make-eval/svのキーワード引数の:default-sv-proc、及び、
;; eval/svの引数のsv-procは、以下のような手続きとなる。
;; この手続きは、eval/sv実行中に、後述のタイミングで割り込み実行される。
;; (define (sv-proc type symbol expr args return except) ...)
;; - typeは、割り込み種別を示すシンボルとなる。この種類については後述する。
;; - symbolは、束縛対象のシンボル(シンボルが無い場合は#f)。
;; - exprは、symbolに束縛された実体。
;;   これを手動で実行する事で、評価のステップが進行する。
;;   マクロ展開やsyntaxの場合も、lambdaにくるまれて渡されるので、
;;   特に問題がなければ、何も考えずに(apply expr args)してしまってよい。
;; - argsは、exprを実行する際の引数。
;; -- マクロ展開時やsyntaxの場合、この値は意味を持たない可能性が高い。
;; - returnは、eval/svを一気に抜ける為の継続が渡される。
;; -- error例外による脱出はguardされる可能性があるので、これを用意した。
;; -- returnに渡した引数は、そのままeval/svの返り値になる。
;; - exceptは、エラー例外を発生させてeval/svを一気に抜ける為の継続が渡される。
;; -- error例外による脱出はguardされる可能性があるので、これを用意した。
;; -- exceptに渡した引数は、(apply error args)される。
;; -- 要は(error "hoge" ...)したい時に、代わりに(except "hoge" ...)すれば良い。
;;
;; typeの詳細は、以下の通りとなる。
;; - 'procは、割り込みが設定された手続き、method, promise, cont等、
;;   手続きに類するものの実行時にこの割り込みが発生した事を示す。
;; - 'macroは、マクロの展開時にこの割り込みが発生した事を示す。
;;   展開が再帰的に行われる場合は、その一回一回に対して割り込みが発生する。
;; - 'syntaxは、special formの実行時にこの割り込みが発生した事を示す。
;; - 'loopは、特定proc及びsyntaxで、ループまたは再帰が呼び出される毎に
;;   割り込みが発生した事を示す。
;;   (これは、ループ/再帰内で、割り込みを発生させるものが全く無かった時に
;;    ループ/再帰を停止させる手段が無くなるのを防止する為に用意された。)


(define-module eval-sv
  (use srfi-1)
  (use util.list)
  (use gauche.parameter)
  (use eval-sv.nullport)
  (use eval-sv.common)
  (use eval-sv.enfold)
  (use eval-sv.template)
  (use eval-sv.form) ; expand-form

  (export
    make-eval/sv

    ;; came from eval-sv.common
    atmn ; テンプレート指定補助マクロ
    ;; came from eval-sv.enfold
    import/sv ; 束縛追加手続き
    enfold-entity ; 監視追加手続き
    ;; enfold-entityは、eval/sv内で評価した時に監視が行われるように、
    ;; 対象をlambdaやマクロでくるみ、くるんだ結果を返り値として返す。
    ;; (但し、entityがlistや即値の場合は何もしない。)
    ;; import/svは、enfold-entityすると同時に、指定モジュールに対して
    ;; グローバルな束縛を生成する。
    ))
(select-module eval-sv)


;; importしている手続き類をそのまま外部にexportする為の定義
(define atmn atmn)
(define import/sv import/sv)
(define enfold-entity enfold-entity)


;;; ----
;;; ユーティリティ手続き類など


;; デフォルトのテンプレート親モジュール
;; (詳細はbinds.scmを参照)
(define *default-parent-module-symbol* [atmn 'r5rs])
;; TODO: binds.scmが生成する、何種類かのテンプレートモジュール(のシンボル)を
;;       ユーザが指定しやすいように、それらしく提供する必要がある

;; fallbackなdefault-sv-proc
;; (監視無しのeval実行をエミュレートする用途)
(define (fallback-sv-proc type symbol expr args return except)
  (apply expr args))


;;; ----
;;; 本体

(define (make-eval/sv . keywords)
  (let-keywords keywords ((default-sv-proc #f) ; 前述の説明参照
                          (isolate-port? #t)
                          ;; current-*-portをダミーのものに差し替えるフラグ。
                          ;; (current-*-portがブロックデバイスだった場合、
                          ;;  ブロッキング停止が発生してしまう可能性がある為、
                          ;;  安全性を確保する為に、#tにしておいた方が良い。
                          ;;  自前でcurrent-*-portでブロッキング停止が
                          ;;  発生しない保証をしているなら、#fにしてもよい。)
                          (parent-module *default-parent-module-symbol*)
                          ;; シンボルで指定する事(無名モジュール不可)。
                          ;; parent-moduleに含まれる束縛は、
                          ;; 既にimporterを通した形になっているものとする
                          ;; (importerを通していない場合は、
                          ;;  sv-procによる監視実行が行われない事に注意)
                          (bind-alist '()) ; (symbol expr)形式の追加束縛設定
                          ;; bind-alistはparent-moduleとは違い、
                          ;; 必要な束縛には明示的にimporterを通す。
                          )
    ;; parent-moduleの簡易チェック
    (unless (find-module parent-module)
      (errorf "cannot found module ~a" parent-module))
    ;; parent-moduleをextendしたenv(無名モジュール)を生成する
    (let1 env (spawn-module parent-module)
      ;; envにbind-alistを束縛する
      (for-each
        (lambda (symbol+expr)
          (apply import/sv env symbol+expr))
        bind-alist)
      ;; eval/svを生成する
      (let1 eval/sv (%make-eval/sv default-sv-proc env isolate-port?)
        ;; 値を返す
        (values eval/sv env)))))


;; eval/svを生成して返す手続き。
;; - eval/svは、入れ子実行可能である必要がある。
;; -- import/svを使って、eval/sv内にeval/svを持ち込めるという事。
;; - eval/svは、第二引数に、新しいsv-procを指定できる。
;; -- 但し、これはR5RSのevalの引数と混乱しやすいので、
;;    第二引数がモジュールだった場合のみ、何もしない仕様とする。
;;    (指定されたモジュールは単に無視される。)
;; - もし、新しいsv-procが指定された場合、そのeval/sv内では、
;;   古いsv-procと新しいsv-procの、両方が実行される事になる。
;;   (もし、eval/svのネストが三段なら、三つのsv-procが実行される。)
;;   (実行順序は、再帰検出の都合上、古いものから先に実行される。)
;; - sv-procがcontを呼んだ場合、
;;   そのsv-procが設定された時のeval/svを抜ける事になる。
;; -- eval/svが二段になっていて、別々のsv-procが設定されている場合、
;;    外側のsv-procに渡されるcontを呼ぶと外側のeval/sv実行時点に戻り、
;;    内側のsv-procに渡されるcontを呼ぶと内側のeval/sv実行時点に戻る。
;; note: sv-proc及びnew-sv-procは引数六つ。
;;       しかしisv-procとnew-isv-procは引数四つとする。
;;       isv-proc類は、ただ単にlambdaでcontを内包しただけのもの。
(define (%make-eval/sv default-sv-proc env isolate-port?)
  (let1 port-guarder (if isolate-port?
                       isolate-current-port
                       (lambda (thunk) (thunk)))
    ;; ↓の手続きを、eval/svとして返す
    (rec (eval/sv expr . opt-sv-proc)
      ;; return継続とexcept継続を受け取り、実際のeval/sv処理を行う
      (define (solve-eval/sv return except)
        (let ((nested? (param:isv-proc))
              (new-sv-proc (let1 tmp-sv-proc (get-optional opt-sv-proc #f)
                             (and
                               (not (module? tmp-sv-proc))
                               tmp-sv-proc)))
              )
          (define (sv-proc->isv-proc proc)
            (lambda (type symbol expr args)
              (proc type symbol expr args return except)))
          (define (doit isv-proc)
            ;; 新しいisv-procを設定する
            (parameterize ((param:isv-proc isv-proc)
                           (param:eval/sv eval/sv) ; 自分自身
                           (param:env env)
                           )
              ;; macroとsyntaxの展開を行う(ここでもisv-procが参照される)
              (let1 true-expr (expand-form expr env)
                ;; 実行する
                (eval true-expr env))))
          (if (not nested?)
            (port-guarder
              (lambda ()
                (doit
                  (sv-proc->isv-proc
                    (or
                      new-sv-proc
                      default-sv-proc
                      fallback-sv-proc)))))
            (let1 old-isv-proc (param:isv-proc)
              (doit
                ;; 古いsv-procと新しいsv-procをmergeして実行する
                (lambda (type symbol expr args)
                  ;; まずold-isv-procをold-contと共に実行する。
                  ;; old-isv-procが(apply expr args)を実行すると、
                  ;; 次に、new-sv-procが現在のreturnと共に実行される。
                  ;; その後、new-sv-procが(apply expr in-args)を実行し、
                  ;; やっと本来の、割り込み元の手続きが実行される。
                  (old-isv-proc
                    type
                    symbol
                    (lambda in-args
                      (let1 sv-proc (or
                                      new-sv-proc
                                      default-sv-proc
                                      fallback-sv-proc)
                        (sv-proc type symbol expr in-args return except)))
                    args)))))))

      ;; このeval/sv呼び出しを抜ける為の継続を生成し、solve-eval/svを実行する
      ;; (return ...)を実行すると、eval/svを抜け、返り値として...を返す。
      ;; (except ...)を実行すると、eval/svを抜け、(apply error ...)を実行する。
      (let/cc return
        (receive r (let/cc except
                     (receive rr (solve-eval/sv return except)
                       (apply return rr)))
          (apply error r))))))

;;; ----

(provide "eval-sv")

