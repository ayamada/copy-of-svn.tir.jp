;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; このモジュールは、説明文などの長いテキスト及びhtml片を定義する。

(define-module sb-text
  (use text.html-lite)
  (use text.tree)
  (use srfi-1)
  (use util.list)

  (export-all))
(select-module sb-text)


;;; ----


(define *text:story*
  "
  ここはS式コロシアム。
  手下のS式剣闘士をコロシアムに送り込み、
  コロシアムを牛耳るS式剣王とその手下を倒しましょう。
  ")


(define *text:brief*
  "
  戦闘アルゴリズムを自分で書いて、
  それを敵と戦わせるゲームです。
  戦闘アルゴリズムはSchemeで記述する必要があります。
  戦闘はジャンケンベースです。
  尚、戦闘はリアルタイムに進行していきます。
  戦術判定中でも時間が進行していくので、
  アルゴリズムが複雑だと、
  相手に遅れを取る事になります。
  また、戦闘にはランダム要素は含まれないので、
  同じキャラで何度対戦しても、常に同じ結果になります。

  一人プレイでは、事前に用意された相手五人を順に戦います。
  フリー対戦では、入力した二つのS式を戦わせます。
  ")


(define *html:rule*
  (html:ul
    (html:li "対戦者二人の相性によって、"
             "どちらの評価が先に開始されるかが決定します。"
             (html:br)
             "その後、スレッド的に両者の評価が進みます。"
             )
    (html:li "初期HPは100です。")
    (html:li
      "以下の三つの武器のどれかを選んで、相手を攻撃します。"
      (html:ul
        (html:li "Gアックス"
                 (html:br)
                 "丈夫で大きな石斧です。"
                 (html:br)
                 "破壊力がやや高いです。"
                 (html:br)
                 "Cブレードの攻撃をはじきつつ攻撃できますが、"
                 "Pシールドには受け流されてしまいます。"
                 (html:br)
                 "Gアックス同士では普通にダメージを与えます。"
                 )
        (html:li "Cブレード"
                 (html:br)
                 "鋭い刃と輪状の柄を持つ、中央に小さな穴の空いた、"
                 "小柄の刃物です。"
                 (html:br)
                 "本来は二枚一組で使います。"
                 (html:br)
                 "攻撃速度が比較的早めです。"
                 (html:br)
                 "Gアックスには受け流されてしまいますが、"
                 "Pシールドには盾の横から刃を入れて攻撃できます。"
                 (html:br)
                 "Cブレード同士では普通にダメージを与えます。"
                 ;"Cブレード同士は鍔競り合いになってダメージを与えられません。"
                 )
        (html:li "Pシールド"
                 (html:br)
                 "受け流しに特化した、大きな正方形の盾です。"
                 (html:br)
                 "Gアックスを余裕で受け流せますが、"
                 "Cブレードを回避する事はできません。"
                 (html:br)
                 "Pシールドは他の武器と違い、構えるのに時間がかかりますが、構えたまま考える事が出来ます。"
                 (html:br)
                 "Pシールドを選んだ場合、相手にダメージを与える事は"
                 "できませんが、Gアックスの攻撃を受け流した場合、"
                 "相手に隙(wait)ができるので、"
                 "すかさず他の武器で攻撃しましょう。"
                 )
        ))
    (html:li "違う武器を構える時には、少し隙があります。同じ武器で連続攻撃する時には隙はありません。"
             (html:br)
             "この隙はゲーム内では単なるwaitとして表現されています。")
    (html:li "前述の構え変更の隙とは別に、武器攻撃を選択してから実際に攻撃するまでに一定時間かかります。これもwait表現です。")
    (html:li "更に、実際の攻撃後から次に行動可能になるまでにも一定時間かかります。これもwait表現です。")
    (html:li "battle-main手続きがエラー例外を返すと、ペナルティとして無防備状態になり、しばらく硬直した後に、battle-main手続きが再度呼ばれます。")
    (html:li "一定ステップ経過後に勝負が付いていない場合、両者の残りHPで勝負を判定します。それも同じだった場合は後攻の勝ちとします。")
    ))


(define *html:explain*
  (html:ul
    (html:li (html:code "(action-g)")
             (html:br)
             "Gアックスを構え、敵を殴ります。"
             "Pシールドを殴ると受け流され、一定時間無防備になります。"
             "合計で約100ステップ費されます。")
    (html:li (html:code "(action-c)")
             (html:br)
             "Cブレードを構え、敵を刺します。"
             "Gアックスを構えた相手には通用しません。"
             "合計で約70ステップ費されます。")
    (html:li (html:code "(action-p)")
             (html:br)
             "Pシールドを構えます。攻撃はしません。"
             "Cブレードの攻撃は防げません。"
             "構えるまでに約30ステップ費されます。")
    (html:li (html:code "(action-off . opt-wait-step)")
             (html:br)
             "自分から無防備状態になります。"
             "ステップ数を指定すると、無防備状態になった後、そのステップ数だけ"
             "そのまま待機します。"
             "通常は使いません。")
    (html:li (html:code "(wait step)")
             (html:br)
             "現在の構えのまま、指定ステップ数だけ待機します。"
             "主に、Pシールドを維持するのに使います。")
    (html:li ;(html:code "(say \"...\" . opt-prior)")
             (html:code "(say \"text ...\")")
             (html:br)
             "テキストを叫びます。"
             "これは、ゲームを盛り上げる為のフィーチャーです。"
             (html:br)
             "また、S式のデバッグにも使えます。"
             ;(html:br)
             ;"opt-priorには0-7の数値を指定します。"
             ;"数値が大きいほど大声で叫びます。"
             ;"デフォルトは4です。"
             ;"0の場合は台詞ではなく、状況描写になります。"
             )
    (html:li (html:code "(self-info)")
             (html:br)
             "現時点での自分自身の情報を取得します。"
             "情報は、以下のようなlistとして得られます。"
             (html:br)
             (html:code "'(hp action wait bonus-flag)")
             (html:br)
             "hpは現在の体力(最大100)、"
             "actionは現在の構え('g 'c 'p #fのいずれか)、"
             "waitは常に0、"
             "bonus-flagは現在は常に#fです。"
             )
    (html:li (html:code "(enemy-info)")
             (html:br)
             "現時点での敵の情報を取得します。"
             "情報の形式は、基本的には(self-info)と同じです。"
             (html:br)
             "waitは、次に敵が行動可能になるまでのステップ数を示します。"
             "尚、(action-g)等の行動は、「構えフェーズ」「行動前モーションフェーズ」「行動後モーションフェーズ」に分かれており、waitで得られるステップ数は各フェーズ単体の時間になります。各フェーズの合計値ではない事に注意してください。"
             )
    (html:li (html:code "(info->hp (enemy-info))")
             (html:br)
             (html:code "(info->action (enemy-info))")
             (html:br)
             (html:code "(info->wait (enemy-info))")
             (html:br)
             (html:code "(info->bonus-flag (enemy-info))")
             (html:br)
             "単なるユーティリティ手続きです。"
             "実体はcar cadr caddr cadddrそのものです。"
             )
    ))


(define *html:freeplay-sample*
  (html:dl
    (html:dt "ひたすらG斧")
    (html:dd
      (html:pre
        (html:code
          (intersperse
            "\n"
            `("(define (battle-main)"
              "  (action-g)"
              "  (battle-main))"
              )))))
    (html:dt "ひたすらC剣")
    (html:dd
      (html:pre
        (html:code
          (intersperse
            "\n"
            `("(define (battle-main)"
              "  (action-c)"
              "  (battle-main))"
              )))))
    (html:dt "G斧とC剣を交互に使う")
    (html:dd
      (html:pre
        (html:code
          (intersperse
            "\n"
            `("(define (battle-main)"
              "  (action-g)"
              "  (action-c)"
              "  (battle-main))"
              )))))
    (html:dt "G斧→C剣→P盾")
    (html:dd
      (html:pre
        (html:code
          (intersperse
            "\n"
            `("(define (battle-main)"
              "  (action-g)"
              "  (action-c)"
              "  (action-p)"
              "  (wait 30)"
              "  (battle-main))"
              )))))
    (html:dt "相手がP盾ならC剣を、それ以外ならG斧を振る")
    (html:dd
      (html:pre
        (html:code
          (intersperse
            "\n"
            `("(define (battle-main)"
              "  (if (eq? 'p (info->action (enemy-info)))"
              "    (action-c)"
              "    (action-g))"
              "  (battle-main))"
              )))))
    (html:dt "相手が無防備ならG剣を、それ以外ならP盾を構える")
    (html:dd
      (html:pre
        (html:code
          (intersperse
            "\n"
            `("(define (battle-main)"
              "  (if (info->action (enemy-info))"
              "    (action-p)"
              "    (action-g))"
              "  (battle-main))"
              )))))
    (html:dt "後出し(不完全)")
    (html:dd
      (html:pre
        (html:code
          (intersperse
            "\n"
            `("(define (battle-main)"
              "  (case (info->action (enemy-info))"
              "    ((g)"
              "     (niyari)"
              "     (let loop ()"
              "       (if (eq? 'g (info->action (enemy-info)))"
              "         (begin (action-p) (loop))"
              "         (action-g))))"
              "    ((c) (niyari) (action-g))"
              "    ((p) (niyari) (action-c))"
              "    (else (wait 1)))"
              "  (battle-main))"
              "(define (niyari)"
              "  (say \"貴様の行動、見切ったり！\"))"
              )))))
    (html:dt "おしゃべり")
    (html:dd
      (html:pre
        (html:code
          (intersperse
            "\n"
            `(";; 喋る判定に負荷を取られる為、"
              ";; 戦闘アルゴリズム的には全く同じ筈の"
              ";; 「ひたすらG斧」と戦っても負けます。"
              "(define (battle-main)"
              "  (talk)"
              "  (action-g)"
              "  (battle-main))"
              "(define (talk)"
              "  (let ((hp-self (info->hp (self-info)))"
              "        (hp-enemy (info->hp (enemy-info))))"
              "    (cond"
              "      ((<= hp-enemy 10) (say \"これでとどめだ！\"))"
              "      ((<= hp-self 10) (say \"ウボァー\"))"
              "      ((< hp-enemy hp-self) (say \"フッ、弱い奴め\"))"
              "      ((and"
              "         (= hp-enemy 100)"
              "         (= hp-self 100))"
              "       (say \"さあ来い！\"))"
              "      ((= hp-enemy hp-self) (say \"互角の勝負か！\"))"
              "      ((<= hp-self 20) (say \"ヒィーッ！\"))"
              "      ((<= hp-self 30) (say \"た……助けてくれぇーッ\"))"
              "      ((<= hp-self 40) (say \"こっ……殺されるッ！\"))"
              "      ((<= hp-self 50) (say \"て、手強い……\"))"
              "      ((<= hp-self 80) (say \"なんだ、コイツは……\"))"
              "      (else #f))))"
              )))))
    ))


(define *html:history*
  (html:ul
    (html:li (html:code "2008/04/13")
             (html:br)
             "メモリ過使用のエラー時にも、"
             "「致命的エラー」画面を出すようにしました。"
             )
    (html:li (html:code "2008/03/10 (2)")
             (html:br)
             "いくらなんでも20秒近くもかかるのはまずいという事で、"
             "battle-main手続きが終了したらペナルティを与えるように"
             "仕様を変更しました。"
             (html:br)
             "それに伴い、説明文章及びサンプルコードも変更しました。"
             )
    (html:li (html:code "2008/03/10 (1)")
             (html:br)
             "「(define (battle-main))」等、キャラクタ二人ともが"
             "極度に少ないステップ数でbattle-main手続きが終了する場合、"
             "eval/svに突入/脱出する際のオーバーヘッドだけが大きくなり、"
             "時間がかかった挙句にタイムアウトしてしまう問題に対応しました。"
             (html:br)
             "(ただ単にタイムアウト秒数を伸ばしただけなので、"
             "サーバの過負荷状況によっては、"
             "これでもタイムアウトする場合があります。"
             "その時はごめんなさい。"
             "今考えると、battle-main手続きが終了したらペナルティを与えた方が"
             "良かった気はしています。)"
             )
    ))


(provide "sb-text")

