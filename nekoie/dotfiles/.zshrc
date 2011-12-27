# $Id: .zshrc 141 2006-02-27 05:29:51Z nekoie $

# 前提 : ログインシェルはbashとし、.bash_loginの中でexec zshされる。
# 前提 : .zshrcの中でSHELL=zshし、子プロセスはbashを起動しないようにする。

# 前提 : rootは、.bash_loginは無し（安全の為）。zshも手動起動。


# zshオプションの設定
if [ -r ~/.zsh/options ]; then
  source ~/.zsh/options
fi

# 環境変数の設定
if [ -r ~/.zsh/environ ]; then
  source ~/.zsh/environ
fi

# 関数の定義
if [ -r ~/.zsh/functions ]; then
  source ~/.zsh/functions
fi

# emacs likeな操作体系にする（$EDITORがvi系な場合、明示的に指定する必要がある）
bindkey -e


# 履歴設定
HISTFILE=$HOME/.zsh-history           # 履歴をファイルに保存する
HISTSIZE=65536                        # メモリ内の履歴の数
SAVEHIST=65536                        # 保存される履歴の数


# gccやcvs等の第二引数補完機能
autoload -U compinit
compinit


# サーバ別環境変数設定
case "${HOSTNAME}" in
  *.so.tir.jp)
    umask 022
    export CVS_RSH=ssh
    export CVSROOT=:ext:nekoie@sv.tir.ne.jp:/home/cvs/cvsroot
    export MAIL=~/Maildir
    screen_sorendition="= wk"
    COLOR_PROMPT=$COLOR_GREEN
    prompt_gorgeous
    ;;
  "m.sc.tir.ne.jp")
    umask 022
    unset CVS_RSH
    export MAIL=~/Maildir
    screen_sorendition="= yk"
    COLOR_PROMPT=$COLOR_GREEN
    #prompt_simple
    prompt_gorgeous
    ;;
  "cf-m34.tir")
    umask 022
    export CVS_RSH=ssh
    export CVSROOT=:ext:nekoie@sv.tir.ne.jp:/home/cvs/cvsroot
    export MAIL=~/Maildir
    screen_sorendition="= bw"
    COLOR_PROMPT=$COLOR_GREEN
    prompt_gorgeous
    ;;
  "mana")
    umask 022
    unset CVS_RSH
    export CVSROOT=/home/cvs/cvsroot
    export MAIL=~/Maildir
    screen_sorendition="= gk"
    COLOR_PROMPT=$COLOR_GREEN
    s -X hardstatus ignore > /dev/null 2>&1
    prompt_gorgeous
    ;;
  *)
    umask 022
    export CVS_RSH=ssh
    export CVSROOT=:ext:nekoie@sv.tir.ne.jp:/home/cvs/cvsroot
    export MAIL=~/Maildir
    screen_sorendition="= wk"
    COLOR_PROMPT=$COLOR_GREEN
    #prompt_simple
    prompt_gorgeous
    ;;
esac

# 一般ユーザとrootユーザで違いを出したい設定
if [ ${UID} = 0 ]; then
  umask 022 # rootは常に022
  COLOR_PROMPT=$COLOR_RED
  prompt_gorgeous
fi

# 通常時のラベルを求めておく
case ${TERM} in
  *term*|vt100)
    X_LABEL_NORMAL="\033]0;${USER}@${HOST}\007" # kterm title change
    ;;
  screen*)
    X_LABEL_NORMAL="\033k${USER}@${HOST}\033\\" # child screen label change
    # ついでなので、screen回りの追加設定も行う
    s -X caption always > /dev/null 2>&1
    s -X sorendition ${screen_sorendition} > /dev/null 2>&1
    ;;
  *)
    X_LABEL_NORMAL=""
    ;;
esac

# ulimit
ulimit -Sc 0

#if [ "${SSH_CONNECTION}" -o "${REMOTEHOST}" ]; then
#  # 遠隔接続
#else
#  # ローカル接続
#fi

# 補完失敗時の冗長表示を有効にする
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*' group-name ''

# gitの未push検出ラッパー
alias git=gitwrap

# 以下は、エラーが出るかもしれないコマンド実行群
# dircolors設定
eval `dircolors -z 2> /dev/null` 2> /dev/null
# screen -lsの実行
s -ls 2> /dev/null


# vim:set ft=zsh sw=2 ts=2 et:
