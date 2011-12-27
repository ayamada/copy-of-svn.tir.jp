# $Id: .zshrc 141 2006-02-27 05:29:51Z nekoie $

# ���� : �����󥷥����bash�Ȥ���.bash_login�����exec zsh����롣
# ���� : .zshrc�����SHELL=zsh�����ҥץ�����bash��ư���ʤ��褦�ˤ��롣

# ���� : root�ϡ�.bash_login��̵���ʰ����ΰ١ˡ�zsh���ư��ư��


# zsh���ץ���������
if [ -r ~/.zsh/options ]; then
  source ~/.zsh/options
fi

# �Ķ��ѿ�������
if [ -r ~/.zsh/environ ]; then
  source ~/.zsh/environ
fi

# �ؿ������
if [ -r ~/.zsh/functions ]; then
  source ~/.zsh/functions
fi

# emacs like������ηϤˤ����$EDITOR��vi�Ϥʾ�硢����Ū�˻��ꤹ��ɬ�פ������
bindkey -e


# ��������
HISTFILE=$HOME/.zsh-history           # �����ե��������¸����
HISTSIZE=65536                        # �����������ο�
SAVEHIST=65536                        # ��¸���������ο�


# gcc��cvs������������䴰��ǽ
autoload -U compinit
compinit


# �������̴Ķ��ѿ�����
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

# ���̥桼����root�桼���ǰ㤤��Ф���������
if [ ${UID} = 0 ]; then
  umask 022 # root�Ͼ��022
  COLOR_PROMPT=$COLOR_RED
  prompt_gorgeous
fi

# �̾���Υ�٥����Ƥ���
case ${TERM} in
  *term*|vt100)
    X_LABEL_NORMAL="\033]0;${USER}@${HOST}\007" # kterm title change
    ;;
  screen*)
    X_LABEL_NORMAL="\033k${USER}@${HOST}\033\\" # child screen label change
    # �Ĥ��ǤʤΤǡ�screen�����ɲ������Ԥ�
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
#  # �����³
#else
#  # ��������³
#fi

# �䴰���Ի��ξ�Ĺɽ����ͭ���ˤ���
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*' group-name ''

# git��̤push���Х�åѡ�
alias git=gitwrap

# �ʲ��ϡ����顼���Ф뤫�⤷��ʤ����ޥ�ɼ¹Է�
# dircolors����
eval `dircolors -z 2> /dev/null` 2> /dev/null
# screen -ls�μ¹�
s -ls 2> /dev/null


# vim:set ft=zsh sw=2 ts=2 et:
