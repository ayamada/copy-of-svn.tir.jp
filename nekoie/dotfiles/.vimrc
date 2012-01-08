" $Id$

" 非香り屋版vim対応
if filereadable($HOME . '/.vimrc_kaoriya')
  source $HOME/.vimrc_kaoriya
endif

hi strong term=reverse cterm=reverse gui=reverse
match strong /　\|\t/
"         註：↑ココに全角スペースがあります。注意

set nocp
set history=256
set termencoding=utf-8
set encoding=utf-8
set ambiwidth=double
set fileencodings-=ucs-2le,ucs-2
set fileformats=unix,dos,mac
"set list
"set listchars=tab:>-,extends:<,trail:-,eol:<
set nobackup
set ts=8
set sts=2
set sw=2
"set expandtab
"set lazyredraw
set cpo+=$

if has('migemo')
  set migemo
endif
if has('win32')
  map <M-Space> :simalt ~<CR>
  set guioptions+=a
endif
if has('im_custom/skk')
  set imoptions=skk,dict:~/.skk-jisyo
  "set imoptions=skk,serv:127.0.0.1,port:1178
endif

set statusline=%<%f\ %m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).']['.&ff.']'.SkkGetModeStr()}%=%l,%c%V%8P

" for gauche
let is_gauche=1
set dictionary=~/.gosh_completions

" skk.vim
let skk_jisyo = '~/.skk-jisyo'
let skk_large_jisyo = '/usr/share/skk/SKK-JISYO.L'
let skk_auto_save_jisyo = 1
let skk_keep_state = 0
let skk_egg_like_newline = 1
let skk_show_annotation = 1
let skk_use_face = 1
let skk_remap_lang_mode = 1

" vim:set sw=2 ts=2 et:
