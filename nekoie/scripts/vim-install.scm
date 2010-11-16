#!/usr/bin/env gosh
;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme sw=2 ts=2 et:
;;; $Id$

;;; usage : INSTALLDIR=/usr/local CFLAGS="-O2" gosh vim-install.scm

;;; WARN : require skk dictionary tool if you use im_custom/skk.
;;;        http://openlab.jp/skk/tools-ja.html

;;; memo : require wget.
;;; memo : require qkc and libc iconv(or libiconv).

;;; ToDo : treat command-line option as configure option

;;; ---- for customize

(define *vim-build-directory*
  (or
    (sys-getenv "BUILDDIR")
    "./vim_build"))
(define *vim-install-prefix*
  (or
    (sys-getenv "INSTALLDIR")
    "/usr"))
(define *vim-cflags*
  (or
    (sys-getenv "CFLAGS")
    "-Os -march=i686 -g"))
(define *vim-configure-option*
  '(
    "--enable-cscope"
    "--enable-multibyte"
    "--without-x"
    "--disable-gui"
    "--disable-gpm"
    "--disable-xim"
    "--with-tlib=ncurses"
    "--disable-nls"
    "--enable-skk"
    "--disable-canna"
    "--disable-pobox"
    "--enable-perlinterp"
    "--disable-pythoninterp"
    "--disable-rubyinterp"
    ;"--program-transform-name=vim63"
    ;"--with-vim-name=vim63"
    ))
(define *vim-major-version* 6)
(define *vim-minor-version* 4)
(define *vim-patch-version* 000)

(define *vim-im_custom-version* 074)

;;; ----

(define *vim-src-directory*
  (format
    "vim~d~d"
    *vim-major-version*
    *vim-minor-version*))
(define *vim-src-tarball*
  (format
    "vim-~d.~d.tar.bz2"
    *vim-major-version*
    *vim-minor-version*))
(define *vim-src-tarball-url*
  (format
    "http://ftp.vim.org/pub/vim/unix/~a"
    *vim-src-tarball*))

(define *vim-extra-tarball*
  (format
    "vim-~d.~d-extra.tar.gz"
    *vim-major-version*
    *vim-minor-version*))
(define *vim-extra-tarball-url*
  (format
    "http://ftp.vim.org/pub/vim/extra/~a"
    *vim-extra-tarball*))

(define *vim-lang-tarball*
  (format
    "vim-~d.~d-lang.tar.gz"
    *vim-major-version*
    *vim-minor-version*))
(define *vim-lang-tarball-url*
  (format
    "http://ftp.vim.org/pub/vim/extra/~a"
    *vim-lang-tarball*))

(use srfi-1) ; iota
(define *vim-patch-files*
  (map
    (lambda (p)
      (format "~d.~d.~3,'0d" *vim-major-version* *vim-minor-version* p))
    (iota *vim-patch-version* 1)))
(define *vim-patch-urls*
  (map
    (lambda (patch)
      (format
        "http://ftp.vim.org/pub/vim/patches/~d.~d/~a"
        *vim-major-version*
        *vim-minor-version*
        patch))
    *vim-patch-files*))

(define *vim-kaoriya-directory*
  ;(format
  ;  "vim-~d.~d.~3,'0d-difj"
  ;  *vim-major-version*
  ;  *vim-minor-version*
  ;  *vim-patch-version*)
  (format
    "vim-~d.~d-difj"
    *vim-major-version*
    *vim-minor-version*)
  )
(define *vim-kaoriya-tarball*
  (format
    "~a.tar.bz2"
    *vim-kaoriya-directory*))
(define *vim-kaoriya-tarball-url*
  (format
    "http://www.kaoriya.net/dist/~a"
    *vim-kaoriya-tarball*))

(define *vim-im_custom-difffile*
  (format
    "im_custom~3,'0d.diff.gz"
    *vim-im_custom-version*))
(define *vim-im_custom-difffile-url*
  (format
    "http://hp.vector.co.jp/authors/VA020411/Vim/im_custom/~a"
    *vim-im_custom-difffile*))

(define *vim-other-patches*
  '(
    ("-p1" "http://d.tir.jp/vim-6.3-im_custom074-dirty-dictmerge.patch")
    ("-p0" "http://d.tir.jp/im_custom074_extend_arrow.patch")
    ("-p0" "http://d.tir.jp/vim-6.3-kaoriya-remove-ucs.patch")
    ("-p0 -d .." "http://d.tir.jp/vimirc.vim-0.9.10-fix-jis-motd.patch")
    ))

(define *vim-scheme.vim-url*
  "http://d.tir.jp/scheme.vim")

(define *vim-vimirc.vim-url*
  "http://www.vim.org/scripts/download_script.php?src_id=3955" ; 0.9.10
  ;"http://www.vim.org/scripts/download_script.php?src_id=4171" ; 0.9.28
  )



(define (sys-system-format-or-die command . params)
  (let1 command-string (apply format command params)
    (or
      (= 0 (sys-system command-string))
      (errorf "'~a' failed." command-string))))



;; ファイル名を明示的に指定してwget
(define (url-fetch/filename! url filename)
  (if (file-exists? filename)
    (format #t "'~a' exists.\n" filename)
    (or
      (= 0 (sys-system
             (format "wget -O '~a' '~a'" filename url)))
      (errorf "cannot fetch url '~a'" url))))

;; wget
(define (url-fetch! url)
  (url-fetch/filename! url (sys-basename url)))

;; ファイル名を明示的に指定して、常に最新のファイルをwget
(define (url-fetch/filename-newest! url filename)
  (sys-unlink filename)
  (url-fetch/filename! url filename))

;; 常に最新のファイルをwget
(define (url-fetch-newest! url)
  (url-fetch/filename-newest! url (sys-basename url)))




;; 必要なファイル一式をダウンロード
(define (fetch-tarballs!)
  (for-each
    url-fetch!
    (list*
      ;; 基本
      *vim-src-tarball-url*
      *vim-extra-tarball-url*
      *vim-lang-tarball-url*
      ;; 香り屋パッチとim_custom
      *vim-kaoriya-tarball-url*
      *vim-im_custom-difffile-url*
      ;; im_custom修正パッチ等
      (map
        cadr
        *vim-other-patches*)))
  ;; scheme.vimとvimirc.vim
  (url-fetch/filename-newest! *vim-scheme.vim-url* "scheme.vim")
  (url-fetch/filename-newest! *vim-vimirc.vim-url* "vimirc.vim")
  )

(define (fetch-patches!)
  (make-directory* "patches")
  (current-directory "patches")
  (for-each
    url-fetch!
    *vim-patch-urls*)
  (current-directory ".."))

(define (clean-old-directories!)
  (for-each
    (lambda (d)
      (when (file-is-directory? d)
        (remove-directory* d)))
    (list
      *vim-src-directory*
      *vim-kaoriya-directory*)))

(define (unzip-tarball tarball)
  (let1 unzip-command (cond
                        ((#/\.gz$/ tarball) "gzip")
                        ((#/\.bz2$/ tarball) "bzip2")
                        ((#/\.tgz$/ tarball) "gzip")
                        ((#/\.tbz$/ tarball) "bzip2")
                        (else "gzip"))
    (sys-system-format-or-die
      "~a -dc ~a | tar xf -"
      unzip-command
      tarball)))

(define (unzip-tarballs!)
  (for-each
    unzip-tarball
    (list
      *vim-src-tarball*
      *vim-extra-tarball*
      *vim-lang-tarball*
      *vim-kaoriya-tarball*)))

(define (apply-patch patchoption patchfile)
  (let1 decompress-command (cond
                      ((#/\.gz$/ patchfile) "gzip -dc")
                      ((#/\.bz2$/ patchfile) "bzip2 -dc")
                      (else "cat"))
    (sys-system-format-or-die
      "cat ~a | ~a | patch ~a"
      patchfile
      decompress-command
      patchoption
      )))


(define (patching!)
  ;; vim
  (for-each
    (lambda (p)
      (apply-patch
        "-p0"
        (format "../patches/~a" p)))
    *vim-patch-files*)

  ;; kaoriya
  (sys-system-format-or-die "cp -dpRf ../~a/* ." *vim-kaoriya-directory*)
  ;(apply-patch
  ;  "-p0"
  ;  (format "../~a/diffs/kaoriya.diff" *vim-kaoriya-directory*))
  ;; ↑im_customは最近更新されてないので、当たらないので、今は諦める

  ;; im_custom
  (apply-patch
    "-p0"
    (format "../~a" *vim-im_custom-difffile*))

  ;; other
  (for-each
    (lambda (po&pf)
      (apply-patch
        (car po&pf)
        (format "../~a" (sys-basename (cadr po&pf)))))
    *vim-other-patches*)
  )


(define (making!)
  ;; configure
  (sys-system-format-or-die
    "CFLAGS=\"~a\" ./configure --prefix=~a ~a"
    *vim-cflags*
    *vim-install-prefix*
    (string-join *vim-configure-option* " "))
  ;; make
  (sys-system-format-or-die "nice make clean all")
  )




;; #tか#fを返す事
(define (installing!)
  (let* (
         (rc-dir (format "~a/share/vim" *vim-install-prefix*))
         (syntax-dir (format "~a/vimfiles/syntax" rc-dir))
         (plugin-dir (format "~a/vimfiles/plugin" rc-dir))
         (su-command (string-join
                       (list
                         ;; ↓これらの文字の中で''を使ってはいけない
                         ;; ↓（su -c '...'で実行する為）
                         "nice make install"
                         (format "mkdir -p ~a ~a" syntax-dir plugin-dir)
                         (format "cp -f vimrc gvimrc ~a" rc-dir)
                         (format "cp -f ../scheme.vim ~a" syntax-dir)
                         (format "cp -f ../vimirc.vim ~a" plugin-dir)
                         )
                       " && \\\n"))
         )
    (with-error-handler
      (lambda (e)
        (newline)
        (display "make install cancelled : ")
        (display "you can continue to execute this commands.")
        (newline)
        (display "$ su")
        (newline)
        (format #t "# cd ~a/~a" *vim-build-directory* *vim-src-directory*)
        (newline)
        (format #t "# ~a" su-command)
        (newline)
        #f)
      (lambda ()
        (sys-system-format-or-die
          "su -c '~a'"
          su-command)
        #t))))




(use file.util) ; make-directory* current-directory
(define (main args)
  ;; prepare 1
  (sys-umask #o022)
  (make-directory* *vim-build-directory*)
  (current-directory *vim-build-directory*)

  ;; prepare 2
  (fetch-tarballs!)
  (fetch-patches!)
  (clean-old-directories!)
  (unzip-tarballs!)

  (current-directory *vim-src-directory*)

  ;; patching
  (patching!)

  ;; make
  (making!)

  ;; install
  (if (installing!)
    0
    1))






