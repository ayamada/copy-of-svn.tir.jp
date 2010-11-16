;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme sw=2 ts=2 et:
;;; $Id$

;;; ToDo : ����ɤ����󥿡��ե�������ͤ����

;;; ToDo : HTTP/1.1����ץ꡼��
;;; ToDo : HTTP/1.1�ɤ��ޤǼ������������ɤν�Ǽ������뤫��ɽ�����

;;; ToDo : Range�إå��Υ��ݡ���
;;; ToDo : HEAD�Υ��ݡ��Ȥȡ����Ĥ��줿method�ʳ���405, 501���֤��褦�ˤ����
;;; ToDo : OPTIONS, TRACE���Υ��ݡ���
;;;        http://www.studyinghttp.net/method

;;; ToDo : apache��mime.types�����Υե������ѡ������ƻȤ��褦�ˤ��롩

;;; memo : ���ߤ�mime-typeȽ�̤ϡ�path�γ�ĥ�ҤǤϤʤ������Τγ�ĥ�Ҥ򸫤ޤ�


;;; ToDo : �ɥåȤϤ��ޤ�Υե������ɽ�������ʤ��褦�ˤ��롩����
;;;        .htaccess���򥵥ݡ��Ȥ��ʤ��ʤ餽���ޤǤ���ɬ�פ�̵���Ȼפ������ġ�
;;;        ��̯��


(define-module tcpcgi.filer
  (use srfi-2) ; and-let*
  (use srfi-13) ; string-prefix?
  (use file.util)
  (use text.tree)
  (use text.html-lite)
  (use www.cgi)

  (use tcpcgi.errordoc)
  (export
    <tcpcgi.filer>
    make-filer-thunk
    *mime-table*
    ))
(select-module tcpcgi.filer)


(define-class <tcpcgi.filer> ()
  (
   (root-path :accessor root-path-of
              :init-keyword :root-path
              :init-value #f)
   (error-proc :accessor error-proc-of
               :init-keyword :error-proc
               :init-value #f)
   (auto-index :accessor auto-index-of
               :init-keyword :auto-index
               :init-value #f)
   (auth :accessor auth-of
         :init-keyword :auth
         :init-value #f)
   ))


;; cgi-thunk
(define (execute-filer self)
  (let* ((root-path (root-path-of self))
         (path-info (or
                      (cgi-get-metavariable "PATH_INFO")
                      "")) ; path-info��/�Ϥ��ޤ�
         (target-path (simplify-path
                        (string-append root-path path-info)))
         (error-proc (error-proc-of self))
         (basename (sys-basename target-path))
         (ext (and-let* ((m (#/\.([^\.]+)$/ basename)))
                (m 1)))
         )
    (cond
      ((not (file-exists? target-path)) (error-proc 404))
      ((file-is-directory? target-path) (error-proc 403))
      ((not (file-is-readable? target-path)) (error-proc 403))
      ((not (string-prefix? root-path target-path)) (error-proc 403))
      (else
        (write-tree
          (apply cgi-header
            :content-type (or
                            (and
                              ext
                              (ref *mime-table* ext #f))
                            *fallback-mime-type*)
            (or
              (and-let* ((response-code
                           (cgi-get-metavariable "X_RESPONSE_CODE"))
                         (response-message
                           (status-code->label response-message))
                         )
                (list :status (format "~a ~a" response-code response-message)))
              '())))
        (copy-port
          (open-input-file target-path)
          (current-output-port))))))



;; cgi-thunk���֤���
(define (make-filer-thunk path . keywords)
  (let* ((root-path (sys-normalize-pathname
                      path
                      :absolute #t
                      :expand #t
                      :canonicalize #t))
         (filer (apply
                  make <tcpcgi.filer>
                  :root-path root-path
                  keywords))
         )
    (lambda ()
      (execute-filer filer))))


;(define-method hoge ((self <tcpcgi.filer>))
;  #f)



(define *fallback-mime-type* "text/plain")
;; ��ĥ��->mime-type
(define *mime-table*
  (hash-table
    'string=?
    '("hqx" . "application/mac-binhex40")
    '("cpt" . "application/mac-compactpro")
    '("mathml" . "application/mathml+xml")
    '("doc" . "application/msword")
    '("bin" . "application/octet-stream")
    '("lzh" . "application/octet-stream")
    '("exe" . "application/octet-stream")
    '("com" . "application/octet-stream")
    '("class" . "application/octet-stream")
    '("so" . "application/octet-stream")
    '("dll" . "application/octet-stream")
    '("ogg" . "application/ogg")
    '("pdf" . "application/pdf")
    '("eps" . "application/postscript")
    '("ps" . "application/postscript")
    '("rdf" . "application/rdf+xml")
    '("xul" . "application/vnd.mozilla.xul+xml")
    '("xls" . "application/vnd.ms-excel")
    '("ppt" . "application/vnd.ms-powerpoint")
    '("dvi" . "application/x-dvi")
    '("js" . "application/x-javascript")
    '("latex" . "application/x-latex")
    '("swf" . "application/x-shockwave-flash")
    '("sit" . "application/x-stuffit")
    '("tex" . "application/x-tex")
    '("texi" . "application/x-texinfo")
    '("tr" . "application/x-troff")
    '("roff" . "application/x-troff")
    '("man" . "application/x-troff-man")
    '("xhtml" . "application/xhtml+xml")
    '("xslt" . "application/xslt+xml")
    '("xml" . "application/xml")
    '("xsl" . "application/xml")
    '("dtd" . "application/xml-dtd")
    '("au" . "audio/basic")
    '("snd" . "audio/basic")
    '("mid" . "audio/midi")
    '("midi" . "audio/midi")
    '("kar" . "audio/midi")
    '("mpga" . "audio/mpeg")
    '("mp2" . "audio/mpeg")
    '("mp3" . "audio/mpeg")
    '("aif" . "audio/x-aiff")
    '("aiff" . "audio/x-aiff")
    '("aifc" . "audio/x-aiff")
    '("m3u" . "audio/x-mpegurl")
    '("ram" . "audio/x-pn-realaudio")
    '("rm" . "audio/x-pn-realaudio")
    '("ra" . "audio/x-realaudio")
    '("wav" . "audio/x-wav")
    '("bmp" . "image/bmp")
    '("gif" . "image/gif")
    '("jpeg" . "image/jpeg")
    '("jpg" . "image/jpeg")
    '("png" . "image/png")
    '("svg" . "image/svg+xml")
    '("tiff" . "image/tiff")
    '("ico" . "image/x-icon")
    '("pnm" . "image/x-portable-anymap")
    '("pbm" . "image/x-portable-bitmap")
    '("pgm" . "image/x-portable-graymap")
    '("ppm" . "image/x-portable-pixmap")
    '("rgb" . "image/x-rgb")
    '("xbm" . "image/x-xbitmap")
    '("xpm" . "image/x-xpixmap")
    '("xwd" . "image/x-xwindowdump")
    '("wrl" . "model/vrml")
    '("vrml" . "model/vrml")
    '("css" . "text/css")
    '("html" . "text/html")
    '("htm" . "text/html")
    '("shtml" . "text/html")
    '("asc" . "text/plain")
    '("txt" . "text/plain")
    '("rtx" . "text/richtext")
    '("rtf" . "text/rtf")
    '("sgml" . "text/sgml")
    '("tsv" . "text/tab-separated-values")
    '("mpeg" . "video/mpeg")
    '("mpg" . "video/mpeg")
    '("mpe" . "video/mpeg")
    '("qt" . "video/quicktime")
    '("mov" . "video/quicktime")
    '("mxu" . "video/vnd.mpegurl")
    '("avi" . "video/x-msvideo")
    ;; added
    '("pls" . "audio/x-scpls")
    '("gz" . "application/octet-stream")
    '("bz2" . "application/octet-stream")
    '("tar" . "application/octet-stream")
    '("zip" . "application/octet-stream")
    '("tgz" . "application/octet-stream")
    '("tbz" . "application/octet-stream")
    ))


(provide "tcpcgi/filer")

