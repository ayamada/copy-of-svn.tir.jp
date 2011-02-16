#!/home/nekoie/bin/gosh
;#!/usr/local/gauche/bin/gosh
;#!/usr/local/gauche/bin/speedygosh

(define-module jwhois.cgi
  (use www.cgi)
  (use rfc.http)
  (use text.html-lite)
  (use text.tree)
  (use srfi-1)
  (use srfi-13)
  (use rfc.uri)
  (use rfc.cookie)
  (use util.list)
  (use gauche.charconv)
  (use gauche.process)
  (export
    ))
(select-module jwhois.cgi)

(define *jwhois-path* "/home/nekoie/bin/jwhois")

(define *option-list* '())
(define-macro (define-option option)
  (set! *option-list* (cons option *option-list*))
  '(values))


(define-option "")
(define-option "-h whois.jprs.jp")


(define (main args)
  (set! (port-buffering (current-error-port)) :line)
  (cgi-main
    (lambda (params)
      (emit-content params))
    :on-error (lambda (e)
                (list
                  (cgi-header)
                  (html:pre
                    (html-escape-string
                      (call-with-output-string
                        (cut with-error-to-port <> (cut report-error e))))))))
  0)


(define (sanitize-query query)
  ;; 許可する文字は、ドメイン名、ip、JPNIC HANDLE、で利用可能な文字のみ
  (regexp-replace-all #/[^\w\.\/\-]/ query ""))


(define (emit-content params)
  (let ((query (cgi-get-parameter "q"
                                  params
                                  :default ""
                                  ;; サニタイズする
                                  :convert sanitize-query))
        (option (cgi-get-parameter "o"
                                   params
                                   :default ""
                                   ;; 規定のオプションのみを有効とする
                                   :convert (lambda (o)
                                              (or
                                                (find
                                                  (cut equal? o <>)
                                                  *option-list*)
                                                "")))))
    (list
      (cgi-header :content-type "text/html; charset=EUC-JP"
                  :pragma "no-cache"
                  :cache-control "no-cache"
                  )
      (html:html
        (html:head
          (html:title "jwhois.cgi")
          )
        (html:body :id "the-body"
                   (the-form query option)
                   (html:hr)
                   (the-result query option)
                   )))))

(define (javascript . scripts)
  (html:script
    :type "text/javascript"
    (intersperse
      "\n"
      `("<!--"
        ,@scripts
        "// -->"))))

(define (the-form query option)
  (define (selected? value)
    (if (equal? option value)
      "selected"
      #f))
  (define (make-option value)
    (html:option :value value :selected (selected? value) value))

  (html:form
    :action (self-url)
    :method "post"
    :target "_self"
    :name "send"
    (html:div
      "$ jwhois "
      (html:select :name "o" (map make-option (reverse *option-list*)))
      " "
      (html:input :type "text" :name "q" :value query)
      (html:input :type "submit" ;:name "submit"
                  :id "post-submit" :value "enter")
      )
    (javascript "self.document.send.q.focus();")
    ))

(define (option->list option)
  (if (equal? "" option)
    '()
    (string-split option #/\s+/)))

(define (hes/sp str)
  (regexp-replace-all
    #/\s/
    (html-escape-string str)
    "&nbsp;"))

(define (the-result query option)
  ;; queryが空の場合は、説明文を出す
  (if (equal? "" query)
    (the-help)
    (let1 cmd `(,*jwhois-path* ,@(option->list option) ,query)
      (html:div
        (html:tt
          (intersperse
            (html:br)
            (map hes/sp (process-output->string-list cmd :encoding "*JP"))))))))

(define (the-help)
  (list
    (html:p
      (html:a
        :href "#"
        "explanation(japanese)"))
    (html:div
      (html:tt
        (intersperse
          (html:br)
          (map hes/sp (process-output->string-list
                                  `(,*jwhois-path* --version :encoding "*JP"))))))))


(define (append-params-to-url url params)
  (if (null? params)
    url
    (receive (url-without-fragment fragment) (let1 m (#/(\#.*)/ url)
                                               (if m
                                                 (values (m 'before) (m 1))
                                                 (values url "")))
      (call-with-output-string
        (lambda (p)
          (letrec ((delimitee (if (#/\?/ url-without-fragment)
                                (lambda () "&")
                                (lambda ()
                                  (set! delimitee (lambda () "&"))
                                  "?"))))
            (display url-without-fragment p)
            (let loop ((left-params params))
              (if (null? left-params)
                (display fragment p)
                (let ((key-encoded (uri-encode-string (caar left-params)))
                      (vals (cdar left-params))
                      (next-left (cdr left-params))
                      )
                  (if (pair? vals)
                    (for-each
                      (lambda (val)
                        (display (delimitee) p) ; "?" or "&"
                        (display key-encoded p)
                        (display "=" p)
                        (display (uri-encode-string (if (string? val) val "")) p))
                      vals)
                    (begin
                      (display (delimitee) p)
                      (display key-encoded p)))
                  (loop next-left))))))))))

(define (completion-uri uri server-name server-port https)
  (receive (uri-scheme
            uri-userinfo
            uri-hostname
            uri-port
            uri-path
            uri-query
            uri-fragment) (uri-parse uri)
    ;; uri-schemeが無い時にだけ補完する
    ;; 但し、server-nameが与えられていない場合は補完できないので、何もしない
    (if (or uri-scheme (not server-name))
      uri
      (let* ((scheme (if https "https" "http"))
             (default-port (if https 443 80))
             )
        (uri-compose
          :scheme scheme
          :userinfo uri-userinfo
          :host server-name
          :port (and
                  server-port
                  (not (eqv? default-port (x->number server-port)))
                  server-port)
          :path uri-path
          :query uri-query
          :fragment uri-fragment)))))

(define (path->url path)
  (if (#/^\// path)
    (completion-uri
      path
      (cgi-get-metavariable "SERVER_NAME")
      (cgi-get-metavariable "SERVER_PORT")
      (cgi-get-metavariable "HTTPS"))
    path))

(define (self-url)
  (path->url (self-path)))

(define (self-url/path-info)
  (path->url (self-path/path-info)))

(define (self-url/slash)
  (string-append (self-url) "/"))

(define (self-path)
  (or (cgi-get-metavariable "SCRIPT_NAME") "/"))

(define (self-path/path-info)
  ;; note: PATH_INFOは既にデコードされてしまっているので使わない事
  (let* ((r (or (cgi-get-metavariable "REQUEST_URI") "/"))
         (m (#/\?/ r))
         )
    (if m
      (m 'before)
      r)))

(define (self-path/slash)
  (string-append (self-path) "/"))




;;;===================================================================

(select-module user)
(define main (with-module jwhois.cgi main))

;; Local variables:
;; mode: scheme
;; end:
;; vim: set ft=scheme:
