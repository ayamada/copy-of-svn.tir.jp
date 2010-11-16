;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ToDo: text->inline-html��url->href:keywords���ѻߤ��롣
;;;       ����ˡ�url->proc���ɲä���url�Τ褦��ʸ������դ�����
;;;       ���ꤷ��proc���ƤФ��褦�ˤ��롣
;;; ToDo: test case���Ѱ�
;;; ToDo: text->block-html�λ��ͤ�ͤ��ơ��������ɲä���
;;; �ʲ���ʸˡ���б�����������
;;; - text->inline-htmlʬ
;;; - ������� -> html:p
;;; �ʲ����Ѵ��򥵥ݡ��Ȥ��٤�������̯��
;;; - ʸƬ����-�ס�+�ײ��Ĥ��ǻϤޤäƤ��� -> html:ul, html:ol
;;; -- ñ�˿��ͤΥޥ��ʥ��ͤ�ץ饹�ͤ�񤤤Ƥ��������λ��˸�ǧ����������
;;; - ʸƬ������ǻϤޤäƤ��� -> html:pre
;;; -- ������ǧ����������
;;; ������ɰ��������ץ���ʥ�������Ѵ�ˡ§������Ǥ���褦�ˤ��롩
;;; �ܳ�Ū�ˤ����ϡ�wiki��ʸ�κƼ����äݤ��ʤäƤ��ޤ��������롣
;;; �ɤ����٤���

;;; ToDo: http-tree-make��񤭤ޤ��礦��

(define-module tir04.cgi.util
  (use gauche.charconv)
  (use gauche.parameter)
  (use srfi-1)
  (use srfi-2)
  (use rfc.uri)
  (use text.html-lite)
  (use text.tree)
  (use util.list)
  (use www.cgi)

  (export
    html-escape-tree
    het
    html-tree-make
    http-tree-make
    cgi-tree-make
    get-html-tree-keyword-symbols
    get-http-tree-keyword-symbols
    get-cgi-tree-keyword-symbols
    cgi-metavariables->html
    cgi-params->html
    append-params-to-url
    completion-uri
    path->url
    location
    location/fragment
    self-url
    self-url/path-info
    self-url/slash
    self-path
    self-path/path-info
    self-path/slash
    make-form
    cgi-on-error/stack-trace
    ces-convert-cgi-params
    get-path-info-keylist
    with-path-info-keylist
    with-reverse-proxy
    text->inline-html
    text->block-html
    ))
(select-module tir04.cgi.util)


(define (html-escape-tree . params)
  (cond
    ((null? params) '()) ; ����̵��
    ((null? (cdr params))
     (html-escape-string (car params))) ; �������(�֤��ͤ���)
    (else ; ������ʣ��(�֤��ͤ�list)
      (map
        (lambda (x)
          (if (list? x)
            (apply html-escape-tree x)
            (html-escape-string x)))
        params))))
(define het html-escape-tree)


(define-syntax when/null
  (syntax-rules ()
    ((_ pred body ...) (if pred
                         (begin body ...)
                         '()))))

(define-macro (let-keywords** keywords tree-keyword-symbols body . bodies)
  ;; ���Υޥ���Ϥ�Τ�������̯�ʤΤǡ����ȤǤ�äȥޥ�����ˡ��ͤ���ľ����
  `(let-keywords* keywords ,(append
                              (map
                                (lambda (x)
                                  (list x #f))
                                (eval tree-keyword-symbols (current-module)))
                              #f)
     ,body . ,bodies))

(define *html-tree-keyword-symbols*
  '(encoding
    base-url
    css-url
    css-body
    js-url
    js-body
    js
    robots
    title
    title-format
    title-format-args
    html-header
    body
    body-attr
    body-header
    body-footer
    frame-body
    ))

(define (html-tree-make . keywords)
  (let-keywords** keywords *html-tree-keyword-symbols*
    (list
      ;; xml header
      (if encoding
        #`"<?xml version=\"1.0\" encoding=\",|encoding|\"?>\n"
        "<?xml version=\"1.0\"?>\n")
      ;; html doctype
      (html-doctype
        :type (if frame-body
                :xhtml-1.0-frameset
                :xhtml-1.0-transitional))
      ;; html
      (html:html
        :lang "ja-JP"
        :xml:lang "ja-JP"
        :xmlns "http://www.w3.org/1999/xhtml"
        (html:head
          (when/null encoding
            (html:meta :http-equiv "Content-Type"
                       :content #`"text/html; charset=,|encoding|"))
          (when/null base-url
            (html:base :href base-url))
          (when/null (or css-url css-body)
            (html:meta :http-equiv "Content-Style-Type"
                       :content "text/css"))
          (when/null (or js-url js-body js)
            (html:meta :http-equiv "Content-Script-Type"
                       :content "text/javascript"))
          (when/null robots
            (html:meta :name "ROBOTS"
                       :content robots))
          ;; title��ͥ���̤ϡ�title����title-format������ͥ�褹��
          (or
            (and
              title-format
              (guard (e (else #f))
                (html:title
                  (het
                    (apply format #f title-format title-format-args)))))
            (when/null title
              (html:title (het title))))
          (when/null css-url
            (html:link :rel "Stylesheet"
                       :type "text/css"
                       :href css-url))
          (when/null css-body
            (html:style :type "text/css"
                        "<!--\n"
                        css-body
                        "\n-->"
                        ))
          (when/null js-url
            (html:script :type "text/javascript"
                         :src js-url
                         ""))
          (when/null js-body
            (html:script :type "text/javascript"
                         "<!--\n"
                         js-body
                         "\n-->"
                         ))
          (when/null html-header html-header)
          )
        (when/null body
          (apply
            html:body
            (append
              (when/null body-attr body-attr)
              (list
                (when/null body-header body-header)
                body
                (when/null body-footer body-footer)
                ))))
        (when/null frame-body
          frame-body)
        ))))


(define *http-tree-keyword-symbols*
  ;; �ޤ�
  '(
    ))
(define (http-tree-make . keywords)
  ;; ̤����
  (error "not implemented"))


(define *cgi-tree-keyword-symbols*
  '(encoding
    content-type
    location
    http-header
    http-body
    body
    frame-body
    ))
(define (cgi-tree-make . keywords)
  (let-keywords** keywords *cgi-tree-keyword-symbols*
    (if location
      (apply
        cgi-header
        :pragma "no-cache"
        :cache-control "no-cache"
        :location location
        (or http-header '()))
      (let (
            (content-type-is-text (and
                                    content-type
                                    (#/^text\// content-type)))
            (content-type-has-charset (and
                                        content-type
                                        (string-scan content-type #\;)))
            (true-content-type (or content-type "text/html"))
            )
        (list
          (apply
            cgi-header
            :content-type (if content-type-has-charset
                            true-content-type
                            (if (not encoding)
                              true-content-type
                              (string-append
                                true-content-type
                                "; charset="
                                encoding)))
            (or http-header '()))
          (cond
            (http-body http-body)
            ((or body frame-body) (apply html-tree-make keywords))
            (else
              (error
                (string-append
                  "cgi-tree-make must be needed "
                  ":location or :body or :frame-body or :http-body")))))))))


(define (uniq src-list)
  ;; note: ���ΤȤ���eq?�ǤΤ�Ƚ���Ԥ����ͤȤ���
  (let loop ((left src-list)
             (result '()))
    (if (null? left)
      result
      (loop
        (cdr left)
        (if (memq (car left) result)
          result
          (cons (car left) result))))))
(define-syntax define-get-*-tree-keyword-symbols
  (syntax-rules ()
    ((_ proc-name target-list)
     (define proc-name
       (let1 promise (delay (uniq target-list))
         (lambda ()
           (force promise)))))))

(define (get-html-tree-keyword-symbols)
  *html-tree-keyword-symbols*)
(define-get-*-tree-keyword-symbols get-http-tree-keyword-symbols
  (append
    *html-tree-keyword-symbols*
    *http-tree-keyword-symbols*))
(define-get-*-tree-keyword-symbols get-cgi-tree-keyword-symbols
  (append
    *html-tree-keyword-symbols*
    *cgi-tree-keyword-symbols*))


(define (cgi-metavariables->html)
  ;; ToDo: �Ķ��ѿ������CGI�᥿�ѿ�������������
  ;;       (environ�򻲾Ȥ�����ʤ��󶡤��줿���������)
  ;;       �������줽���ˤʤ����ϡ���ʬ��Gauche���Ф���ѥå���
  ;;       �񤤤��������
  ;;       (sys-environ)�Ȥ��ǻ��ȤǤ���褦�ˡ�
  (define (sys-environ) '()) ; ����̵���Τǡ����ߡ���������Ƥ���

  (let* ((env-mv (filter
                   (lambda (x)
                     ;; ToDo: �̾�δĶ��ѿ��ȡ�CGI�᥿�ѿ���
                     ;;       �դ뤤ʬ����ɬ�פ����롣
                     ;;       ���ȤǼ������롣
                     #f)
                   (sys-environ)))
         (mv (delete-duplicates
               (append
                 (or (cgi-metavariables) '())
                 env-mv)
               (lambda (x y)
                 (equal? (car x) (car y))))))
    (html:dl
      (map (lambda (x)
             (list
               (html:dt (het (car x)))
               (html:dd (het (cadr x)))))
           ;; map�ˤ��������ˡ�key�Υ���ե��٥åȽ��sort����
           (sort
             mv
             (lambda (x y)
               (string<? (car x) (car y))))))))

(define (cgi-params->html params)
  ;; note: params�����ܸ��ޤ��ǽ����������ϡ�
  ;;       ������ces-convert-cgi-params�ˤ����Ƥ�������
  (html:dl
    (map
      (lambda (x)
        (list
          (html:dt (het (write-to-string (car x))))
          (map
            (lambda (y)
              (html:dd (het (write-to-string y))))
            (cdr x))))
      params)))



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
            uri-fragment)
    (uri-parse uri)
    ;; uri-scheme��̵�����ˤ����䴰����
    ;; â����server-name��Ϳ�����Ƥ��ʤ������䴰�Ǥ��ʤ��Τǡ����⤷�ʤ�
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


(define (location url)
  (define (chop-url-fragment url)
    (or
      (and-let* ((m (#/\#/ url)))
        (m 'before))
      url))
  (cgi-header
    :pragma "no-cache"
    :cache-control "no-cache"
    :location (chop-url-fragment (path->url url))))


(define (location/fragment url)
  (if (not (#/\#/ url))
    (location url)
    (list
      (cgi-header
        :pragma "no-cache"
        :cache-control "no-cache"
        :content-type "text/html")
      (html-tree-make
        :robots "NOINDEX,NOFOLLOW"
        :html-header (html:meta :http-equiv "Refresh"
                                :content (string-append "0;URL=" url))
        :body (html:div (html:a :href url
                                (het url)))))))


(define (self-url)
  (path->url (self-path)))

(define (self-url/path-info)
  (path->url (self-path/path-info)))

(define (self-url/slash)
  (string-append (self-url) "/"))

(define (self-path)
  (or (cgi-get-metavariable "SCRIPT_NAME") "/"))

(define (self-path/path-info)
  ;; note: PATH_INFO�ϴ��˥ǥ����ɤ���Ƥ��ޤäƤ���ΤǻȤ�ʤ���
  (let* ((r (or (cgi-get-metavariable "REQUEST_URI") "/"))
         (m (#/\?/ r))
         )
    (if m
      (m 'before)
      r)))

(define (self-path/slash)
  (string-append (self-path) "/"))



(define (make-form url hidden-params html-tree . keywords)
  (apply
    html:form
    :action url
    (append
      (when/null (not (get-keyword :method keywords #f))
        '(:method "post"))
      (when/null (not (get-keyword :target keywords #f))
        '(:target "_self"))
      keywords
      (map
        (lambda (key+vals)
          (let1 key (car key+vals)
            (map
              (lambda (val)
                (html:input
                  :type "hidden"
                  :name key
                  :value val))
              (cdr key+vals))))
        hidden-params)
      html-tree)))


(define (cgi-on-error/stack-trace e)
  `(,(cgi-header)
    ,(html-doctype)
    ,(html:html
      (html:head (html:title "Error"))
      (html:body (html:h1 "Error")
                 (html:pre (html-escape-string
                             (call-with-output-string
                               (cut
                                 with-error-to-port
                                 <>
                                 (cut report-error e)))))))))


(define (ces-convert-cgi-params params . keywords)
  (let-keywords* keywords ((from-code #f)
                           (to-code (gauche-character-encoding))
                           (excluded-key-list '())
                           )
    (receive (target-params excluded-params) (partition
                                               (lambda (x)
                                                 (not
                                                   (find
                                                     (lambda (y)
                                                       (equal? y (car x)))
                                                     excluded-key-list)))
                                               params)
      (let1 params-encoding (or
                              from-code
                              (ces-guess-from-string
                                (tree->string target-params) "*JP")
                              "none")
        (define (conv str)
          (ces-convert str params-encoding to-code))
        (append
          excluded-params
          (map
            (lambda (key+vals)
              (cons
                (conv (car key+vals))
                (map
                  (lambda (val)
                    ;; val��#t, #f�ξ��⤢��Τǡ������Ƚ�ꤹ��ɬ�פ�����
                    (if (string? val)
                      (conv val)
                      val))
                  (cdr key+vals))))
            target-params))))))



(define (get-path-info-keylist)
  ;; ���줾��ξ��ǡ��ʲ��Τ褦���ͤ��֤���%xx�Υǥ����ɤϹԤ�ʤ���
  ;; (%xx�Υǥ����ɤ�Ԥ�ʤ��Τϡ��������ƥ���ΰ����ΰ�)
  ;; - /path/to/hoge.cgi          => #f
  ;; - /path/to/hoge.cgi/         => '()
  ;; - /path/to/hoge.cgi/?abc=def => '()
  ;; - /path/to/hoge.cgi/abc/def  => '("abc" "def")
  ;; - /path/to/hoge.cgi/abc/def/ => '("abc" "def")
  ;; - /path/to/hoge.cgi/%20      => '("%20")
  ;; - /path/to/hoge.cgi/a///b    => '("a" "" "" "b")
  ;; WARN: apache2�ϤθŤ��С������Ǥϡ�PATH_INFO��ʬ�˥���å��夬ʣ��
  ;;       Ϣ³����¸�ߤ�����ˡ�SCRIPT_NAME�������Ȥ����Х�������Τǡ�
  ;;       ���������С������Ǥϥ���å��夬ʣ��Ϣ³����褦�ʥ���������
  ;;       ��ʤ��褦�ˤ��ʤ��ƤϤʤ�ʤ���
  ;;       (����Ū�ˡ��������ƥ�Ū�ˤ������̵���Ȼפ��Τǡ�
  ;;        �ä��к������ɤ����줿��Ϥ��ʤ�ͽ�ꡣ)
  ;; WARN: ���ΤȤ�����REQUEST_URI�ϡ����SCRIPT_NAME��prefix�Ȥ��ƴޤ��
  ;;       �Ȥ�����������Ȥ��Ƥ��롣
  ;;       ��~�פ���%7e�פˤ��줿�ꡢ��ʸ����ʸ����Ʊ��뤹��褦��httpd�Ǥ�
  ;;       ����ˤʤ�Τ���դ������
  ;;       ���Τ褦�ʾ��Ͼ��#f���֤��Τǡ�
  ;;       �⤷(get-path-info-keylist)��#f���֤������ϡ���ö
  ;;       (string-append (self-url) "/")
  ;;       �˥�����쥯�Ȥ��Ƥ��������Ԥ��褦�ˤ��٤���
  ;;       (������쥯�ȸ�ʤ顢REQUEST_URI��SCRIPT_NAME�ϰ��פ���Ȼפ���)
  ;; ToDo: REQUEST_URI��������?��̵����#�����ä����ε�ư�б�
  (define (path-info-split path)
    (and-let* ((m (#/^\/(.*?)\/?$/ path))
               (plain-path (m 1))) ; ��Ƭ��������/�����
      (if (string=? plain-path "")
        '()
        (string-split plain-path #\/))))

  (and-let* ((script-name (cgi-get-metavariable "SCRIPT_NAME"))
             (request-uri (cgi-get-metavariable "REQUEST_URI"))
             (re (string->regexp
                   (string-append "^" (regexp-quote script-name))))
             (m (re request-uri))
             (path-info+query (m 'after))
             (result (or
                       (and-let* ((m (#/\?/ path-info+query)))
                         (m 'before))
                       path-info+query)))
    (if (string=? result "")
      #f
      (path-info-split result))))


;; note: ���μ�³����cgi-main��ǸƤӽФ�����
;; ToDo: ������쥯�ȸ��REQUEST_URI��SCRIPT_NAME�����פ��ʤ���硢
;;       ̵�¥�����쥯�ȥ롼�פ�ȯ�����Ƥ��ޤ���
;;       �к���ͤ������
(define (with-path-info-keylist proc)
  (let1 path-info-keylist (get-path-info-keylist)
    (if (or
          (equal? "POST" (cgi-get-metavariable "REQUEST_METHOD"))
          path-info-keylist)
      (proc (or path-info-keylist '()))
      ;; �����ʤ顢location��Ԥ��ݤˤϡ�append-params-to-url��Ȥäơ�
      ;; ����Υꥯ�����Ȥ��Ϥ��줿params��ȿ�Ǥ����Ĥĥ�����쥯�Ȥ���������
      ;; �����ˤ�params���Ϥ���Ƥ��ʤ��Τǡ�����ϤǤ��ʤ��Τǡ�
      ;; params̵���ǥ�����쥯�Ȥ�Ԥ���
      (location (string-append (self-url) "/")))))


(define (with-reverse-proxy server-name server-port thunk)
  (parameterize ((cgi-metavariables
                   (delete-duplicates
                     (list*
                       `("SERVER_NAME" ,(x->string server-name))
                       `("SERVER_PORT" ,(x->string server-port))
                       (or (cgi-metavariables) '()))
                     (lambda (x y)
                       (equal? (car x) (car y))))))
    (thunk)))

(define (google-redirector url)
  (string-append
    "http://www.google.com/url?sa=D&q="
    url))

(define *re:url*
  #/https?\:\/\/[\!\#\$\%\&\'\(\)\*\+\,\-\.\/0-9\:\;\=\?\@A-Z\_a-z\~]+/)
(define *re:linefeed*
  (string->regexp "\r\n|\r|\n"))
(define *re:text->inline-html*
  (string->regexp
    (tree->string
      (intersperse
        "|"
        (map
          (lambda (x)
            (list "(?:" x ")"))
          (list
            (regexp->string *re:linefeed*)
            (regexp->string *re:url*)
            " "
            "\t"
            ))))))
(define (text->inline-html text . keywords)
  ;; �Ѵ����٤���ʸ�ϡ��ʲ����̤ꡣ
  ;; - url���񤤤Ƥ��� -> html:a
  ;; - ���԰�� -> html:br
  ;; - #\space -> &nbsp;
  ;; - #\tab -> &nbsp; * n (�ޤ�̤����)
  ;; �Ĥ��het�ˤ����롣
  (let-keywords* keywords ((url->href #t)
                           (url->href:filter google-redirector)
                           (url->href:keywords '()) ; target�Ȥ�name�Ȥ�
                           (linefeed->br #t)
                           (space->nbsp #t)
                           (tab->nbsp #f) ; ToDo: ̤����
                           )
    (define (convert target)
      (cond
        ((*re:linefeed* target) (if linefeed->br
                                  (html:br)
                                  (het target)))
        ((equal? " " target) (if space->nbsp
                               "&nbsp;"
                               (het target)))
        ((equal? "\t" target) (if tab->nbsp
                                (het target) ; ToDo: ̤����
                                (het target)))
        ((*re:url* target) (if url->href
                             (apply html:a
                                    :href (if url->href:filter
                                            (url->href:filter target)
                                            target)
                                    (append
                                      url->href:keywords
                                      (list (het target))))
                             (het target)))
        (else
          (error "assertion"))))

    (tree->string
      (let recursive ((leftover text))
        (if (string=? "" leftover)
          '()
          (let1 m (*re:text->inline-html* leftover)
            (if (not m)
              (het leftover)
              (list
                (het (m 'before))
                (convert (m))
                (recursive (m 'after))))))))))

(define (text->block-html text . keywords)
  ;; ToDo: ���ȤǤ����ȼ��������
  (html:p
    (apply text->inline-html text keywords)))


(provide "tir04/cgi/util")

