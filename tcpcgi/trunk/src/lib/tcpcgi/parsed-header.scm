;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ToDo : 不正なヘッダ検出時等にerror例外を投げるようにする

(define-module tcpcgi.parsed-header
  (use srfi-1)
  (use srfi-2) ; and-let*
  (use srfi-13) ; string-titlecase
  (use rfc.uri)
  (use text.tree)
  (use text.html-lite)
  (use util.list)
  (use www.cgi)

  (use tcpcgi.common)
  (export
    cgi/1.1->http/1.1
    ;; CGI/1.1形式のrfc.822なalistのヘッダ及びその他のキーワード情報から、
    ;; HTTP/1.1形式の値を生成する。
    ;; 返り値は以下の通り。
    ;; (values
    ;;   http/1.1-response-code   ; レスポンスコードの数値
    ;;   http/1.1-status-line     ; "HTTP/1.1 200 OK" 等
    ;;   http/1.1-response-header ; rfc.822形式のalist
    ;;   response-body-port)      ; response-bodyの入ったport
    ;; 以下のヘッダを自動的に追加する。
    ;; - date
    ;; - connection (キーワードが渡されている場合のみ)
    ;; - server (キーワードが渡されている場合のみ)
    ;; - content-length (キーワードが渡されている場合のみ)
    ;; 以下のキーワードを受け入れる。
    ;; - 上記の connection server content-length の三つ
    ;; - server-name server-port https (location時にuriを修正する際に使用)
    ;; - extra-header (他に付加したいヘッダがあるならrfc.822形式のalistで指定)
    ;;
    ;; 尚、:content-lengthとCGI/1.1ヘッダのcontent-lengthの両方が存在する場合は
    ;; CGI/1.1ヘッダの方が優先される。
    ;; (HEADのように、実際に送るbodyのサイズと一致しない場合もあるので)
    ;; なので、cgiスクリプトが不正なサイズのヘッダを出力した場合、
    ;; 永続的接続には異常が発生する事になる事に注意。
    ;; content-lengthの自動生成は行わないので、予め自前で測定しておき、
    ;; キーワードとして渡す事。
    ;; 今のところ、tcpcgiはchunkedでの出力をサポートしていないので、
    ;; content-lengthの指定は必須だという事に注意！

    display-http/1.1-header
    ;; cgi/1.1->http/1.1の結果のヘッダ部分を出力する。
    ;; response-bodyは出力しないので、response-bodyは自前で出力する必要がある。
    ))
(select-module tcpcgi.parsed-header)



(define (completion-uri location server-name server-port https)
  (receive (uri-scheme
            uri-userinfo
            uri-hostname
            uri-port
            uri-path
            uri-query
            uri-fragment)
    (uri-parse location)
    ;; uri-schemeが無い時にだけ補完する
    ;; 但し、server-nameが与えられていない場合は補完できないので、何もしない
    (if (or uri-scheme (not server-name))
      location
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
          :flagment uri-fragment)))))



(define (make-date-header-list)
  (list
    "date"
    (sys-strftime
      "%a, %d %b %Y %X %Z"
      (sys-gmtime (sys-time)))))




(define (get-location-html-port&size full-uri)
  (let1 location-html (tree->string
                        (html:html
                          (html:head
                            (html:title "302 Found"))
                          (html:body
                            (html:h1 "Found")
                            (html:p
                              "The document has moved "
                              (html:a
                                :href full-uri
                                (html-escape-string full-uri))
                              "."))))
    (cons (open-input-string location-html) (string-size location-html))))




(define (status-string->status-number status)
  (and-let* ((m (#/^(\d+)\s/ status)))
    (string->number (m 1))))



(define (cgi/1.1->http/1.1 src-header src-body . keywords)
  ;; src-bodyは文字列、input-port, output-port, どれでも可とする
  (let-keywords* keywords (
                           ;; 重要なヘッダ指定
                           (connection #f) ; connectionヘッダが無い場合に使用
                           (server #f) ; serverヘッダが無い場合に
                           (content-length #f) ; content-lengthヘッダが

                           ;; location時に必要になる情報
                           (server-name #f)
                           (server-port #f)
                           (https #f)

                           ;; 常に付与する追加ヘッダがあるなら指定
                           (extra-header '()) ; alist
                           )
    (define (get-body-port)
      (cond
        ((string? src-body) (open-input-string src-body))
        ((input-port? src-body) src-body)
        ((output-port? src-body)
         (open-input-string (get-output-string src-body)))
        ((not src-body) (open-input-string ""))
        (else (error "invalid src-body"))))

    ;; 予め、よく使う物を取り出しておく
    (let* (
           (src-content-type (get-header-value "content-type" src-header))
           (src-content-length (get-header-value "content-length" src-header))
           (src-status (get-header-value "status" src-header))
           (src-location (get-header-value "location" src-header))
           (true-location (and src-location (completion-uri
                                              src-location
                                              server-name
                                              server-port
                                              https)))
           (response-body-port&size (and
                                     true-location
                                     (get-location-html-port&size
                                       true-location)))
           (response-body-port (if response-body-port&size
                                (car response-body-port&size)
                                (get-body-port)))
           (location-body-size (and
                                 response-body-port&size
                                 (cdr response-body-port&size)))
           (remainder-header (remove
                               (lambda (x)
                                 (member
                                   (car x)
                                   '("status" ; HTTP/1.1として出さない
                                     "location" ; 一旦消して再構築する
                                     "content-type" ; 一旦消して再構築する
                                     "content-length" ; 一旦消して再構築する
                                     )))
                               src-header))
           (response-code&status-line
             (cond
               (src-status (cons
                             (status-string->status-number status)
                             (format "HTTP/1.1 ~a" src-status)))
               (true-location (cons 302 "HTTP/1.1 302 Found"))
               (src-content-type (cons 200 "HTTP/1.1 200 OK"))
               (else (cons 200 "HTTP/1.1 200 OK"))))
           (response-code (car response-code&status-line))
           (status-line (cdr response-code&status-line))
           (true-content-length (and
                                  location-body-size
                                  src-content-length
                                  content-length))
           (added-header
             (fold-right
               ;; #fまたは'("header" #f)でない時のみlistに追加する
               (lambda (a b)
                 (if (or
                       (not a)
                       (not (cadr a)))
                   b
                   (cons a b)))
               '()
               (list
                 ;; 基本的に、常に追加するヘッダ
                 (make-date-header-list)
                 (list "server" server)
                 (list "connection" connection)
                 ;; 必要に応じて追加するヘッダ
                 (list "location" true-location)
                 (list "content-type" (cond
                                        (src-status src-content-type)
                                        (true-location "text/html")
                                        (src-content-type src-content-type)
                                        (else
                                          ;; apacheでは、この場合に勝手に
                                          ;; Content-Type: text/plain
                                          ;; を付加する挙動を真似する
                                          "text/plain")))
                 ;; 状況に応じて追加する
                 (list "content-length" true-content-length)
                 )))
           )
      (values
        response-code
        status-line
        (append
          added-header
          extra-header
          remainder-header)
        response-body-port))))



(define *crlf* "\r\n")

(define (display-http/1.1-header http/1.1-status-line http/1.1-response-header)
  ;; status-line出力
  (display http/1.1-status-line)
  (display *crlf*)
  ;; header出力
  ;; form形式の複数値は無い筈(rfc822形式の複数値はある)なので、
  ;; そのままcadrを使って良い
  (for-each
    (lambda (key&values)
      (display (string-titlecase (car key&values)))
      (display ": ")
      (display (cadr key&values))
      (display *crlf*))
    http/1.1-response-header)
  (display *crlf*)
  (flush))



(provide "tcpcgi/parsed-header")

