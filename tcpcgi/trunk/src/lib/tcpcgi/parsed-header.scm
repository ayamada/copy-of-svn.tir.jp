;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ToDo : �����ʥإå����л�����error�㳰���ꤲ��褦�ˤ���

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
    ;; CGI/1.1������rfc.822��alist�Υإå��ڤӤ���¾�Υ�����ɾ��󤫤顢
    ;; HTTP/1.1�������ͤ��������롣
    ;; �֤��ͤϰʲ����̤ꡣ
    ;; (values
    ;;   http/1.1-response-code   ; �쥹�ݥ󥹥����ɤο���
    ;;   http/1.1-status-line     ; "HTTP/1.1 200 OK" ��
    ;;   http/1.1-response-header ; rfc.822������alist
    ;;   response-body-port)      ; response-body�����ä�port
    ;; �ʲ��Υإå���ưŪ���ɲä��롣
    ;; - date
    ;; - connection (������ɤ��Ϥ���Ƥ�����Τ�)
    ;; - server (������ɤ��Ϥ���Ƥ�����Τ�)
    ;; - content-length (������ɤ��Ϥ���Ƥ�����Τ�)
    ;; �ʲ��Υ�����ɤ��������롣
    ;; - �嵭�� connection server content-length �λ���
    ;; - server-name server-port https (location����uri��������ݤ˻���)
    ;; - extra-header (¾���ղä������إå�������ʤ�rfc.822������alist�ǻ���)
    ;;
    ;; ����:content-length��CGI/1.1�إå���content-length��ξ����¸�ߤ������
    ;; CGI/1.1�إå�������ͥ�褵��롣
    ;; (HEAD�Τ褦�ˡ��ºݤ�����body�Υ������Ȱ��פ��ʤ����⤢��Τ�)
    ;; �ʤΤǡ�cgi������ץȤ������ʥ������Υإå�����Ϥ�����硢
    ;; ��³Ū��³�ˤϰ۾郎ȯ��������ˤʤ������ա�
    ;; content-length�μ�ư�����ϹԤ�ʤ��Τǡ�ͽ�Ἣ����¬�ꤷ�Ƥ�����
    ;; ������ɤȤ����Ϥ�����
    ;; ���ΤȤ���tcpcgi��chunked�Ǥν��Ϥ򥵥ݡ��Ȥ��Ƥ��ʤ��Τǡ�
    ;; content-length�λ����ɬ�ܤ��Ȥ���������ա�

    display-http/1.1-header
    ;; cgi/1.1->http/1.1�η�̤Υإå���ʬ����Ϥ��롣
    ;; response-body�Ͻ��Ϥ��ʤ��Τǡ�response-body�ϼ����ǽ��Ϥ���ɬ�פ����롣
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
    ;; uri-scheme��̵�����ˤ����䴰����
    ;; â����server-name��Ϳ�����Ƥ��ʤ������䴰�Ǥ��ʤ��Τǡ����⤷�ʤ�
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
  ;; src-body��ʸ����input-port, output-port, �ɤ�Ǥ�ĤȤ���
  (let-keywords* keywords (
                           ;; ���פʥإå�����
                           (connection #f) ; connection�إå���̵�����˻���
                           (server #f) ; server�إå���̵������
                           (content-length #f) ; content-length�إå���

                           ;; location����ɬ�פˤʤ����
                           (server-name #f)
                           (server-port #f)
                           (https #f)

                           ;; �����Ϳ�����ɲåإå�������ʤ����
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

    ;; ͽ�ᡢ�褯�Ȥ�ʪ����Ф��Ƥ���
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
                                   '("status" ; HTTP/1.1�Ȥ��ƽФ��ʤ�
                                     "location" ; ��ö�ä��ƺƹ��ۤ���
                                     "content-type" ; ��ö�ä��ƺƹ��ۤ���
                                     "content-length" ; ��ö�ä��ƺƹ��ۤ���
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
               ;; #f�ޤ���'("header" #f)�Ǥʤ����Τ�list���ɲä���
               (lambda (a b)
                 (if (or
                       (not a)
                       (not (cadr a)))
                   b
                   (cons a b)))
               '()
               (list
                 ;; ����Ū�ˡ�����ɲä���إå�
                 (make-date-header-list)
                 (list "server" server)
                 (list "connection" connection)
                 ;; ɬ�פ˱������ɲä���إå�
                 (list "location" true-location)
                 (list "content-type" (cond
                                        (src-status src-content-type)
                                        (true-location "text/html")
                                        (src-content-type src-content-type)
                                        (else
                                          ;; apache�Ǥϡ����ξ��˾����
                                          ;; Content-Type: text/plain
                                          ;; ���ղä����ư�򿿻�����
                                          "text/plain")))
                 ;; �����˱������ɲä���
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
  ;; status-line����
  (display http/1.1-status-line)
  (display *crlf*)
  ;; header����
  ;; form������ʣ���ͤ�̵��Ȧ(rfc822������ʣ���ͤϤ���)�ʤΤǡ�
  ;; ���Τޤ�cadr��Ȥä��ɤ�
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

