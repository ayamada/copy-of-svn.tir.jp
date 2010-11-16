;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; HTTP/1.1�����Υꥯ�����Ȥ���CGI�᥿�ѿ�����������٤Υ⥸�塼��

(define-module tcpcgi.metavariables
  (use srfi-1) ; filter filter-map
  (use srfi-2) ; and-let*

  ;; get-header-value
  ;; set-temporary-state!
  ;; with-port-buffering
  ;; with-signal-handler
  (use tcpcgi.common)
  (export
    request->metavariables-alist
    uniq-rfc822-header-list
    append-metavariables-alist
    ))
(select-module tcpcgi.metavariables)



(define *exclude-http-header-regexp*
  #/^(?:(?:Content\-(?:Type|Length))|(?:(?:Proxy\-)?Authorization))$/i)



(define (header-name->mv-name header-name)
  (with-string-io
    header-name
    (lambda ()
      (display "HTTP_")
      (let loop ((r (read-byte)))
        (if (eof-object? r)
          #t
          (begin
            (write-byte
              (cond
                ((<= #x61 r #x7a) (logand #xdf r)) ; a-z -> A-Z
                ((eq? #x2d r) #x5f) ; "-" -> "_"
                (else r)))
            (loop (read-byte))))))))


;; Ʊ�������ν�ʣ�Τ���rfc822��alist���饭���ν�ʣ��ʤ����ơ�
;; ��ĤΥ�����ʣ�����ͤ����alist�����������֤�
(define (uniq-rfc822-header-list request-header)
  (let loop ((src-alist request-header)
             (result-alist '()))
    (if (null? src-alist)
      result-alist
      (let* ((current-line (car src-alist))
             (next-alist (cdr src-alist))
             (key (car current-line))
             (value-list (cdr current-line))
             )
        (loop
          next-alist
          (if (assoc key result-alist)
            result-alist ; pass
            (acons
              key
              (apply
                append
                value-list
                (filter-map
                  (lambda (line)
                    (and
                      (string=? key (car line))
                      (cdr line)))
                  next-alist))
              result-alist)))))))




;; keywords����᥿�ѿ�alist����������
(define (request->metavariables-alist . keywords)
  (let-keywords* keywords (
                           (server-software "tcpcgi")

                           (remote-addr #f)
                           (remote-port #f)
                           (remote-host #f)
                           (server-addr #f)
                           (server-port #f)

                           (request-method #f)
                           (request-uri #f)
                           (request-protocol #f)
                           (parsed-uri-list '(#f #f #f #f #f #f #f))
                           (request-header '())

                           (https #f)

                           (server-name #f)
                           (script-name #f)
                           (path-info #f)
                           (path-translated #f)

                           (auth-type #f)
                           (remote-user #f)
                           )
    (let* ((query-string (list-ref parsed-uri-list 5))
           (content-type (get-header-value "content-type" request-header))
           (content-length (get-header-value "content-length" request-header))
           )
      (filter
        cadr ; value��#f�ʤ顢�᥿�ѿ��ˤϴޤ�ʤ�
        (list*
          ;; �᥿�ѿ����ȤΥ᥿����
          '("GATEWAY_INTERFACE" "CGI/1.1")
          (list "SERVER_SOFTWARE" server-software)
          ;; tcp��³�˴ؤ������
          (list "REMOTE_ADDR" remote-addr)
          (list "REMOTE_PORT" (and remote-port (x->string remote-port)))
          (list "REMOTE_HOST" remote-host)
          (list "SERVER_ADDR" server-addr)
          (list "SERVER_PORT" (and server-port (x->string server-port)))
          ;; request����
          (list "REQUEST_METHOD" request-method)
          (list "REQUEST_URI" request-uri)
          (list "SERVER_PROTOCOL" request-protocol)
          (list "QUERY_STRING" (or query-string ""))
          ;; https
          (list "HTTPS" (and https "on"))
          ;; dispatch����
          (list "SERVER_NAME" server-name)
          (list "SCRIPT_NAME" script-name)
          (list "PATH_INFO" path-info)
          (list "PATH_TRANSLATED" path-translated)
          ;; auth����
          (list "AUTH_TYPE" auth-type)
          (list "REMOTE_USER" remote-user)
          ;; content-type, content-length
          (list "CONTENT_TYPE" content-type)
          (list "CONTENT_LENGTH" content-length)
          ;; HTTP_*�إå�
          (map
            (lambda (line)
              ;; �ʲ��λͤĤΥإå��Ͻ���������ͤ�#f�Ȥ����
              ;; Content-Type, Content-Length,
              ;; Authorization, Proxy-Authorization
              (if (*exclude-http-header-regexp* (car line))
                '(#f #f)
                (list
                  (header-name->mv-name (car line))
                  (string-join ; Ʊ�������Υإå���ʣ������ʤ�Ϣ��
                    (cdr line)
                    ", "))))
            ;; rfc822�إå��ϡ�Ʊ��̾���Υإå���ʣ��¸�ߤ����礬����Τǡ�
            ;; ������ͽ��ޡ������Ƥ���ɬ�פ�����
            (uniq-rfc822-header-list request-header)))))))



;; mv-alistƱ�Τ��������utility�ؿ�
;; note : mv-alist�ϡ�ͽ��uniq���Ƥ�����
;; note : Ʊ��������¸�ߤ������ϡ���Υꥹ�Ȥ��ͤ����Ѥ����
;;        �ʸ�Υꥹ�Ȥ��ͤ��˴�������
(define (append-metavariables-alist . list-of-mv-alists)
  (let loop ((src (apply append list-of-mv-alists))
             (result '()))
    (if (null? src)
      result
      (loop
        (cdr src)
        (let1 line (car src)
          (if (assoc (car line) result)
            result ; Ʊ̾����������¸�ߤ���ʤ��ɲä��ʤ�
            (cons
              line
              result)))))))






(provide "tcpcgi/metavariables")

