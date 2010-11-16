;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; stdin����HTTP�ꥯ�����Ȥ��ɤ߼�ꡢ�ѡ��������֤��ؿ����󶡡�
;;; �����ॢ���Ƚ����ϹԤ�ʤ���
;;; Content-Length�������Ͱʾ�ξ�硢request-body�ϰ���ե��������¸����

;;; note : ������400��ȯ��������硢persistent connection��³���Բ�ǽ


(define-module tcpcgi.request
  (use gauche.charconv)
  (use gauche.uvector)

  (use srfi-2) ; and-let*
  (use rfc.822)
  (use rfc.uri)

  ;; get-header-value
  ;; set-temporary-state!
  ;; with-port-buffering
  ;; with-signal-handler
  (use tcpcgi.common)
  (export
    stdin->http-request
    stdin->request-body-port
    ))
(select-module tcpcgi.request)




(define (read-request-line)
  (let1 l (read-line (current-input-port) #t)
    (cond
      ((equal? l "") (read-request-line))
      ((eof-object? l) #f)
      (else l))))


(define (ces-convert-*jp incomplete-string)
  (with-error-handler
    (lambda (e) #f)
    (lambda ()
      (ces-convert
        incomplete-string
        (ces-guess-from-string incomplete-string "*JP")))))




;; ToDo : fs�ε�ư��¸�ʤΤǡ�ľ��ɬ�פ�����
(define *read-block-size* 4096)
(define (stdin->tmp-file-port temporary-file-prefix target-length)
  ;; target-length����stdin�����ɤ߽Ф�������ե�����˽񤭽Ф���
  ;; ����ե������input-port�Ȥ��Ƴ���������ե������rm����
  ;; ����ե������input-port���֤���
  (receive (oport filename)
    (sys-mkstemp temporary-file-prefix)
    (let1 buf (make-u8vector *read-block-size*)
      (let loop ((left-byte target-length))
        (let* ((block-end? (< left-byte *read-block-size*))
               (read-length (if block-end?
                              left-byte
                              *read-block-size*))
               )
          (read-block! buf (current-input-port) 0 read-length)
          (write-block buf oport 0 read-length)
          (if block-end?
            (close-output-port oport)
            (loop (- left-byte *read-block-size*))))))
    (begin0
      (open-input-file filename)
      (sys-unlink filename))))


(define (stdin->request-body-port request-header
                                  request-body-on-memory-limit-size
                                  temporary-file-prefix)
  (or
    (and-let* ((content-length-string
                 (get-header-value "content-length" request-header))
               (content-length (x->number content-length-string))
               )
      (if (and
            request-body-on-memory-limit-size ; �ͤ����ꤵ��Ƥ��ꡢ
            (< request-body-on-memory-limit-size
               content-length)) ; content-length�������礭�����Τߡ�
        (stdin->tmp-file-port temporary-file-prefix content-length) ; fs���ݻ�
        (open-input-string (read-block content-length)))) ; ��������ݻ�
    (open-input-string ""))) ; �����ä�




;; �ʲ��Τ褦�����ͤ��֤�
;; - request-result��������ɥꥹ�ȡ��ꥹ�Ȥν񼰤ϰʲ����̤ꡣ
;;   '(
;;     :request-body-port input-port ; ���ɤߤ���request-body�����ɤ�̵������ǽ
;;     :request-header (("host" "hoge.com")
;;                      ("connection" "Keep-Alive")
;;                      ) ; rfc.822
;;     :request-line "GET / HTTP/1.1"
;;     :request-method "GET"
;;     :request-uri "/"
;;     :parsed-uri-list (#f #f #f #f "/" #f #f) ; rfc.uri
;;     :request-protocol "HTTP/1.1"
;;     )
;; - error-desc��#f�ޤ��ϥ���ܥ�ˤ���ñ������ʸ��
;;   error-desc��#f�Ǥʤ�����400���顼�����Ȥ��٤���
;;   â����HTTP���饤����Ȥμ����ˤ�äƤϡ�
;;   keep-alive-timeout����'empty_request��ȯ�������ǽ��������
(define (stdin->http-request . keywords)
  (let-keywords* keywords (
                           (convert-incomplete-string-uri #t) ; ces�Ѵ���̵ͭ
                           (request-body-caching #t) ; request-body�������
                           (temporary-file-prefix "/tmp/tcpcgi") ; ����ե�����
                           (request-body-on-memory-limit-size #f) ; byte�ǻ���
                           )
    (with-port-buffering
      (current-input-port)
      :full
      (lambda ()
        (let/cc return
          (let* (
                 (pre-request-line (or
                                     (read-request-line)
                                     (return '() 'empty_request)))
                 (request-line (if (string-incomplete? pre-request-line)
                                 (or
                                   (and
                                     convert-incomplete-string-uri
                                     (ces-convert-*jp pre-request-line))
                                   (return
                                     `(:request-line ,pre-request-line)
                                     'abnormal_request_line))
                                 pre-request-line))
                 (mup (with-error-handler
                        (lambda (e)
                          (return
                            `(:request-line ,request-line)
                            'abnormal_request_line))
                        (lambda ()
                          (string-split request-line #\space))))
                 (mup-num (length mup))
                 (request-method (car mup))
                 (request-uri (if (= 1 mup-num)
                                "/" ; for HTTP/0.9
                                (cadr mup)))
                 (request-protocol (if (< mup-num 3)
                                     "HTTP/0.9"
                                     (caddr mup)))
                 (parsed-uri-list (call-with-values
                                    (lambda ()
                                      (uri-parse request-uri))
                                    list))
                 ;; (list-ref parsed-uri-list 0) ; uri-scheme
                 ;; (list-ref parsed-uri-list 1) ; uri-userinfo
                 ;; (list-ref parsed-uri-list 2) ; uri-hostname
                 ;; (list-ref parsed-uri-list 3) ; uri-port
                 ;; (list-ref parsed-uri-list 4) ; uri-path
                 ;; (list-ref parsed-uri-list 5) ; uri-query
                 ;; (list-ref parsed-uri-list 6) ; uri-fragment
                 (request-line-result
                   `(:request-line ,request-line
                     :request-method ,request-method
                     :request-uri ,request-uri
                     :parsed-uri-list ,parsed-uri-list
                     :request-protocol ,request-protocol
                     ))
                 )
            ;; �����ޤǼ����������Ƥ򸡾ڤ���#t��#f���֤�
            (cond
              ;; �ѥ�᡼���ϻ��Ĥޤ�
              ((not (<= mup-num 3))
               (return request-line-result 'too_many_request_line_parameter))
              ;; protocol��HTTP/n.m�񼰤Ǥ����
              ((not (#/^http\/\d\.\d/i request-protocol))
               (return request-line-result 'bad_protocol))
              ;; path��/�Ϥ��ޤ�Ǥ����
              ((not (#/^\// (list-ref parsed-uri-list 4)))
               (return request-line-result 'bad_path))
              ;; uri-scheme��ͭ��ʤ顢�����http��https�Ǥ����
              ((let1 uri-scheme (list-ref parsed-uri-list 0)
                 (and
                   uri-scheme
                   (not (#/^https?$/ uri-scheme))))
               (return request-line-result 'bad_uri_scheme))
              ;; HTTP/0.9�ʤ顢request-header���ϼ������ʤ�
              ((string=? "HTTP/0.9" request-protocol)
               (return
                 (list*
                   :request-body-port (if request-body-caching
                                        (open-input-string "")
                                        (current-input-port))
                   :request-header '()
                   request-line-result)
                 #f)) ; ���ｪλ
              (else
                ;; request-header��request-body����������֤�
                (let* ((request-header
                         (rfc822-header->list (current-input-port)))
                       (request-body-port
                         (if request-body-caching
                           (stdin->request-body-port
                             request-header
                             request-body-on-memory-limit-size
                             temporary-file-prefix)
                           (current-input-port)))
                       )
                  (return
                    (list*
                      :request-body-port request-body-port
                      :request-header request-header
                      request-line-result)
                    #f))))))))))



(provide "tcpcgi/request")

