;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; stdinからHTTPリクエストを読み取り、パーズして返す関数を提供。
;;; タイムアウト処理は行わない。
;;; Content-Lengthが指定値以上の場合、request-bodyは一時ファイルに保存する

;;; note : ここで400が発生した場合、persistent connectionは続行不可能


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




;; ToDo : fsの挙動依存なので、直す必要がある
(define *read-block-size* 4096)
(define (stdin->tmp-file-port temporary-file-prefix target-length)
  ;; target-lengthだけstdinから読み出し、一時ファイルに書き出し、
  ;; 一時ファイルをinput-portとして開き、一時ファイルをrmし、
  ;; 一時ファイルのinput-portを返す。
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
            request-body-on-memory-limit-size ; 値が設定されており、
            (< request-body-on-memory-limit-size
               content-length)) ; content-lengthの方が大きい場合のみ、
        (stdin->tmp-file-port temporary-file-prefix content-length) ; fsに保持
        (open-input-string (read-block content-length)))) ; オンメモリ保持
    (open-input-string ""))) ; 空だった




;; 以下のような二値を返す
;; - request-result。キーワードリスト。リストの書式は以下の通り。
;;   '(
;;     :request-body-port input-port ; 先読みしたrequest-body。先読み無効化可能
;;     :request-header (("host" "hoge.com")
;;                      ("connection" "Keep-Alive")
;;                      ) ; rfc.822
;;     :request-line "GET / HTTP/1.1"
;;     :request-method "GET"
;;     :request-uri "/"
;;     :parsed-uri-list (#f #f #f #f "/" #f #f) ; rfc.uri
;;     :request-protocol "HTTP/1.1"
;;     )
;; - error-desc。#fまたはシンボルによる簡単な説明文。
;;   error-descが#fでない場合は400エラー扱いとすべき。
;;   但し、HTTPクライアントの実装によっては、
;;   keep-alive-timeout時に'empty_requestが発生する可能性がある
(define (stdin->http-request . keywords)
  (let-keywords* keywords (
                           (convert-incomplete-string-uri #t) ; ces変換の有無
                           (request-body-caching #t) ; request-bodyを取り込む
                           (temporary-file-prefix "/tmp/tcpcgi") ; 一時ファイル
                           (request-body-on-memory-limit-size #f) ; byteで指定
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
            ;; ここまで取得した内容を検証し、#tか#fを返す
            (cond
              ;; パラメータは三つまで
              ((not (<= mup-num 3))
               (return request-line-result 'too_many_request_line_parameter))
              ;; protocolはHTTP/n.m書式である事
              ((not (#/^http\/\d\.\d/i request-protocol))
               (return request-line-result 'bad_protocol))
              ;; pathは/はじまりである事
              ((not (#/^\// (list-ref parsed-uri-list 4)))
               (return request-line-result 'bad_path))
              ;; uri-schemeが有るなら、それはhttpかhttpsである事
              ((let1 uri-scheme (list-ref parsed-uri-list 0)
                 (and
                   uri-scheme
                   (not (#/^https?$/ uri-scheme))))
               (return request-line-result 'bad_uri_scheme))
              ;; HTTP/0.9なら、request-header等は取得しない
              ((string=? "HTTP/0.9" request-protocol)
               (return
                 (list*
                   :request-body-port (if request-body-caching
                                        (open-input-string "")
                                        (current-input-port))
                   :request-header '()
                   request-line-result)
                 #f)) ; 正常終了
              (else
                ;; request-headerとrequest-bodyを取得して返す
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

