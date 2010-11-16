;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme sw=2 ts=2 et:
;;; $Id$

;;; ToDo : 各レスポンスコードを網羅したエラードキュメントのコンプリート

(define-module tcpcgi.errordoc
  (use gauche.parameter)
  (use srfi-2) ; and-let*
  (use text.tree)
  (use text.html-lite)
  (use www.cgi)

  (export
    <tcpcgi.errordoc>
    display-errordoc
    default-errordoc-thunk-table
    fallback-errordoc-thunk
    status-code->label
    *status-code->label*
    ))
(select-module tcpcgi.errordoc)


(define-class <tcpcgi.errordoc> ()
  (
   ;; keyにはresponse-code
   ;; valueには(cons cgi-thunk params)
   (ht :accessor ht-of
       :init-keyword :ht
       :init-value '())
   ))


(define-method display-errordoc ((self <tcpcgi.errordoc>) response-code)
  (parameterize ((cgi-metavariables (cons
                                      (list
                                        "X_RESPONSE_CODE"
                                        (number->string response-code))
                                      (or
                                        (cgi-metavariables)
                                        '()))))
    (or
      (and-let* ((target-list (ref (ht-of self) response-code #f))
                 (target-error-thunk (car target-list))
                 (keywords (cdr target-list))
                 )
        ;; ToDo : keywordsに応じた処理
        target-error-thunk ; 存在するなら実行
        (with-error-handler
          (lambda (e)
            #f) ; エラーを返したならfallbackを実行
          (lambda ()
            ;; エラー時に備えて、一旦バッファリング
            (display
              (with-output-to-string
                target-error-thunk))
            #t)))
      ((or ; どっちかを実行
         (ref default-errordoc-thunk-table response-code #f)
         fallback-errordoc-thunk)))))


(define (d400)
  (write-tree
    (list
      (cgi-header
        :status "400 Bad Request"
        )
      (html:html
        (html:head
          (html:title "400 Bad Request"))
        (html:body
          (html:h1 "Bad Request")
          (html:p "Your browser sent a request "
                  "that this server could not understand."))))))

(define (d403)
  (write-tree
    (list
      (cgi-header
        :status "403 Forbidden"
        )
      (html:html
        (html:head
          (html:title "403 Forbidden")
          )
        (html:body
          (html:h1 "Forbidden")
          (html:p "Your don't have permission to access this url."))))))


(define (d404)
  (write-tree
    (list
      (cgi-header
        :status "404 Not Found"
        :pragma "no-cache"
        )
      (html:html
        (html:head
          (html:title "404 Not Found")
          )
        (html:body
          (html:h1 "Not Found")
          (html:p "The requested URL was not found on this server."))))))


(define (d500)
  (write-tree
    (list
      (cgi-header
        :status "500 Internal Server Error"
        :pragma "no-cache"
        )
      (html:html
        (html:head
          (html:title "500 Internal Server Error"))
        (html:body
          (html:h1 "Internal Server Error")
          (html:p "The server encountered an internal error "
                  "or misconfiguration "
                  "and was unable to complete "
                  "your request."))))))




(define default-errordoc-thunk-table
  (hash-table
    'equal?
    (cons 400 d400)
    (cons 403 d403)
    (cons 404 d404)
    (cons 500 d500)
    ))

(define (fallback-errordoc-thunk)
  (let* ((response-code (cgi-get-metavariable "X_RESPONSE_CODE"))
         (response-message (status-code->label response-code))
         (status (format "~a ~a" response-code response-message))
         )
    (write-tree
      (list
        (cgi-header
          :status status
          :pragma "no-cache"
          )
        (html:html
          (html:head
            (html:title status))
          (html:body
            (html:h1 response-message)
            (html:p "sorry, not prepared to document "
                    "of this response.")
            ))))))


(define (status-code->label status-code)
  (or
    (and
      status-code
      (ref *status-code->label* (x->number status-code) #f))
    "Unknown Error"))

(define *status-code->label*
  (hash-table 'eqv?
              '(100 . "Continue")
              '(101 . "Switching Protocols")
              '(102 . "Processing")

              '(200 . "OK")
              '(201 . "Created")
              '(202 . "Accepted")
              '(203 . "Non-Authoritative Information")
              '(204 . "No Content")
              '(205 . "Reset Content")
              '(206 . "Partial Content")
              '(207 . "Multi-Status")

              '(300 . "Multiple Choices")
              '(301 . "Moved Permanently")
              '(302 . "Found")
              '(303 . "See Other")
              '(304 . "Not Modified")
              '(305 . "Use Proxy")
              '(306 . "unused")
              '(307 . "Temporary Redirect")

              '(400 . "Bad Request")
              '(401 . "Authorization Required")
              '(402 . "Payment Required")
              '(403 . "Forbidden")
              '(404 . "Not Found")
              '(405 . "Method Not Allowed")
              '(406 . "Not Acceptable")
              '(407 . "Proxy Authentication Required")
              '(408 . "Request Time-out")
              '(409 . "Conflict")
              '(410 . "Gone")
              '(411 . "Length Required")
              '(412 . "Precondition Failed")
              '(413 . "Request Entity Too Large")
              '(414 . "Request-URI Too Large")
              '(415 . "Unsupported Media Type")
              '(416 . "Requested Range Not Satisfiable")
              '(417 . "Expectation Failed")
              '(418 . "unused")
              '(419 . "unused")
              '(420 . "unused")
              '(421 . "unused")
              '(422 . "Unprocessable Entity")
              '(423 . "Locked")
              '(424 . "Failed Dependency")

              '(500 . "Internal Server Error")
              '(501 . "Method Not Implemented")
              '(502 . "Bad Gateway")
              '(503 . "Service Temporarily Unavailable")
              '(504 . "Gateway Time-out")
              '(505 . "HTTP Version Not Supported")
              '(506 . "Variant Also Negotiates")
              '(507 . "Insufficient Storage")
              '(508 . "unused")
              '(509 . "unused")
              '(510 . "Not Extended")
              ))


(provide "tcpcgi/errordoc")

