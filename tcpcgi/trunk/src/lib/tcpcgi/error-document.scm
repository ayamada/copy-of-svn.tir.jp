;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ToDo : 各レスポンスコードを網羅したエラードキュメントのコンプリート

(define-module tcpcgi.error-document
  (use gauche.parameter)
  (use srfi-2) ; and-let*
  (use text.tree)
  (use text.html-lite)
  (use www.cgi)

  (use tcpcgi.common)
  (export
    <tcpcgi.error-document>
    display-error-document
    default-error-document-thunk-table
    fallback-error-document-thunk
    ))
(select-module tcpcgi.error-document)


(define-class <tcpcgi.error-document> ()
  (
   ;; keyにはresponse-code
   ;; valueには(cons cgi-thunk params)
   (ht :accessor ht-of
       :init-keyword :ht
       :init-value '())
   ))


(define-method display-error-document ((self <tcpcgi.error-document>) response-code)
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
         (ref default-error-document-thunk-table response-code #f)
         fallback-error-document-thunk)))))


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




(define default-error-document-thunk-table
  (hash-table
    'equal?
    (cons 400 d400)
    (cons 403 d403)
    (cons 404 d404)
    (cons 500 d500)
    ))

(define (fallback-error-document-thunk)
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




(provide "tcpcgi/error-document")

