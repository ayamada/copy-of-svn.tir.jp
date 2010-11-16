;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; ToDo : あとで動作確認する事

(define-module tcpcgi.common
  (use www.cgi)
  (export
    get-header-value
    set-temporary-state!
    with-port-buffering
    with-signal-handler
    status-code->label
    *status-code->label*
    ))
(select-module tcpcgi.common)


(define get-header-value cgi-get-parameter)


(define-syntax set-temporary-state!
  (syntax-rules ()
    ((_ old-value new-value value-setter thunk)
     (dynamic-wind
       (lambda ()
         (value-setter new-value))
       thunk
       (lambda ()
         (value-setter old-value))))))

(define (with-port-buffering port mode thunk)
  (if port
    (set-temporary-state!
      (port-buffering port)
      mode
      (lambda (new)
        (with-error-handler
          (lambda (e) #f)
          (lambda ()
            (set! (port-buffering port) new))))
      thunk)
    (thunk)))

(define (with-signal-handler signal handler thunk)
  (set-temporary-state!
    (get-signal-handler signal)
    handler
    (cut set-signal-handler! signal <>)
    thunk))




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


(provide "tcpcgi/common")

