;;; coding: euc-jp
;;; -*- scheme -*-
;;; vim:set ft=scheme ts=8 sts=2 sw=2 et:
;;; $Id$

;;; よく使われる、cgiユーティリティ集。

(define-module tcpcgi.supplement
  (use gauche.parameter)
  (use srfi-2) ; and-let*
  (use text.html-lite)
  (use www.cgi)

  (export
    tcpcgi:display-cgi-metavariables-cgi-thunk
    tcpcgi:display-debug-information-cgi-thunk
    tcpcgi:tcpcgi-process-finish-cgi-thunk
    ))
(select-module tcpcgi.supplement)


(define hes html-escape-string)

(define (display-html proc)
  (cgi-main
    (lambda (params)
      (let1 keywords (proc params)
        (let-keywords* keywords ((title "(untitled document)")
                                 (body (html:p "no content."))
                                 )
          (list
            (cgi-header :pragma "no-cache")
            (html:html
              (html:head
                (html:title (hes title)))
              (html:body
                (html:h1 (hes title))
                body))))))))


(define (tcpcgi:display-cgi-metavariables-cgi-thunk)
  (display-html
    (lambda (params)
      (list
        :title "tcpcgi cgi metavariables"
        :body (list
                (html:dl
                  (map
                    (lambda (x)
                      (list
                        (html:dt (hes (car x)))
                        (html:dd (hes (cadr x)))))
                    (sort
                      (cgi-metavariables)
                      (lambda (x y)
                        (string<? (car x) (car y))))))
                (html:hr)
                (html:h1 "form-parameters")
                (html:dl
                  (map
                    (lambda (x)
                      (list
                        (html:dt (hes (car x)))
                        (map
                          (lambda (xx)
                            (html:dd (hes xx)))
                          (cdr x))))
                    params)))))))

;; note : この中からtcpcgiのgauche.parameterを利用する為には、
;;        with-moduleを使わなくてはならない
(define (tcpcgi:display-debug-information-cgi-thunk)
  (display-html
    (lambda (params)
      (list
        :title "tcpcgi debug information"
        :body (html:dl
                (map
                  (lambda (key&vals)
                    (let ((key (car key&vals))
                          (vals (cdr key&vals)))
                      (list
                        (html:dt (hes (x->string key)))
                        (map
                          (lambda (val)
                            (html:dd (hes (x->string val))))
                          vals))))
                  `(("tcpcgi version" ,(with-module
                                         tcpcgi.version (tcpcgi-version)))
                    ("process id" ,(sys-getpid))
                    ("time" ,(sys-ctime (sys-time)))
                    ;; ToDo : あとで
                    ("other" "sorry, now preparing...")
                    )))))))

(define (tcpcgi:tcpcgi-process-finish-cgi-thunk)
  (display-html
    (lambda (params)
      (sys-kill (sys-getpid) SIGTERM)
      (list
        :title "tcpcgi process finished"
        :body (html:p
                (format
                  "tcpcgi process ~d finished."
                  (sys-getpid)))))))


(provide "tcpcgi/supplement")

