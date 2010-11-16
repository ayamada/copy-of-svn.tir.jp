;;; usage:
;;; cat gauche_modules | gosh make_gosh_completions.scm | sort | uniq \
;;; > ~/.gosh_completions

;;; ToDo: automate to getting gauche_modules

(use gauche.interactive)
(define (display-error e where)
  (warn
    (string-append
      "in "
      where
      " : "
      (ref e 'message))))


(let loop ((line (read-line)))
  (if (eof-object? line)
    (with-input-from-string
      (with-output-to-string
        (lambda ()
          (apropos '||)))
      (lambda ()
        (let loop ((line (read-line)))
          (unless (eof-object? line)
            (let1 symbol (read-from-string line)
              (when (guard (e (else
                                (display-error e line)
                                #f))
                      (global-variable-bound? (current-module) symbol))
                (write symbol)
                (newline))
              (loop (read-line)))))))
    (begin
      (with-error-handler
        (cut display-error <> line)
        (lambda ()
          (eval `(use ,(string->symbol line)) (interaction-environment))
          (print line)))
      (loop (read-line)))))
