;;; usage:
;;; cat .gosh_completions | gosh typestat.scm

;; load all modules
(use file.util)
(define *gauche_modules* (expand-path "~/nekoie/scripts/gauche_modules"))
(for-each
  (lambda (line)
    (with-error-handler
      (lambda (e)
        (warn (ref e 'message)))
      (lambda ()
        (eval `(use ,(string->symbol line)) (interaction-environment)))))
  (file->list read-line *gauche_modules*))

(newline)

(define *results* (make-hash-table))

;; main
(let loop ((line (read-line)))
  (if (eof-object? line)
    (hash-table-for-each
      *results*
      (lambda (key val)
        (display key)
        (display " : ")
        (display val)
        (newline)
        (flush)))
    (let1 symbol (string->symbol line)
      (with-error-handler
        (lambda (e)
          (hash-table-update! *results* 'module (cut + 1 <>) 0))
        (lambda ()
          (hash-table-update!
            *results*
            (eval `(class-of ,symbol) (interaction-environment))
            (cut + 1 <>)
            0)))
      (loop (read-line)))))
