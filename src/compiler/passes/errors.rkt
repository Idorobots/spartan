#lang racket

;; Error handling within the compiler.

(require "../utils/utils.rkt")
(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require "../errors.rkt")

(provide report-errors)

(define report-errors
  (pass (schema "report-errors"
                'errors a-list?
                'module non-empty-string?
                'input non-empty-string?)
        (lambda (env)
          (let ((errors (env-get env 'errors)))
            (unless (empty? errors)
              (map (partial report-error env)
                   (source-order errors))
              (raise-compilation-error (location 0 0)
                                       (format "Compilation aborted due to ~s errors." (length errors))))
            env))))

(define (report-error env error)
  (let* ((location (compilation-error-location error))
         (what (compilation-error-what error)))
    (display (format-error (env-get env 'module)
                           (env-get env 'input)
                           location
                           what))
    (newline)))
