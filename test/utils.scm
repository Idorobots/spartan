;; Testing shenanigans.

(define-syntax assert
  (syntax-rules ()
    ((_ actual expected)
     (let ((a actual)
           (e expected))
       (when (not (equal? a e))
         (display "FAILURE: ") (newline)
         (display "  expression: ") (newline) (pretty-print 'actual) (newline)
         (display "  expected:   ") (newline) (pretty-print e) (newline)
         (display "  actual:     ") (newline) (pretty-print a) (newline)
         (error "Assertion failed: " a "did not equal" e))))
    ((_ actual)
     (assert actual #t))))

(define (run-file filename)
  (with-output-to-string
    (lambda ()
      filename
      (run (parse (slurp filename))))))

(define-syntax test-file
  (syntax-rules ()
      ((_ filename)
       (let ((expected-file (string-append filename ".output")))
         (if (file-exists? expected-file)
             (let ((expected (slurp expected-file)))
               (assert (run-file filename)
                       expected))
             (with-output-to-file expected-file
               (lambda ()
                 (run (parse (slurp filename))))))))))
