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
