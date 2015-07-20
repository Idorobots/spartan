;; Testing shenanigans.


(define-syntax assert
  (syntax-rules ()
    ((_ actual expected)
     (let ((a actual)
           (e expected))
       (when (not (equal? a e))
         (display "FAILURE: ") (newline)
         (display "  expression: ") (display 'actual) (newline)
         (display "  expected:   ") (display e) (newline)
         (display "  actual:     ") (display a) (newline))))
    ((_ actual)
     (assert actual #t))))
