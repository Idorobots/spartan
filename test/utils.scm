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

(define (sort-lines contents)
  (string-join
         (sort (string-split contents "\n")
               string>?)
         "\n"))

(define-syntax test-file
  (syntax-rules ()
    ((_ filename)
     (test-file filename id))
    ((_ filename preprocess)
     (let ((expected-file (string-append filename ".output")))
       (if (file-exists? expected-file)
           (let ((expected (slurp expected-file)))
             (assert (preprocess (run-file filename))
                     (preprocess expected)))
           (with-output-to-file expected-file
             (lambda ()
               (write (run-file filename)))))))))

(define-syntax time-execution
  (syntax-rules ()
    ((_ body ...)
     (let-values (((_ cpu real gc) (time-apply
                                    (lambda ()
                                      body ...)
                                    '())))
       (list cpu real gc)))))

(define (compare-perf actual expected factor)
  (cond ((and (number? actual)
              (number? expected))
         (cond ((> actual (* factor expected))
                1)
               ((< actual expected)
                -1)
               (else 0)))
        ((and (pair? actual)
              (pair? expected))
         (max (compare-perf (car actual)
                            (car expected)
                            factor)
              (compare-perf (cdr actual)
                            (cdr expected)
                            factor)))
        ((and (null? actual)
              (null? expected)
              -1))
        (else (error "Invalid performance values supplied: " actual expected))))

(define-syntax test-perf
  (syntax-rules ()
    ((_ filename factor)
     (test-perf (string-append filename ".perf") factor
                (time-execution
                 (run-file filename))))
    ((_ filename factor body ...)
     (let ((test (lambda ()
                   body
                   ...)))
       (if (file-exists? filename)
           (let* ((actual (test))
                  (expected (parse (slurp filename)))
                  (result (compare-perf actual expected factor)))
             (assert (begin filename
                            (< result 1)))
             (when (< 0 result)
               (spit filename actual)))
           (spit filename (test)))))))
