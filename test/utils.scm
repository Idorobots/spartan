;; Testing shenanigans.

(define-struct test-failed-exception (msg log))

(define-struct assert-exception (predicate expression value expected))

(define (assert->string e)
  (format "~a did not satisfy ~a\n\texpected: ~a\n\treceived: ~a\n"
          (assert-exception-expression e)
          (assert-exception-predicate e)
          (assert-exception-expected e)
          (assert-exception-value e)))

(define-syntax describe
  (syntax-rules ()
    ((describe what body ...)
     (begin
       (display what)
       (newline)
       (map (lambda (thunk)
              (let ()
                (with-handlers ((test-failed-exception?
                               (lambda (e)
                                 (display " !!!FAILURE!!!")
                                 (newline)
                                 (display (test-failed-exception-msg e))
                                 (newline)
                                 (display ";;;;;;;;;;;;;;;;;;;;;")
                                 (newline)
                                 (display "Test log:")
                                 (newline)
                                 (display ";;;;;;;;;;;;;;;;;;;;;")
                                 (newline)
                                 (display (test-failed-exception-log e))
                                 (newline)
                                 (display ";;;;;;;;;;;;;;;;;;;;;")
                                 (newline))))
                  (thunk))))
            (list body ...))))))

(define-syntax ignore
  (syntax-rules ()
    ((ignore what body ...)
     (lambda ()
       (display (string-append "- !!!IGNORED!!!" what))
       (newline)))))

(define-syntax it
  (syntax-rules ()
    ((it what body ...)
     (lambda ()
       (display (string-append "- " what))
       (let* ((output-file (make-temporary-file "~a.log"))
              (result (time-execution
                       (with-handlers ((assert-exception?
                                        (lambda (e)
                                          (raise
                                           (make-test-failed-exception
                                            (assert->string e)
                                            (slurp output-file))))))
                         (with-output-to-file output-file
                           (lambda ()
                             body
                             ...)
                           #:exists 'replace)))))
         (display (format " (~a ms)" (car result)))
         (newline))))))

(define-syntax assert
  (syntax-rules ()
    ((assert value)
     (assert value #t))
    ((assert value expected)
     (assert equal? value expected))
    ((assert predicate value expected)
     (let ((v value)
           (e expected))
       (unless (predicate v e)
         (raise (make-assert-exception predicate 'value v e)))))))

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
                   (collect-garbage)
                   body
                   ...)))
       (if (file-exists? filename)
           (let* ((actual (test))
                  (expected (parse (slurp filename)))
                  (result (compare-perf actual expected factor)))
             (assert <
                     (begin filename
                            result)
                     1)
             (when (< 0 result)
               (spit filename actual)))
           (spit filename (test)))))))
