;; Testing shenanigans.

(define-struct assert-exception (predicate expression value expected))

(define-syntax describe
  (syntax-rules ()
    ((_ what body ...)
     (register-test-suite! what
                           (list body ...)))))

(define-syntax ignore
  (syntax-rules ()
    ((_ what body ...)
     (list 'ignored what (lambda () body ...)))))

(define-syntax it
  (syntax-rules ()
    ((_ what body ...)
     (list 'it what (lambda () body ...)))))

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

(define-syntax test-file
  (syntax-rules ()
    ((_ filename)
     (test-file filename id))
    ((_ filename preprocess)
     (let ((expected-file (string-append filename ".output")))
       (if (file-exists? expected-file)
           (let ((expected (slurp expected-file)))
             (assert (preprocess (run-test-file filename))
                     (preprocess expected)))
           (with-output-to-file expected-file
             (lambda ()
               (write (run-test-file filename)))))))))

(define-syntax time-execution
  (syntax-rules ()
    ((_ body ...)
     (let-values (((_ cpu real gc) (time-apply
                                    (lambda ()
                                      body ...)
                                    '())))
       (list cpu real gc)))))

(define-syntax test-perf
  (syntax-rules ()
    ((_ filename factor)
     (test-perf (string-append filename ".perf") factor
                (time-execution
                 (run-test-file filename))))
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

(define-syntax with-test-bindings
  (syntax-rules ()
    ((_ () body ...)
     (begin body ...))
    ((_ ((var val) bindings ...) body ...)
     (let ((tmp var))
       (set! var val)
       (with-test-bindings (bindings ...) body ...)
       (set! var tmp)))))

(define (assert->string e)
  (format "~a did not satisfy ~a\n\texpected: ~a\n\treceived: ~a\n"
          (assert-exception-expression e)
          (assert-exception-predicate e)
          (assert-exception-expected e)
          (assert-exception-value e)))

(define *registered-test-suites* (ref '()))

(define (register-test-suite! name test-cases)
  (assign! *registered-test-suites*
           (cons (list name test-cases)
                 (deref *registered-test-suites*))))

(define (clear-tests!)
  (assign! *registered-test-suites* '()))

(define (run-all-tests)
  (let ((failed-tests (ref '()))
        (tests (reverse (deref *registered-test-suites*))))
    (map (lambda (test)
           (let ((test-name (car test))
                 (test-cases (cadr test)))
             (display test-name)
             (newline)
             (map (lambda (test-case)
                    (let ((test-case-type (car test-case))
                          (test-case-name (cadr test-case))
                          (test-case-thunk (caddr test-case)))
                      (if (equal? test-case-type 'ignored)
                          (display (format "- ~a - !!!IGNORED!!!~n" test-case-name))
                          (let* ((output-file (make-temporary-file "~a.log"))
                                 (log-failure (lambda (message)
                                                (display (format "- ~a - !!!FAILURE!!!~n" test-case-name))
                                                (display message)
                                                (newline)
                                                (display ";;;;;;;;;;;;;;;;;;;;;")
                                                (newline)
                                                (display "Test log: ")
                                                (display output-file)
                                                (newline)
                                                (display ";;;;;;;;;;;;;;;;;;;;;")
                                                (newline)
                                                (display (slurp output-file))
                                                (newline)
                                                (display ";;;;;;;;;;;;;;;;;;;;;")
                                                (newline)
                                                (assign! failed-tests
                                                         (cons (format "~a - ~a" test-name test-case-name)
                                                               (deref failed-tests)))))
                                 (timing (time-execution
                                          (with-handlers ((assert-exception?
                                                           (lambda (e)
                                                             (log-failure (assert->string e))))
                                                          (exn:fail?
                                                           (lambda (e)
                                                             (log-failure e))))
                                            (with-output-to-file output-file
                                              test-case-thunk
                                              #:exists 'replace)))))
                            (display (format "- ~a (~a ms)~n" test-case-name (car timing)))
                            (delete-file output-file)))))
                  test-cases)))
         tests)
    (unless (empty? (deref failed-tests))
      (display "Tests failed:")
      (newline)
      (map (lambda (fail)
             (display fail)
             (newline))
           (deref failed-tests))
      (error "Some tests have failed!"))))

(define (run-test-file filename)
  (with-output-to-string
    (lambda ()
      (run-string (slurp filename)))))

(define (sort-lines contents)
  (string-join
         (sort (string-split contents "\n")
               string>?)
         "\n"))

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
