#lang racket

;; Testing shenanigans.

(require "gen.rkt")

(require "../src/main.rkt")
(require "../src/compiler/utils/utils.rkt")
(require "../src/compiler/utils/io.rkt")
(require "../src/compiler/utils/refs.rkt")
(require "../src/compiler/errors.rkt")
(require "../src/compiler/ast/match.rkt")

(provide (all-defined-out))
(provide (all-from-out "gen.rkt"))

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

(define-syntax assert-ast
  (syntax-rules ()
    ((_ expr pattern body ...)
     (match-ast expr
                (pattern body ...)
                (else (raise (make-assert-exception 'match-ast 'expr expr 'pattern)))))))

(define-syntax test-file
  (syntax-rules ()
    ((_ filename)
     (test-file filename id))
    ((_ filename instrument)
     (test-file filename instrument id))
    ((_ filename instrument preprocess)
     (let ((expected-file (string-append filename ".output")))
       (if (file-exists? expected-file)
           (let ((expected (slurp expected-file)))
             (assert (preprocess (run-instrumented-test-file filename instrument))
                     (preprocess expected)))
           (with-output-to-file expected-file
             (lambda ()
               (display (run-instrumented-test-file filename instrument)))))))))

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
                (collect-garbage 'major)
                (time-execution
                 (run-test-file filename))))
    ((_ filename factor body ...)
     (if (file-exists? filename)
         (let ((actual (begin body ...))
               (expected (with-input-from-string (slurp filename) read))
               (not-worse-performance (lambda (a e)
                                        (< (compare-perf a e factor)
                                           1)))
               (better-performance (lambda (a e)
                                        (< (compare-perf a e factor)
                                           0))))
           (assert not-worse-performance
                   actual
                   expected)
           (when (better-performance actual expected)
             (spit filename actual)))
         (spit filename (begin body ...))))))

(define-syntax with-test-bindings
  (syntax-rules ()
    ((_ () body ...)
     (begin body ...))
    ((_ ((var val) bindings ...) body ...)
     (let ((var var))
       (with-test-bindings (bindings ...) body ...)))))

(define-syntax check
  (syntax-rules (sample random)
    ((_ (generators ...) body ...)
     (check 1000 (generators ...) body ...))
    ((_ iterations (generators ...) body ...)
     (let ((seed (random 1234567890)))
       (check iterations seed (generators ...) body ...)))
    ((_ iterations seed ((name generator) ...) body ...)
     (begin
       (display (format "Running ~a property checks using ~a seed.~n" iterations seed))
       (random-seed seed)
       (let loop ((n 0))
         (let* ((name (sample generator random)) ...)
           body ...)
         (when (< n iterations)
           (loop (+ n 1))))))))

(define (assert->string e)
  (format "~a did not satisfy ~a\nexpected:\n~a\nreceived:\n~a\n"
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

(define (run-tests tests failures)
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
                        (display (yellow (format "- ~a - !!!IGNORED!!!~n" test-case-name)))
                        (let* ((output-file (make-temporary-file "~a.log"))
                               (result (ref #t))
                               (log-failure (lambda (message)
                                              (assign! result message)
                                              (assign! failures
                                                       (cons (red (format "- ~a - ~a" test-name test-case-name))
                                                             (deref failures)))))
                               (timing (time-execution
                                        (with-handlers ((assert-exception?
                                                         (lambda (e)
                                                           (log-failure (assert->string e))))
                                                        (compilation-error?
                                                         (lambda (e)
                                                           (log-failure (compilation-error-what e))))
                                                        (exn:fail?
                                                         (lambda (e)
                                                           (log-failure (format "~s~n" e)))))
                                          (with-output-to-file output-file
                                            test-case-thunk
                                            #:exists 'replace)))))
                          (if (string? (deref result))
                              (let ((output (slurp output-file)))
                                (display (red (format "- ~a - !!!FAILURE!!!~n" test-case-name)))
                                (display (red (deref result)))
                                (newline)
                                (unless (zero? (string-length output))
                                  (display (format "Test log: ~a~n" output-file))
                                  (newline)
                                  (display output)
                                  (newline)))
                              (display (green (format "- ~a (~a ms)~n" test-case-name (car timing)))))
                          (delete-file output-file)))))
                test-cases)))
       tests))

(define (run-all-tests test-names)
  (let* ((failed-tests (ref '()))
         (all-tests (reverse (deref *registered-test-suites*)))
         (tests (if (empty? test-names)
                    all-tests
                    (filter (lambda (t)
                              (member (car t)
                                      test-names))
                            all-tests)))
         (total-time (time-execution (run-tests tests failed-tests))))
    (if (empty? (deref failed-tests))
        (begin
          (display (green (format "All ~a tests have passed. (~a ms)" (length tests) (car total-time))))
          (newline))
        (begin
          (display (red "Some tests have failed:"))
          (newline)
          (map (lambda (fail)
                 (display fail)
                 (newline))
               (deref failed-tests))
          (error (red (format "~a tests failed." (length (deref failed-tests)))))))))

(define (run-instrumented-test-file filename instrument)
  (with-output-to-string
    (lambda ()
      ;; NOTE Ignores the compilation abort.
      (with-handlers ((compilation-error?
                       (lambda (e)
                         (display (compilation-error-what e))
                         (newline))))
        (run-instrumented-file filename instrument)))))

(define (run-test-file filename)
  (run-instrumented-test-file filename id))

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
