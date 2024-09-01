#lang racket

;; Code examples that require instrumentation.

(require "../testing.rkt")
(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/utils/utils.rkt")
(require "../../src/main.rkt")
(require "../../src/runtime/rt.rkt")
(require "../../src/runtime/processes.rkt")
(require "../../src/runtime/closures.rkt")
(require "../../src/runtime/continuations.rkt")
(require "../../src/runtime/bootstrap.rkt")

(define *instruments-bootstrapped* #f)

(define (bootstrap-instruments!)
  (let ((rt (bootstrap-rt!)))
    ;; FIXME RT reuses its namespace for the evaluation, so this can be done only once.
    (unless *instruments-bootstrapped*
      (set! *instruments-bootstrapped* #t)
      (import-defaults! rt)

      ;; Silence task info logs since these might vary in the specific timings.
      (rt-define! rt
                  'test-task-info
                  (make-closure '()
                                (lambda (env cont)
                                  (make-resumable cont '()))))

      ;; Ensure that monitor task doesn't ever hang the execution.
      (rt-define! rt
                  'test-monitor
                  (make-closure '()
                                (lambda (env interval cont)
                                  (make-resumable cont interval))))

      ;; Ensure that timeouts take very short time.
      (rt-define! rt
                  'test-sleep
                  (make-closure '()
                                (lambda (env delay cont)
                                  ;; FIXME Not quite the same as bumping the rtime of the current process.
                                  (delay-milliseconds (min delay 25))
                                  (make-resumable cont delay))))

      ;; Determined by a fairly random dice roll.
      (rt-define! rt
                  'test-random
                  (make-closure '()
                                (let ((*random* 0.05))
                                  (lambda (env cont)
                                    (let ((r *random*))
                                      (set! *random* (+ r 0.05))
                                      (when (> *random* 1.0)
                                        (set! *random* 0.05))
                                      (make-resumable cont r)))))))
    rt))

(define (instrument-for-test ast)
  (map-ast (lambda (expr)
             (match-ast expr
              ((app (symbol 'task-info) args ...)
               (set-ast-app-op expr
                               (set-ast-symbol-value (ast-app-op expr)
                                                     'test-task-info)))

              ((app (symbol 'monitor) args ...)
               (set-ast-app-op expr
                               (set-ast-symbol-value (ast-app-op expr)
                                                     'test-monitor)))

              ((app (symbol 'sleep) args ...)
               (set-ast-app-op expr
                               (set-ast-symbol-value (ast-app-op expr)
                                                     'test-sleep)))

              ((app (symbol 'random) args ...)
               (set-ast-app-op expr
                               (set-ast-symbol-value (ast-app-op expr)
                                                     'test-random)))

              (else
               expr)))
           ast))

(describe
 "instrumented r7rs target"

 (it "should support exceptions"
     (define rt (bootstrap-instruments!))
     (test-instrumented-file rt "examples/errors.sprtn" instrument-for-test)
     ;; FIXME Needs line sorting as the wait-list scheduler sleep implementation is now much different than just sleeping for a time.
     (test-instrumented-file rt "examples/errors2.sprtn" instrument-for-test sort-lines)
     (test-instrumented-file rt "examples/errors3.sprtn" instrument-for-test))

 (it "should support the RBS"
     (define rt (bootstrap-instruments!))
     (test-instrumented-file rt "examples/rbs.sprtn" instrument-for-test)
     (test-instrumented-file rt "examples/cep.sprtn" instrument-for-test)))
