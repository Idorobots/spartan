#lang racket

;; Code examples that require instrumentation.

(require "../testing.rkt")
(require "../../src/main.rkt")
(require "../../src/runtime/rt.rkt")
(require "../../src/runtime/closures.rkt")
(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/utils/utils.rkt")

(define (bootstrap-instruments!)
  (let ((rt (bootstrap-rt!)))
    (import-defaults! rt)

    ;; Silence task info logs since these might vary in the specific timings.
    (rt-define! rt
                'test-task-info
                (make-closure '()
                              (lambda (env cont)
                                (cont '()))))

    ;; Ensure that monitor task doesn't ever hang the execution.
    (rt-define! rt
                'test-monitor
                (make-closure '()
                              (lambda (env interval cont)
                                (cont interval))))

    ;; Ensure that timeouts take very short time.
    (rt-define! rt
                'test-sleep
                (make-closure '()
                              (lambda (env delay cont)
                                (sleep (min delay 25))
                                (cont delay))))

    ;; Determined by a fairly random dice roll.
    (rt-define! rt
                'test-random
                (make-closure '()
                              (let ((*random* 0.05))
                                (lambda (env x cont)
                                  (let ((r *random*))
                                    (set! *random* (+ r 0.05))
                                    (when (> *random* 1.0)
                                      (set! *random* 0.05))
                                    (cont r))))))
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

 (ignore "should support continuations"
     (bootstrap-instruments!)
     ;; FIXME This should take the RT created by bootstrap-instruments.
     (test-instrumented-file "examples/errors.sprtn" instrument-for-test)
     (test-instrumented-file "examples/errors3.sprtn" instrument-for-test))

 (ignore "should support Actor Model"
     (bootstrap-instruments!)
     ;; FIXME This should take the RT created by bootstrap-instruments.
     (test-instrumented-file "examples/uprocs.sprtn" instrument-for-test)
     (test-instrumented-file "examples/uprocs2.sprtn" instrument-for-test sort-lines)
     (test-instrumented-file "examples/fibonacci2.sprtn" instrument-for-test)
     (test-instrumented-file "examples/errors2.sprtn" instrument-for-test))

 (ignore "should support the RBS"
     (bootstrap-instruments!)
     ;; FIXME This should take the RT created by bootstrap-instruments.
     (test-instrumented-file "examples/rbs.sprtn" instrument-for-test)
     (test-instrumented-file "examples/cep.sprtn" instrument-for-test)))
