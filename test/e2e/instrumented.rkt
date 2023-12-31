#lang racket

;; Code examples that require instrumentation.

(require "../testing.rkt")
(require "../../src/main.rkt")
(require "../../src/runtime/rt.rkt")
(require "../../src/rete/rete.rkt")
(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/utils/utils.rkt")

;; Silence task info logs since these might vary in the specific timings.
(intern-instrument
 '(define __test_task_info (bootstrap (lambda () '()))))

;; Ensure that monitor task doesn't ever hang the execution.
(intern-instrument
 '(define __test_monitor (bootstrap (lambda (time) '()))))

;; Ensure that timeouts take very short time.
(intern-instrument
 '(define (test-sleep time)
  (sleep (min time 25))))

;; Determined by a fairly random dice roll.
(intern-instrument
 '(define *random* 0.05))
(intern-instrument
 '(define (test-random)
  (let ((r *random*))
    (set! *random* (+ r 0.05))
    (when (> *random* 1.0)
      (set! *random* 0.05))
    r)))

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
              ((primop-app 'random args ...)
               (set-ast-primop-app-op expr 'test-random))
              ((primop-app 'sleep args ...)
               (set-ast-primop-app-op expr 'test-sleep))
              (else
               expr)))
           ast))

(describe
 "instrumented r7rs target"

 (it "should support continuations"
     (test-instrumented-file "examples/errors.sprtn" instrument-for-test)
     (test-instrumented-file "examples/errors3.sprtn" instrument-for-test))

 (it "should support Actor Model"
     (test-instrumented-file "examples/uprocs.sprtn" instrument-for-test)
     (test-instrumented-file "examples/uprocs2.sprtn" instrument-for-test sort-lines)
     (test-instrumented-file "examples/fibonacci2.sprtn" instrument-for-test)
     (test-instrumented-file "examples/errors2.sprtn" instrument-for-test))

 (it "should support the RBS"
     (test-instrumented-file "examples/rbs.sprtn" instrument-for-test)
     (test-instrumented-file "examples/cep.sprtn" instrument-for-test))

 (it "handles reused variables correctly"
     (assert (run '(let ((n 23))
                     (+ n (let ((n (- n 1)))
                            n))))
             45)
     (assert (run '(letrec ((fact (lambda (n)
                                    (if (< n 2)
                                        n
                                        (* n ;; NOTE the `n` here would be propagated into the `let` resulting in wrong computation.
                                           (let ((n (- n 1)))
                                             (if (< n 2)
                                                 n
                                                 (* n (fact (- n 1))))))))))
                     (if (< 10 2)
                         10
                         (* 10 (fact 9)))))
             3628800))

 (it "handles free variables in inlined procedures correctly"
     (assert (run '(let ((x 23))
                     (let ((foo (lambda ()
                                  ;; NOTE The `x` here would point to the inner `x = 5` binding after `foo` got inlined.
                                  x))
                           (x 5))
                       (+ x (foo)))))
             28))

 (it "optimizes bindings out correctly"
     (assert (run '(list
                    (let ((x1 '7)) (if (= '0 x1) 't nil))
                    ;; NOTE This `x2` would remain as a free variable (and therefore an undefined variable) despite being optimized out.
                    (let ((x2 '7)) (if (= '0 x2) nil 't))))
             '(() t))))