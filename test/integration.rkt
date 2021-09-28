#lang racket

;; Actual code exmaples

(require "testing.rkt")
(require "../src/main.rkt")
(require "../src/runtime/rt.rkt")
(require "../src/rete/rete.rkt")
(require "../src/compiler/ast.rkt")
(require "../src/compiler/utils/utils.rkt")

;; NOTE The test framework evaluates these code fragments in an environment that needs to have access to these values.
(provide __test_task_info __test_monitor test-sleep test-random)

;; Silence task info logs since these might vary in the specific timings.
(define __test_task_info (bootstrap (lambda () '())))

;; Ensure that monitor task doesn't ever hang the execution.
(define __test_monitor (bootstrap (lambda (time) '())))

;; Ensure that timeouts take very short time.
(define (test-sleep time)
  (sleep (min time 25)))

;; Determined by a fairly random dice roll.
(define *random* 0.05)
(define (test-random)
  (let ((r *random*))
    (set! *random* (+ r 0.05))
    (when (> *random* 1.0)
      (set! *random* 0.05))
    r))

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
 "Spartan"
 (it "should support basic language features"
     (test-file "test/sprtn/math.sprtn")
     (test-file "test/sprtn/rsa.sprtn")
     (test-file "test/sprtn/hello.sprtn")
     (test-file "test/sprtn/fibonacci.sprtn")
     (test-file "test/sprtn/logger.sprtn"))

 (it "should support continuations"
     ;; NOTE Should not be instrumented to maximize the chance of colliding continuations when using a single delimited stack.
     (test-file "test/sprtn/continuations.sprtn" sort-lines)
     (test-file "test/sprtn/errors.sprtn" id instrument-for-test)
     (test-file "test/sprtn/errors3.sprtn" id instrument-for-test)
     (test-file "test/sprtn/coroutines.sprtn")
     (test-file "test/sprtn/coroutines2.sprtn")
     (test-file "test/sprtn/coroutines3.sprtn")
     (test-file "test/sprtn/amb.sprtn"))

 (it "should support Actor Model"
     (test-file "test/sprtn/uprocs.sprtn" id instrument-for-test)
     (test-file "test/sprtn/uprocs2.sprtn" sort-lines instrument-for-test)
     (test-file "test/sprtn/msgwait.sprtn")
     (test-file "test/sprtn/fibonacci2.sprtn" id instrument-for-test)
     (test-file "test/sprtn/errors2.sprtn" id instrument-for-test))

 (it "should support the RBS"
     (test-file "test/sprtn/rbs2.sprtn")
     (test-file "test/sprtn/rbs.sprtn" id instrument-for-test)
     (test-file "test/sprtn/cep.sprtn" id instrument-for-test))

 (it "handles reused variables correctly"
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
