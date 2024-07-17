#lang racket

;; Optimizer tests.

(require "../testing.rkt")
(require "../../src/compiler/env.rkt")
(require "../../src/compiler/compiler.rkt")
(require "../../src/compiler/utils/gensym.rkt")
(require "../../src/compiler/utils/io.rkt")
(require "../../src/compiler/utils/set.rkt")
(require "../../src/compiler/utils/utils.rkt")
(require "../../src/compiler/passes/optimize.rkt")

(describe
 "optimize-naive"
 (it "optimizes the code"
     (gensym-reset!)
     (assert (-> (env 'module "optimize"
                      'input (slurp "examples/math.sprtn")
                      'globals (set '+ '* 'display)
                      'intrinsics '((+ pure) (* pure) (display)))
                 (compile)
                 (env-get 'generated))
             '(begin (define __global10 '(5 1462731 23))
                     (display __global10)))
     (assert (-> (env 'module "optimize"
                      'input "(letrec ((q (lambda () 8))
                                            (f (lambda (x) (+  x (q))))
                                            (r (lambda () (f (q))))
                                            (s (lambda () (+ (r) (f 2))))
                                            (g (lambda () (+ (r) (s))))
                                            (t (lambda () (g))))
                                     (t))"
                      'globals (set '+)
                      'intrinsics '((+ pure)))
                 (compile)
                 (env-get 'generated))
             ''42)
     (assert (-> (env 'module "optimize"
                      'input "(letrec ((fact (lambda (x)
                                                   (if (= 0 x)
                                                       1
                                                       (* x (fact (- x 1)))))))
                                    (fact 2))"
                      'globals (set '- '* '=)
                      'intrinsics '((- pure) (* pure) (= pure)))
                 (compile)
                 (env-get 'generated))
             ''2)))

(describe
 "optimize-super"
 (it "superoptimizes the code"
     (gensym-reset!)
     (assert (-> (env 'module "optimize"
                      'optimize optimize-super
                      'input (slurp "examples/math.sprtn")
                      'globals (set '+ '* 'display)
                      'intrinsics '((+ pure) (* pure) (display)))
                 (compile)
                 (env-get 'generated))
             '(begin (define __global10 '(5 1462731 23))
                     (display __global10)))
     (assert (-> (env 'module "optimize"
                      'optimize optimize-super
                      'input "(letrec ((q (lambda () 8))
                                            (f (lambda (x) (+  x (q))))
                                            (r (lambda () (f (q))))
                                            (s (lambda () (+ (r) (f 2))))
                                            (g (lambda () (+ (r) (s))))
                                            (t (lambda () (g))))
                                     (t))"
                      'globals (set '+)
                      'intrinsics '((+ pure)))
                 (compile)
                 (env-get 'generated))
             ''42))

 (ignore "superoptimizes recursive functions"
         (assert (-> (env 'module "optimize"
                          'optimize optimize-super
                          'input "(letrec ((fact (lambda (x)
                                                   (if (= 0 x)
                                                       1
                                                       (* x (fact (- x 1)))))))
                                    (fact 2))"
                          'globals (set '- '* '=)
                          'intrinsics '((- pure) (* pure) (= pure)))
                     (compile)
                     (env-get 'generated))
                 ''2)))

(describe
 "estimate-performance"
 (it "should ignore abstractions that are likely compiled away"
     (check ((node gen-valid-def-node))
            (assert (estimate-performance node (score-table 'noop 23))
                    23)))

 (it "should estimate simple AST performance"
     (let ((cost (score-table
                  'memory-ref 2
                  'const-ref 1
                  'noop 0)))
       (check ((constant gen-const-node)
               (memref gen-valid-symbol-node))
              (assert (estimate-performance constant cost)
                      1)
              (assert (estimate-performance memref cost)
                      2))))

 (it "should estimate complex AST performance"
     (let ((cost (score-table
                  'allocating-closure 9
                  'unknown-fun 8
                  'call 7
                  'unknown-primop 6
                  'primop-call 5
                  'branch 4
                  'memory-set 3
                  'memory-ref 2
                  'const-ref 1
                  'noop 0)))
       (check ((branch (gen-if-node gen-const-node
                                    gen-const-node
                                    gen-valid-symbol-node))
               (seq-len (gen-integer 0 5))
               (seq (gen-do-node seq-len gen-const-node))
               (formals-len (gen-integer 0 5))
               (formals (gen-arg-list formals-len))
               (body gen-valid-symbol-node)
               (fv-len (gen-integer 0 5))
               (fv (gen-list fv-len gen-valid-symbol))
               (fun (gen-with-fv (gen-lambda-node formals body)
                                 (apply set fv)))
               (name gen-valid-symbol-node)
               (binding (gen-binding-node name fun))
               (args (gen-list formals-len gen-const-node))
               (app (apply gen-app-node name args))
               (node (gen-let-node (list binding)
                                   app)))
              (assert (estimate-performance branch cost)
                      ;; Branch, condition & max of the branches.
                      (+ 4 1 2))
              (assert (estimate-performance seq cost)
                      ;; N constants.
                      (* seq-len 1))
              (assert (estimate-performance fun cost)
                      ;; Allocating closure, N memory reads & sets, no body complexity.
                      (+ 9 (* fv-len (+ 2 3))))
              (assert (estimate-performance app cost)
                      ;; Call, mem ref, unknown fun body & N arg consts.
                      (+ 7 2 8 (* formals-len 1)))
              (assert (estimate-performance node cost)
                      ;; Memory set for the binding, same value as for fun, same value as for app, except known fun body.
                      (+ 3
                         (+ 9 (* fv-len (+ 2 3)))
                         (+ 7 2 2 (* formals-len 1)))))))

 (it "estimates AST performance for any node"
     (check ((node gen-ast-node))
            (assert (>= (estimate-performance node +perf-cost+) 0)))))
