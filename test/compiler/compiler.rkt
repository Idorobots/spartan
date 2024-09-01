#lang racket

;; Parse tree validation tests.

(require "../testing.rkt")
(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/env.rkt")
(require "../../src/compiler/compiler.rkt")
(require "../../src/compiler/utils/set.rkt")
(require "../../src/compiler/utils/utils.rkt")
(require "../../src/compiler/utils/gensym.rkt")
(require "../../src/compiler/passes/optimize.rkt")

(describe
 "compiler"
 (it "runs configured optimization"
     (let* ((optimized? #f))
       (compile (env 'module "optimize"
                     'optimize (lambda (passes env)
                                 (set! optimized? #t)
                                 env)
                     'input "(* 2 2)"
                     'globals (set '*)))
       (assert optimized?)))

 (it "doesn't run optimization if disabled"
     (let* ((optimized? #f))
       (compile (env 'module "optimize"
                     'optimize (lambda (passes env)
                                 (set! optimized? #t)
                                 env)
                     'optimization-level 0
                     'input "(* 2 2)"
                     'globals (set '*)))
       (assert (not optimized?))))

 (it "runs configured instrumentation"
     (let* ((instrumented? #f)
            (result (-> (env 'module "instrument"
                             'instrument (lambda (ast)
                                           (set! instrumented? #t)
                                           (make-ast-const (location 5 23)
                                                           (make-ast-number (location 5 23)
                                                                            23.5)))
                             'input "(letrec ((fact (lambda (n)
                                                           (if (= n 0)
                                                               1
                                                               (* n (fact (- n 1)))))))
                                            (fact 120))"
                             'globals (set '= '* '-))
                        (compile)
                        (env-get 'generated))))
       (assert instrumented?)
       (assert result ''23.5)))

 (it "runs the correct phases"
     (let* ((parsed (compile (env 'module "phases"
                                  'input "(define (foo x) x)"
                                  'last-phase 'parse)))
            (expanded (compile (env-set parsed
                                        'input ""
                                        'first-phase 'parse
                                        'last-phase 'expand))))
       (assert (ast->plain (env-get parsed 'ast))
               '(define (foo x) x))
       (assert (ast->plain (env-get expanded 'ast))
               '(define foo (lambda (x) x))))))
