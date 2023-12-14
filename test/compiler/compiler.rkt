#lang racket

;; Parse tree validation tests.

(require "../testing.rkt")
(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/env.rkt")
(require "../../src/compiler/compiler.rkt")
(require "../../src/compiler/utils/gensym.rkt")
(require "../../src/compiler/passes/optimize.rkt")

(describe
 "compiler"
 (it "finds the expected errors"
     (map (lambda (filename)
            (test-file filename))
          (filter (lambda (filename)
                    (string-suffix? filename ".sprtn"))
                  (map (lambda (path)
                         (string-append "test/sprtn/errors/"
                                        (path->string path)))
                       (directory-list "test/sprtn/errors/")))))

 (it "runs configured optimization"
     (let* ((optimized? #f)
            (result (compile (env 'module "optimize"
                                  'optimize (lambda (env passes)
                                              (set! optimized? #t)
                                              (optimize-naive env passes))
                                  'input "(letrec ((q (lambda () 8))
                                            (f (lambda (x) (+  x (q))))
                                            (r (lambda () (f (q))))
                                            (s (lambda () (+ (r) (f 2))))
                                            (g (lambda () (+ (r) (s))))
                                            (t (lambda () (g))))
                                     (t))"))))
       (assert optimized?)
       (assert result ''42)))

 (it "doesn't run optimization if disabled"
     (let* ((optimized? #f)
            (result (compile (env 'module "optimize"
                                  'optimize (lambda (env passes)
                                              (set! optimized? #t)
                                              (optimize-naive env passes))
                                  'optimization-level 0
                                  'input "(* 2 2)"))))
       (assert (not optimized?))))

 (it "runs configured instrumentation"
     (let* ((instrumented? #f)
            (result (compile (env 'module "instrument"
                                  'instrument (lambda (ast)
                                                (set! instrumented? #t)
                                                (make-ast-const (location 5 23)
                                                                (make-ast-number (location 5 23)
                                                                                 23.5)))
                                  'input "(letrec ((fact (lambda (n)
                                                           (if (= n 0)
                                                               1
                                                               (* n (fact (- n 1)))))))
                                            (fact 120))"))))
       (assert instrumented?)
       (assert result ''23.5))))
