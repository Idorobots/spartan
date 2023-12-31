#lang racket

;; The main entry point.

(require "compiler/utils/utils.rkt")
(require "compiler/utils/io.rkt")
(require "compiler/env.rkt")
(require "compiler/compiler.rkt")
(require "runtime/rt.rkt")
(require "rete/rete.rkt")

(provide intern-instrument
         run-code
         run
         run-instrumented
         run-string
         run-instrumented-string
         run-file
         run-instrumented-file
         compile-string
         compile-instrumented-string
         compile-file
         compile-instrumented-file)

(provide (all-from-out "runtime/rt.rkt"))

(provide (all-from-out "rete/rete.rkt"))

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (intern-instrument expr)
  (eval expr ns))

(define (run-code expr)
  (reset-rete!)
  (reset-tasks! '())
  (spawn-task! (&yield-cont (closurize intern-instrument) expr)
               (closurize
                (lambda (e _)
                  (display ";; Execution finished due to an unhandled error: ")
                  (display e)
                  (newline)
                  e)))
  ;; NOTE Returns only the last result.
  (last (execute!)))

(define (run-instrumented expr instrument)
  (run-code
   (compile
    (env 'input (with-output-to-string
                  (lambda ()
                    (pretty-write expr)))
         'module "expr"
         'instrument instrument))))

(define (run expr)
  (run-instrumented expr id))

(define (compile-instrumented-string input instrument)
  (compile
    (env 'input input
         'module "string"
         'instrument instrument)))

(define (compile-string input)
  (compile-instrumented-string input id))

(define (run-instrumented-string input instrument)
  (run-code
   (compile-instrumented-string input instrument)))

(define (run-string input)
  (run-instrumented-string input id))

(define (compile-instrumented-file filename instrument)
  (compile
    (env 'input (slurp filename)
         'module filename
         'instrument instrument)))

(define (compile-file filename)
  (compile-instrumented-file filename id))

(define (run-instrumented-file filename instrument)
  (run-code
   (compile-instrumented-file filename instrument)))

(define (run-file filename)
  (run-instrumented-file filename id))
