#lang racket

;; The main entry point.

(require "compiler/utils/assets.rkt")
(require "compiler/utils/utils.rkt")
(require "compiler/utils/io.rkt")
(require "compiler/env.rkt")
(require "compiler/compiler.rkt")
(require "compiler/passes/rename.rkt") ;; FIXME For symbol->safe
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
         compile-instrumented-file
         ;; FIXME For test access
         bootstrap-core-once!)

(provide (all-from-out "runtime/rt.rkt"))
(provide (all-from-out "rete/rete.rkt"))

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (intern-instrument expr)
  (eval expr ns))

(define *core-interned* #f)
(define +core-spartan+ (embed-file-contents "./runtime/core.sprtn"))

(define (bootstrap-core-once!)
  ;; FIXME This is very hacky, should be replaced when the module system is fleshed out more.
  (unless *core-interned*
    (set! *core-interned* #t)
    (let ((core (run-code (compile-string +core-spartan+))))
      (map (lambda (b)
             (let ((binding `(define ,(symbol->safe (car b)) ,(cdr b))))
               (intern-instrument binding)))
           (cdr core)))))

(define (run-code expr)
  (bootstrap-core-once!)
  (reset-rete!)
  (reset-tasks! '())
  (spawn-task! (make-resumable
                (make-closure
                 '()
                 (lambda (_ expr)
                   (intern-instrument expr)))
                expr)
               (make-closure
                '()
                (lambda (e err restart _)
                  (display ";; Execution finished due to an unhandled error: ")
                  (display err)
                  (newline)
                  err)))
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
