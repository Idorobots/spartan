;; The main entry point.

(define *imported-modules* '())

(define-syntax load-once
  (syntax-rules (*imported-modules*)
    ((load-once file)
     (unless (member file *imported-modules*)
       (set! *imported-modules* (cons file *imported-modules*))
       (load file)))))

(require "compiler/utils/io.rkt")
(require "compiler/env.rkt")
(require "compiler/compiler.rkt")
(load-once "runtime/rt.scm")
(load-once "rete/rete.scm")

(define (run-code expr)
  (reset-rete!)
  (reset-tasks! '())
  (spawn-task! (&yield-cont (closurize eval) expr)
               (closurize
                (lambda (e _)
                  (display ";; Execution finished due to an unhandled error: ")
                  (display e)
                  (newline)
                  e)))
  ;; NOTE Returns only the last result.
  (last (execute!)))

(define (run-string input)
  (run-code
   (compile
    (env 'input input
         'module "string"))))

(define (run expr)
  (run-code
   (compile
    (env 'input (with-output-to-string
                  (lambda ()
                    (pretty-write expr)))
         'module "expr"))))

(define (run-file filename)
  (run-code
   (compile
    (env 'input (slurp filename)
         'module filename))))
