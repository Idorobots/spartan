;; The main entry point.

(load "compiler/env.scm")
(load "compiler/compiler.scm")
(load "runtime/rt.scm")
(load "rete/rete.scm")

(define (run-code expr)
  (reset-rete!)
  (reset-tasks! nil)
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
         'module 'string))))

(define (run expr)
  (run-code
   (compile
    (env 'input (with-output-to-string
                  (lambda ()
                    (pretty-write expr)))
         'module 'expr))))

(define (run-file filename)
  (run-code
   (compile
    (env 'input (slurp filename)
         'module filename))))
