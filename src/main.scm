;; The main entry point.

(load "compiler/compiler.scm")
(load "compiler/parser.scm")
(load "runtime/bootstrap.scm")
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
  (run-code (compile input)))

(define (run expr)
  (run-string
   (with-output-to-string
     (lambda ()
       (pretty-write expr)))))

(define (run-file filename)
  (run-string (slurp filename)))
