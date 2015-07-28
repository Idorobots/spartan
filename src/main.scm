;; The main entry point.

(load "compiler/compiler.scm")
(load "compiler/parser.scm")
(load "runtime/bootstrap.scm")
(load "rete/rete.scm")

(define (do-expr expr)
  (eval/pp (compile/pp expr)))

(define (do-string str)
  (eval/pp (compile/pp (parse str))))

(define (do-file filename)
  (eval/pp (compile/pp (parse (slurp filename)))))

(define (compile/pp expr)
  (display ";; Code:")
  (newline)
  (pretty-print expr)
  (compile expr))

(define (eval/pp expr)
  (display ";; Compiled code:")
  (newline)
  (pretty-print expr)
  (display ";; Result:")
  (newline)
  (eval expr))

(define (run expr)
  (reset-rete!)
  (reset-tasks! nil)
  (spawn-task! (&yield-cont do-expr expr)
               (lambda (e _)
                 (display ";; Execution finished due to an unhandled error: ")
                 (display e)
                 (newline)
                 e))
  ;; NOTE Returns only the last result.
  (last (execute!)))
