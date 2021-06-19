#lang racket

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast/utils.rkt")

(provide debug-ast)

(define debug-ast
  (pass (schema "debug")
        (lambda (env)
          (pretty-print (ast->plain (env-get env 'ast)))
          env)))
