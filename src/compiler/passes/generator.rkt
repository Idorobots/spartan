#lang racket

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast/utils.rkt")

(provide generate-target-code)

(define generate-target-code
  (pass (schema "generate-target-code"
                'ast (ast-subset? '(const symbol if do let binding lambda primop-app)))
        (lambda (env)
          ;; FIXME Actually implement a proper code-gen.
          (ast->plain (env-get env 'ast)))))
