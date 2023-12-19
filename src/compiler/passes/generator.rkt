#lang racket

(require "../env.rkt")
(require "../pass.rkt")
(require "../targets/js/generator.rkt")
(require "../targets/scheme/generator.rkt")

(provide generate-target-code)

(define generate-target-code
  (pass (schema "generate-target-code"
                'data (list-of? (a-pair? a-symbol?
                                         (ast-subset? '(const symbol if do let binding lambda primop-app))))
                'init (ast-subset? '(const symbol if do let binding primop-app))
                'target a-symbol?)
        (lambda (env)
          (case (env-get env 'target)
            ((ECMAScript6)
             (generate-js env))
            (else
             (generate-scheme env))))))
