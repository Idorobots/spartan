#lang racket

;; Macro-expander pass definition

(require "../utils/utils.rkt")
(require "../expander/expander.rkt")
(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require "../errors.rkt")

(provide macro-expand make-static-environment)

(define macro-expand
  (pass (schema "macro-expand"
                'static-env non-empty-hash?
                'errors a-list?
                'ast (ast-subset? '(quote quasiquote unquote unquote-splicing
                                    number symbol string list
                                    primop-app body <error>)))
        (lambda (env)
          (let ((result (collect-errors (env-get env 'errors)
                                        (lambda ()
                                          (expand (env-get env 'static-env)
                                                  (env-get env 'ast))))))
            (env-set env
                     'ast (car result)
                     'errors (cadr result))))))
