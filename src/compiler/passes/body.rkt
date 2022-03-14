#lang racket

;; Implicit body handling.

(require "../utils/utils.rkt")
(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require "../errors.rkt")

(require (only-in "../expander/syntax-forms.rkt"
                  unique-bindings))

(provide body-expand
         ;; FIXME For test access.
         expand-body)

(define body-expand
  (pass (schema "body-expand"
                'errors a-list?
                'ast (ast-subset? '(quote quasiquote unquote unquote-splicing
                                    number symbol string list
                                    if do let letrec binding lambda app def
                                    primop-app body <error>)))
        (lambda (env)
          (let ((result (collect-errors (env-get env 'errors)
                                        (lambda ()
                                          (expand-body (env-get env 'ast))))))
            (env-set env
                     'ast (car result)
                     'errors (cadr result))))))

(define (expand-body expr)
  (match-ast expr
   ;; FIXME Move this to validation phase.
   ((def name value)
    (raise-compilation-error
     ;; NOTE So that we might find more meaningful errors in the future passes.
     (walk-ast expand-body expr)
     (format "~a, not allowed in this context:" (ast-node-context* expr "Bad `define` syntax"))))
   (else
    (walk-ast expand-body expr))))
