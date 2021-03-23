;; Final frontend code validation.

(load "compiler/utils/utils.scm")

(load "compiler/env.scm")
(load "compiler/tree-ast.scm")
(load "compiler/errors.scm")

(define (validate env)
  (let ((result (collect-errors (env-get env 'errors)
                                (lambda ()
                                  (map-ast id
                                           validate-ast
                                           (env-get env 'ast))))))
    (env-set env
             'ast (car result)
             'errors (cadr result))))

(define (validate-ast expr)
  (case (get-type expr)
    ((def) (raise-compilation-error
            (get-location expr)
            (format "~a, not allowed in this context:" (get-context* expr "Bad `define` syntax"))))
    (else expr)))
