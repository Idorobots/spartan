;; Implicit body handling.

(load "compiler/utils.scm")
(load "compiler/env.scm")
(load "compiler/tree-ast.scm")

(define (body-expand env)
  (env-update env 'ast expand-body))

(define (expand-body expr)
  (map-ast id
           (lambda (expr)
             (case (get-type expr)
               ((lambda let letrec) (ast-update expr 'body (flip wrap-with-do (format "Bad `~a` body syntax" (get-type expr)))))
               (else expr)))
           expr))
