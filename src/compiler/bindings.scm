
(load "compiler/utils/set.scm")
(load "compiler/utils/utils.scm")
(load "compiler/tree-ast.scm")

(define (annotate-bindings env)
  (env-update env 'ast compute-binding-complexity))

(define (compute-binding-complexity expr)
  (map-ast id
           (lambda (expr)
             (ast-case expr
              ((binding _ ,val)
               (complexity (compute-complexity val)
                          expr))
              (else
               expr)))
           expr))

(define (compute-complexity expr)
  (case (get-type expr)
    ;; Simple values.
    ((number string quote) 'simple)
    ;; Function values, used by letrec-conversion later on.
    ((lambda) 'lambda)
    ;; Any potentially side-effecting expressions.
    (else 'complex)))
