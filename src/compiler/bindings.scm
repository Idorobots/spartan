;; Bindings analysis

(load "compiler/utils/set.scm")
(load "compiler/utils/utils.scm")
(load "compiler/ast.scm")

(define (annotate-bindings env)
  (env-update env 'ast (partial analyze-bindings #f)))

(define (analyze-bindings within-letrec? expr)
  (ast-case expr
   ((let _ _)
    (walk-ast (partial analyze-bindings #f) expr))
   ((letrec _ _)
    (walk-ast (partial analyze-bindings #t) expr))
   ((binding _ ,val)
    (complexity (compute-complexity val)
                (self-recoursive (and within-letrec?
                                      (not (set-empty? (set-intersection (get-bound-vars expr)
                                                                         (get-free-vars expr)))))
                                 (walk-ast (partial analyze-bindings within-letrec?) expr))))
   (else
    (walk-ast (partial analyze-bindings within-letrec?) expr))))

(define (compute-complexity expr)
  (case (get-type expr)
    ;; Simple values.
    ((number string quote) 'simple)
    ;; Function values, used by letrec-conversion later on.
    ((lambda) 'lambda)
    ;; Any potentially side-effecting expressions.
    (else 'complex)))
