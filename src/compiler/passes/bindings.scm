;; Bindings analysis

(require "../utils/set.rkt")
(require "../utils/utils.rkt")

(require "../env.rkt")
(load-once "compiler/pass.scm")
(load-once "compiler/ast.scm")

(define annotate-bindings
  (pass (schema "annotate-bindings"
                'ast (ast-subset? '(const symbol
                                    if do let letrec binding lambda app
                                    primop-app <error>)))
        (lambda (env)
          (env-update env 'ast (partial analyze-bindings #f)))))

(define (analyze-bindings within-letrec? expr)
  (match-ast expr
   ((let _ _)
    (traverse-ast analyze-bindings #f expr))
   ((letrec _ _)
    (traverse-ast analyze-bindings #t expr))
   ((binding _ val)
    (set-ast-binding-complexity
     (set-ast-binding-self-recursive
      (traverse-ast analyze-bindings within-letrec? expr)
      (and within-letrec?
           (not (set-empty? (set-intersection (ast-node-bound-vars expr)
                                              (ast-node-free-vars expr))))))
     (compute-complexity val)))
   (else
    (traverse-ast analyze-bindings within-letrec? expr))))

(define (compute-complexity expr)
  (case (ast-node-type expr)
    ;; Simple values.
    ((const) 'simple)
    ;; Function values, used by letrec-conversion later on.
    ((lambda) 'lambda)
    ;; Any potentially side-effecting expressions.
    (else 'complex)))
