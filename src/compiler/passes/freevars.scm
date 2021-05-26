;; Free vars computation

(load-once "compiler/utils/set.scm")
(load-once "compiler/utils/utils.scm")

(load-once "compiler/env.scm")
(load-once "compiler/pass.scm")
(load-once "compiler/ast.scm")

(define annotate-free-vars
  (pass (schema "annotate-free-vars"
                'ast (ast-subset? '(const symbol
                                    if do let letrec fix binding lambda app ;; NOTE fix, since this pass is used multiple times.
                                    primop-app <error>)))
        (lambda (env)
          (env-update env 'ast compute-free-vars))))

(define (compute-free-vars expr)
  (map-ast (lambda (expr)
             (match-ast expr
              ((do exprs ...)
               (set-ast-node-free-vars (set-sum (map ast-node-free-vars exprs))
                                       expr))
              ((if condition then else)
               (set-ast-node-free-vars (set-sum (list (ast-node-free-vars condition)
                                                      (ast-node-free-vars then)
                                                      (ast-node-free-vars else)))
                                       expr))
              ((lambda formals body)
               (let ((bound (set-sum (map ast-node-free-vars formals))))
                 (set-ast-node-free-vars
                  (set-difference (ast-node-free-vars body) bound)
                  (set-ast-node-bound-vars bound
                                           expr))))
              ((let _ _)
               (compute-let-fv expr))
              ((letrec _ _)
               (compute-letrec-fv expr))
              ((fix _ _)
               (compute-fix-fv expr))
              ((binding var val)
               (set-ast-node-free-vars (ast-node-free-vars val)
                                       (set-ast-node-bound-vars (ast-node-free-vars var)
                                                                expr)))
              ((app op args ...)
               (set-ast-node-free-vars (set-union (ast-node-free-vars op)
                                                  (set-sum (map ast-node-free-vars args)))
                                       expr))
              ((primop-app _ args ...)
               (set-ast-node-free-vars (set-sum (map ast-node-free-vars args))
                                       expr))
              ((def name val)
               ;; NOTE This can still occur as a subnode of <error>, so we process it so that we can find more errors in validation.
               (let ((bound (ast-node-free-vars name)))
                 (set-ast-node-free-vars (set-difference (ast-node-free-vars val)
                                                         bound)
                                         (set-ast-node-bound-vars bound
                                                                  expr))))
              (else
               expr)))
           expr))

(define (compute-let-fv expr)
  (let* ((bindings (ast-let-bindings expr))
         (bound (set-sum (map ast-node-bound-vars bindings)))
         (free-in-bindings (set-sum (map ast-node-free-vars bindings)))
         (free-in-body (ast-node-free-vars (ast-let-body expr))))
    (set-ast-node-free-vars
     (set-union free-in-bindings
                (set-difference free-in-body
                                bound))
     (set-ast-node-bound-vars bound
                              expr))))

(define (compute-letrec-fv expr)
  (compute-rec-fv (ast-letrec-bindings expr)
                  (ast-letrec-body expr)
                  expr))

(define (compute-fix-fv expr)
  (compute-rec-fv (ast-fix-bindings expr)
                  (ast-fix-body expr)
                  expr))

(define (compute-rec-fv bindings body expr)
  (let ((bound (set-sum (map ast-node-bound-vars bindings)))
        (free-in-bindings (set-sum (map ast-node-free-vars bindings)))
        (free-in-body (ast-node-free-vars body)))
    (set-ast-node-free-vars
     (set-difference (set-union free-in-bindings free-in-body)
                     bound)
     (set-ast-node-bound-vars bound
                              expr))))
