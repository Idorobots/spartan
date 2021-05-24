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
             (ast-case expr
              ((do . ,exprs)
               (free-vars (set-sum (map get-free-vars exprs))
                          expr))
              ((if ,condition ,then ,else)
               (free-vars (set-sum (list (get-free-vars condition)
                                         (get-free-vars then)
                                         (get-free-vars else)))
                          expr))
              ((lambda ,formals ,body)
               (let ((bound (set-sum (map get-free-vars formals))))
                  (free-vars
                   (set-difference (get-free-vars body) bound)
                   (bound-vars bound
                               expr))))
              ((let _ _)
               (compute-let-fv expr))
              ((letrec _ _)
               (compute-letrec-fv expr))
              ((fix _ _)
               (compute-fix-fv expr))
              ((binding ,var ,val)
               (free-vars (get-free-vars val)
                          (bound-vars (get-free-vars var)
                                      expr)))
              ((app ,op . ,args)
               (free-vars (set-union (get-free-vars op)
                                     (set-sum (map get-free-vars args)))
                          expr))
              ((primop-app _ . ,args)
               (free-vars (set-sum (map get-free-vars args))
                          expr))
              ((def ,name ,val)
               ;; NOTE This can still occur as a subnode of <error>, so we process it so that we can find more errors in validation.
               (let ((bound (get-free-vars name)))
                 (free-vars (set-difference (get-free-vars val)
                                            bound)
                            (bound-vars bound
                                        expr))))
              (else
               expr)))
           expr))

(define (compute-let-fv expr)
  (let* ((bindings (ast-let-bindings expr))
         (bound (set-sum (map get-bound-vars bindings)))
         (free-in-bindings (set-sum (map get-free-vars bindings)))
         (free-in-body (get-free-vars (ast-let-body expr))))
    (free-vars
     (set-union free-in-bindings
                (set-difference free-in-body
                                bound))
     (bound-vars bound
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
  (let ((bound (set-sum (map get-bound-vars bindings)))
        (free-in-bindings (set-sum (map get-free-vars bindings)))
        (free-in-body (get-free-vars body)))
    (free-vars
     (set-difference (set-union free-in-bindings free-in-body)
                     bound)
     (bound-vars bound
                 expr))))
