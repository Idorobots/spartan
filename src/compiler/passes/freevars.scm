;; Free vars computation

(load "compiler/utils/set.scm")
(load "compiler/utils/utils.scm")

(load "compiler/ast.scm")

(define (annotate-free-vars env)
  (env-update env 'ast compute-free-vars))

(define (compute-free-vars expr)
  (map-ast id
           (lambda (expr)
             (ast-case expr
              ((do . ,exprs)
               (free-vars (set-sum (map get-fv exprs))
                          expr))
              ((if ,condition ,then ,else)
               (free-vars (set-sum (list (get-fv condition)
                                         (get-fv then)
                                         (get-fv else)))
                          expr))
              ((lambda ,formals ,body)
               (let ((bound (set-sum (map get-fv formals))))
                  (free-vars
                   (set-difference (get-fv body) bound)
                   (bound-vars bound
                               expr))))
              ((let _ _)
               (compute-let-fv expr))
              ((letrec _ _)
               (compute-letrec-fv expr))
              ((fix _ _)
               (compute-fix-fv expr))
              ((binding ,var ,val)
               (free-vars (get-fv val)
                          (bound-vars (get-fv var)
                                      expr)))
              ((app ,op . ,args)
               (free-vars (set-union (get-fv op)
                                     (set-sum (map get-fv args)))
                          expr))
              ((primop-app _ . ,args)
               (free-vars (set-sum (map get-fv args))
                          expr))
              (else
               expr)))
           expr))

(define (compute-let-fv expr)
  (let* ((bindings (ast-let-bindings expr))
         (bound (set-sum (map get-bound-vars bindings)))
         (free-in-bindings (set-sum (map get-fv bindings)))
         (free-in-body (get-fv (ast-let-body expr))))
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
        (free-in-bindings (set-sum (map get-fv bindings)))
        (free-in-body (get-fv body)))
    (free-vars
     (set-difference (set-union free-in-bindings free-in-body)
                     bound)
     (bound-vars bound
                 expr))))

(define (get-fv expr)
  ;; NOTE Symbols always are their own free var, no need to store that in the AST.
  (if (symbol-node? expr)
      (set (ast-symbol-value expr))
      (get-free-vars expr)))
