;; Closure conversion.
;; Assumes macro-expanded code.

(load "compiler/ast.scm")
(load "compiler/utils.scm")
(load "compiler/freevars.scm")
(load "compiler/substitute.scm")

(define (closure-convert expr internals)
  (walk id
        (lambda (expr)
          (cond ((application? expr)
                 (cc-application expr internals))
                ((lambda? expr)
                 (cc-lambda expr internals))
                ((fix? expr)
                 (cc-fix expr internals))
                (else expr)))
        expr))

(define (make-internal-applicatives)
  '(&apply
    &cons
    &car
    &cdr
    &env-ref
    &error-handler
    &make-env
    &make-closure
    &make-structure
    &set-env!
    &set-closure-env!
    &set-error-handler!
    &structure-binding
    &structure-ref
    &yield-cont
    &push-delimited-continuation!
    &pop-delimited-continuation!))

(define (cc-application expr internals)
  (let ((op (app-op expr)))
    (if (member op internals)
        expr
        (make-app '&apply
                  (cons op
                        (app-args expr))))))

(define (cc-lambda expr internals)
  (let ((env (gensym 'env))
        (args (lambda-args expr))
        (body (lambda-body expr))
        (free (set-difference (free-vars expr)
                              internals)))
    (make-app '&make-closure
              (list (create-env free)
                    (make-lambda (cons env args)
                                 (substitute (create-ref-substitutes env free)
                                             body))))))

(define (create-env free)
  (cond ((= (length free) 0)
         ''())
        ((= (length free) 1)
         (car free))
        ((= (length free) 2)
         (make-app '&cons free))
        (else
         (make-app '&make-env free))))

(define (create-ref-substitutes env free)
  (cond ((= (length free) 1)
         (list (cons (car free)
                     env)))
        ((= (length free) 2)
         (list (cons (car free)
                     (make-app '&car (list env)))
               (cons (cadr free)
                     (make-app '&cdr (list env)))))
        (else
         (map (lambda (var)
                (cons var
                      (make-app '&env-ref
                                (list env
                                      (offset var free)))))
              free))))

(define (cc-fix expr internals)
  ;; NOTE These lambdas have already been converted, so we can modify their envs.
  (let* ((lambdas (fix-bindings expr))
         (lambda-vars (bindings-vars lambdas))
         (lambda-vals (bindings-vals lambdas))
         (lambda-envs (map cadr lambda-vals))
         (env-vars (map (lambda (b)
                          (gensym 'env))
                        lambdas))
         (envs (map (partial patch-env lambda-vars)
                    env-vars
                    lambda-envs))
         (closures (map (lambda (ev b)
                          (list (car b)
                                (make-app '&make-closure
                                          (list ev
                                                (caddr (cadr b))))))
                        env-vars
                        lambdas))
         (setters (foldl append
                         '()
                         (map (partial create-env-setters lambda-vars)
                              env-vars
                              lambda-vars
                              lambda-envs))))
    (make-let envs
              (make-let closures
                        (make-do (append setters
                                         (list (fix-body expr))))))))

(define (patch-env lambda-vars env-var env)
  (cond ((symbol? env)
         (list env-var
               ''()))
        ((and (application? env)
              (equal? (app-op env) '&cons))
         (list env-var
               ''()))
        (else
         (list env-var
               (map (lambda (v)
                      (if (member v lambda-vars)
                          ''()
                          v))
                    env)))))

(define (create-env-setters lambda-vars env-var lambda-var env)
  (cond ((symbol? env)
         (if (member env lambda-vars)
             (list (make-app '&set-closure-env!
                       (list lambda-var
                             env)))
             '()))
        ((and (application? env)
              (equal? (app-op env) '&cons))
         (if (or (member (cadr env) lambda-vars)
                 (member (caddr env) lambda-vars))
             (list (make-app '&set-closure-env!
                             (list lambda-var
                                   env)))
             '()))
        (else
         (map (lambda (v)
                (make-app '&set-env!
                          (list env-var
                                (offset v (cdr env))
                                v)))
              (filter (lambda (v)
                        (member v lambda-vars))
                      env)))))
