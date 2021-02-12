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
    &env-ref
    &error-handler
    &make-env
    &make-closure
    &make-structure
    &set-env!
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
              (list (make-app '&make-env free)
                    (make-lambda (cons env args)
                                 (substitute (map (lambda (var)
                                                    (cons var
                                                          (make-app '&env-ref
                                                                    (list env
                                                                          (offset var free)))))
                                                  free)
                                             body))))))

(define (cc-fix expr internals)
  ;; NOTE These lambdas have already been converted, so we can modify their envs.
  (let* ((lambdas (fix-bindings expr))
         (lambda-vars (bindings-vars lambdas))
         (lambda-vals (bindings-vals lambdas))
         (lambda-envs (map cadr lambda-vals))
         (env-vars (map (lambda (b)
                          (gensym 'env))
                        lambdas))
         (envs (map (lambda (ev e)
                      (list ev
                            (map (lambda (v)
                                   (if (member v lambda-vars)
                                       ''()
                                       v))
                                 e)))
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
                         (map (lambda (ev e)
                                (map (lambda (v)
                                       (make-app '&set-env!
                                                 (list ev
                                                       (offset v (cdr e))
                                                       v)))
                                     (filter (lambda (v)
                                               (member v lambda-vars))
                                             e)))
                              env-vars
                              lambda-envs))))
    (make-let envs
              (make-let closures
                        (make-do (append setters
                                         (list (fix-body expr))))))))
