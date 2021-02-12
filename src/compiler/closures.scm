;; Closure conversion.
;; Assumes macro-expanded code.

(load "compiler/ast.scm")
(load "compiler/utils.scm")
(load "compiler/freevars.scm")
(load "compiler/substitute.scm")

(define (closure-convert expr globals)
  (walk id
        (lambda (expr)
          (cond ((application? expr)
                 (let ((op (app-op expr)))
                   (if (member op globals)
                       expr
                       (make-app '&apply
                                 (cons op
                                       (app-args expr))))))
                ((lambda? expr)
                 (let ((env (gensym 'env))
                       (args (lambda-args expr))
                       (body (lambda-body expr))
                       (free (set-difference (free-vars expr)
                                             globals)))
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
                (else expr)))
        expr))

(define (make-global-environment)
  '(&apply
    &env-ref
    &error-handler
    &make-env
    &make-closure
    &make-structure
    &set-error-handler!
    &structure-binding
    &structure-ref
    &yield-cont
    &push-delimited-continuation!
    &pop-delimited-continuation!))

