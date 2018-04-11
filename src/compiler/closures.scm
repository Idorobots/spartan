;; Closure conversion.
;; Assumes macro-expanded code.

(load "compiler/ast.scm")
(load "compiler/utils.scm")

(define (closure-convert expr globals)
  (walk (lambda (expr)
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
                                                            (make-do body)))))))

                ('else expr)))
        expr))

(define (make-global-environment)
  '(&apply
    &env-ref
    &error-handler
    &make-env
    &make-closure
    &set-error-handler!
    &structure-ref
    &yield-cont))

(define (substitute subs expr)
  (walk (lambda (expr)
          (let ((a (assoc expr subs)))
            (if a
                (cdr a)
                expr)))
        expr))

(define (free-vars expr)
  (cond ((symbol? expr) (set expr))
        ((number? expr) (set))
        ((string? expr) (set))
        ((vector? expr) (set))
        ((nil? expr) (set))
        ((char? expr) (set))
        ((quote? expr) (set))
        ((lambda? expr) (set-difference (free-vars (lambda-body expr))
                                        (apply set (lambda-args expr))))
        ((define? expr) (free-vars (define-value expr)))
        ((do? expr) (set-sum (map free-vars
                                  (do-statements expr))))
        ((if? expr) (set-sum (list (free-vars (if-predicate expr))
                                   (free-vars (if-then expr))
                                   (free-vars (if-else expr)))))
        ((let? expr) (set-difference (set-union (set-sum (map (compose free-vars cadr)
                                                                 (let-bindings expr)))
                                                   (free-vars (let-body expr)))
                                        (set-sum (map (compose free-vars car)
                                                      (let-bindings expr)))))
        ((letrec? expr) (set-difference (set-union (set-sum (map (compose free-vars cadr)
                                                                 (let-bindings expr)))
                                                   (free-vars (let-body expr)))
                                        (set-sum (map (compose free-vars car)
                                                      (let-bindings expr)))))
        ((application? expr) (set-union (free-vars (app-op expr))
                                        (foldl set-union
                                               (set)
                                               (map free-vars
                                                    (app-args expr)))))
        ;; These are required by broken letrec implementation.
        ((set!? expr) (free-vars (set!-val expr)))
        ;; These shouldn't be here anymore.
        ((letcc? expr) (set-difference (free-vars (let-body expr))
                                       (set (let-bindings expr))))
        ((reset? expr) (free-vars (reset-expr expr)))
        ((shift? expr) (set-difference (free-vars (shift-expr expr))
                                       (set (shift-cont expr))))
        ((handle? expr) (set-union (free-vars (handle-expr expr))
                                   (free-vars (handle-handler expr))))
        ((raise? expr) (free-vars (raise-expr expr)))
        ;; --
        ('else (error "Unexpected expression:" expr))))
