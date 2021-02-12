;; Closure conversion.
;; Assumes macro-expanded code.

(load "compiler/ast.scm")
(load "compiler/utils.scm")
(load "compiler/freevars.scm")

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
                ('else expr)))
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

;; FIXME Rewrite in terms of ast/walk.
(define (substitute subs expr)
  (cond ((empty? subs)
         expr)
        ((simple? expr)
         expr)
        ((symbol? expr)
         (let ((a (assoc expr subs)))
           (if a
               (cdr a)
               expr)))
        ((or (let? expr)
             (letrec? expr))
         (let* ((bindings (let-bindings expr))
                (body (let-body expr))
                (vars (bindings-vars bindings))
                (unbound-subs (filter (lambda (s)
                                        (not (member (car s) vars)))
                                      subs))
                (derefied-bindings (map (lambda (b)
                                          (list (car b)
                                                (substitute unbound-subs (cadr b))))
                                        bindings)))
           ((if (let? expr)
                make-let
                make-letrec)
            derefied-bindings
            (substitute unbound-subs body))))
        ((lambda? expr)
         (let ((vars (lambda-args expr)))
           (make-lambda vars
                        (substitute (filter (lambda (s)
                                              (not (member (car s) vars)))
                                            subs)
                                    (lambda-body expr)))))
        ((pair? expr)
         (cons (substitute subs (car expr))
               (substitute subs (cdr expr))))
        (else expr)))

