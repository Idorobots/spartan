;; Closure conversion.
;; Assumes macro-expanded code.

(load "compiler/utils/utils.scm")
(load "compiler/utils/gensym.scm")

(load "compiler/env.scm")
(load "compiler/substitute.scm")

(define (closure-convert env)
  (env-update env 'ast (flip convert-closures (env-get env 'globals))))

(define (make-global-definitions-list)
  (apply set
         '(nil car cadr cdr cddr list cons append concat
           equal? nil? true false not
           * + - / = < random zero?
           ref deref assign!
           call/current-continuation call/reset call/shift call/handler raise
           sleep self send recv spawn task-info monitor
           assert! signal! retract! select notify-whenever
           display newline debug)))

(define (convert-closures expr globals)
  (let ((expr (walk-ast (flip convert-closures globals)
                        expr)))
    (ast-case expr
     ((app ,op . ,args)
      (replace expr
               (make-primop-app-node
                (at (get-location op)
                    (generated
                     (make-symbol-node '&apply)))
                (cons op args))))
     ((lambda ,formals ,body)
      (let* ((loc (get-location expr))
             (free (set-difference (get-free-vars expr)
                                   globals))
             (env (at loc (make-gensym-node 'env))))
        (at loc
            (generated
             (make-primop-app-node
              (at loc
                  (generated
                   (make-symbol-node '&make-closure)))
              (list (make-env loc free)
                    (replace expr
                             (make-lambda-node
                              (cons env formals)
                              (substitute (make-env-subs env free)
                                          body)))))))))
     ((fix ,bindings ,body)
      todo)
     (else expr))))

(define (make-env loc free)
  (case (length free)
    ((0)
     (at loc
         (generated
          (make-quote-node
           (at loc
               (generated
                (make-list-node '())))))))
    ((1)
     (at loc
         (generated
          (make-symbol-node (car free)))))
    ((2)
     (at loc
         (generated
          (make-primop-app-node
           (at loc
               (generated
                (make-symbol-node '&cons)))
           (map (lambda (var)
                  (at loc
                      (generated
                       (make-symbol-node var))))
                free)))))
    (else
     (at loc
         (generated
          (make-primop-app-node
           (at loc
               (generated
                (make-symbol-node '&make-env)))
           (map (lambda (var)
                  (at loc
                      (generated
                       (make-symbol-node var))))
                free)))))))

(define (make-env-subs env free)
  (case (length free)
    ((1) (list (cons (car free)
                     (flip replace env))))
    ((2) (map (lambda (var accessor)
                (cons var
                      (lambda (expr)
                        (replace expr
                                 (generated
                                  (make-primop-app-node
                                   (at (get-location expr)
                                       (make-symbol-node accessor))
                                   (list env)))))))
              free
              (list '&car '&cdr)))
    (else
     (map (lambda (var)
            (cons var
                  (lambda (expr)
                    (replace expr
                             (generated
                              (make-primop-app-node
                               (at (get-location expr)
                                   (generated
                                    (make-symbol-node '&env-ref)))
                               (list env
                                     (at (get-location expr)
                                         (generated
                                          (make-number-node (offset var free)))))))))))
          free))))

(load "compiler/freevars.scm")
(load "compiler/ast.scm")

(define (old-closure-convert expr globals)
  (walk id
        (lambda (expr)
          (cond ((application? expr)
                 (cc-application expr))
                ((lambda? expr)
                 (cc-lambda expr globals))
                ((fix? expr)
                 (cc-fix expr))
                (else expr)))
        expr))

(define (cc-application expr)
  (let ((op (app-op expr)))
    (if (primop? op)
        expr
        (make-app '&apply
                  (cons op
                        (app-args expr))))))

(define (cc-lambda expr globals)
  (let ((env (gensym 'env))
        (args (lambda-args expr))
        (body (lambda-body expr))
        (free (filter (compose not primop?)
                      (set-difference (free-vars-old expr)
                                      globals))))
    (make-app '&make-closure
              (list (create-env free)
                    (make-lambda (cons env args)
                                 (old-substitute (create-ref-substitutes env free)
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

(define (cc-fix expr)
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
