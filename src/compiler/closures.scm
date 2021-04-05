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
        (make-primop-app loc
                         '&make-closure
                         (list (make-env loc free)
                               (replace expr
                                        (make-lambda-node
                                         (cons env formals)
                                         (substitute (make-env-subs env free)
                                                     body)))))))
     ;; FIXME This should create a single shared env for all the tightly bound closures that end up in this fix.
     ((fix ,bindings ,body)
      (let* ((bound (get-bound-vars expr))
             (original-envs (map (compose car ast-primop-app-args ast-binding-val) bindings))
             (env-vars (map (lambda (env)
                              (at (get-location env)
                                  (make-gensym-node 'env)))
                            original-envs))
             (envs (map (lambda (var e)
                          (at (get-location e)
                              (generated
                               (make-binding-node var
                                                  ;; Replaces any recoursive variables with nils not to cause undefined vars.
                                                  (substitute (map (flip cons (compose make-nil get-location)) bound)
                                                              e)))))
                        env-vars
                        original-envs))
             (closures (map (lambda (b e)
                              (ast-update b
                                          'val
                                          (lambda (v)
                                            (ast-update v
                                                        'args
                                                        (lambda (args)
                                                          ;; Effectively replaces the env with associated env var.
                                                          (cons e (cdr args)))))))
                            bindings
                            env-vars))
             (setters (foldl append
                             '()
                             (map (partial make-env-setters bound)
                                  env-vars
                                  (map ast-binding-var closures)
                                  original-envs))))
        (at (get-location expr)
            (generated
             (make-let-node envs
                            (replace expr
                                     (make-let-node closures
                                                    (at (get-location body)
                                                        (generated
                                                         (make-do-node
                                                          (append setters
                                                                  (list body))))))))))))
     (else expr))))

(define (make-nil loc)
  (at loc
      (generated
       (make-quote-node
        (at loc
            (generated
             (make-list-node '())))))))

(define (make-primop-app loc primop args)
  (at loc
      (generated
       (make-primop-app-node
        (at loc
            (generated
             (make-symbol-node primop)))
        args))))

(define (make-env loc free)
  (case (length free)
    ((0)
     (make-nil loc))
    ((1)
     (at loc
         (generated
          (make-symbol-node (car free)))))
    ((2)
     (make-primop-app loc
                      '&cons
                      (map (lambda (var)
                             (at loc
                                 (generated
                                  (make-symbol-node var))))
                           free)))
    (else
     (make-primop-app loc
                      '&make-env
                      (map (lambda (var)
                             (at loc
                                 (generated
                                  (make-symbol-node var))))
                           free)))))

(define (make-env-subs env free)
  (case (length free)
    ((1)
     (list (cons (car free)
                 (flip replace env))))
    ((2)
     (map (lambda (var accessor)
            (cons var
                  (lambda (expr)
                    (make-primop-app (get-location expr)
                                     accessor
                                     (list env)))))
          free
          (list '&car '&cdr)))
    (else
     (map (lambda (var)
            (cons var
                  (lambda (expr)
                    (let ((loc (get-location expr)))
                      (make-primop-app loc
                                       '&env-ref
                                       (list env
                                             (at loc
                                                 (generated
                                                  (make-number-node (offset var free))))))))))
          free))))

(define (make-env-setters bound env-var closure-var env)
  (ast-case env
   ((symbol ,sym)
    (if (set-member? bound (ast-symbol-value sym))
        (list
         (make-primop-app (get-location env)
                          '&set-closure-env!
                          (list closure-var env)))
        '()))
   ((primop-app '&cons ,first ,second)
    (if (or (set-member? bound (ast-symbol-value first))
            (set-member? bound (ast-symbol-value second)))
        (list
         (make-primop-app (get-location env)
                          '&set-closure-env!
                          (list closure-var env)))
        '()))
   (else
    (ast-update env
                'args
                (lambda (args)
                  (let loop ((i 0)
                             (args args))
                    (if (empty? args)
                        '()
                        (let ((arg (car args)))
                          (if (set-member? bound (ast-symbol-value arg))
                              (cons
                               (make-primop-app (get-location arg)
                                                '&set-env!
                                                (list env-var
                                                      (at (get-location arg)
                                                          (generated
                                                           (make-number-node i)))))
                               (loop (+ i 1) (cdr args)))
                              (loop (+ i 1) (cdr args)))))))))))

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
