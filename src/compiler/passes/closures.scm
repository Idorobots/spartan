;; Closure conversion.
;; Assumes macro-expanded code.

(load-once "compiler/utils/utils.scm")
(load-once "compiler/utils/gensym.scm")

(load-once "compiler/env.scm")
(load-once "compiler/pass.scm")
(load-once "compiler/ast.scm")
(load-once "compiler/substitute.scm")

(define closure-convert
  (pass (schema "closure-convert"
                'globals a-list?
                'ast (ast-subset? '(const symbol
                                    if do let fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (flip convert-closures (env-get env 'globals))))))

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
  (ast-case expr
   ((app ,op . ,args)
    (replace expr
             (at (get-location op)
                 (make-primop-app-node
                  '&apply
                  (map (flip convert-closures globals)
                       (cons op args))))))
   ((lambda ,formals ,body)
    (let ((free (set-difference (get-free-vars expr)
                                globals)))
      (recreate-closure expr
                        ;; NOTE This is an anonymous function, so we can't really reuse it's names location for the environment.
                        (make-env (get-location expr) free '())
                        (flip make-env-subs free)
                        globals)))
   ((fix ,bindings ,body)
    (let* ((loc (get-location expr))
           (env-var (at loc (make-gensym-node 'env)))
           (free (set-difference (set-sum (map get-free-vars bindings))
                                 globals))
           (env-subs (flip make-env-subs free))
           (closures (map (lambda (b)
                            (ast-update b 'val
                                        (lambda (fun)
                                          (recreate-closure fun
                                                            env-var
                                                            env-subs
                                                            globals))))
                          bindings))
           (closure-vars (map ast-binding-var closures))
           (bound (get-bound-vars expr))
           (full-env (make-env loc free closure-vars))
           ;; NOTE So that we don't reference undefined (yet) variables.
           (actual-env (substitute-symbols
                        (map (flip cons (compose make-nil get-location)) bound)
                        full-env))
           (env-binding (at loc (make-binding-node env-var actual-env)))
           (setters (make-env-setters full-env env-var free bound closure-vars))
           (converted-body (convert-closures body globals)))
      (replace expr
               (generated
                (make-let-node (list env-binding)
                               (at loc
                                   (generated
                                    (make-let-node closures
                                                   (at loc
                                                       (generated
                                                        (make-do-node
                                                         (append setters
                                                                 (list converted-body)))))))))))))
   (else (walk-ast (flip convert-closures globals) expr))))

(define (recreate-closure expr env make-subs globals)
  (let* ((formals (ast-lambda-formals expr))
         (body (ast-lambda-body expr))
         (loc (get-location expr))
         (env-var (at loc (make-gensym-node 'env))))
    (replace expr
             (make-primop-app-node
              '&make-closure
              (list env
                    (at loc
                        (make-lambda-node
                         (cons env-var formals)
                         (substitute-symbols
                          (make-subs env-var)
                          (convert-closures body globals)))))))))

(define (make-nil loc)
  (at loc
      (generated
       (make-const-node
        (at loc
            (generated
             (make-list-node '())))))))

(define (make-env loc free closures)
  (case (length free)
    ((0)
     (make-nil loc))
    ((1)
     (pick-matching-var loc (car free) closures))
    ((2)
     (at loc
         (make-primop-app-node
          'cons
          (map (lambda (var)
                 (pick-matching-var loc var closures))
               free))))
    (else
     (at loc
         (make-primop-app-node
          '&make-env
          (map (lambda (var)
                 (pick-matching-var loc var closures))
               free))))))

(define (pick-matching-var loc name vars)
  (foldl (lambda (v acc)
           (if (equal? (ast-symbol-value v) name)
               v
               acc))
         (at loc
             (generated
              (make-symbol-node name)))
         vars))

(define (make-env-subs env free)
  (case (length free)
    ((1)
     (list (cons (car free)
                 (flip replace env))))
    ((2)
     (map (lambda (var accessor)
            (cons var
                  (lambda (expr)
                    (replace expr
                             (make-primop-app-node
                              accessor
                              (list env))))))
          free
          (list 'car 'cdr)))
    (else
     (map (lambda (var)
            (cons var
                  (lambda (expr)
                    (replace expr
                             (make-primop-app-node
                              '&env-ref
                              (list env
                                    (at (get-location expr)
                                        (make-const-node
                                         (at (get-location expr)
                                             (generated
                                              (make-number-node (offset var free))))))))))))
          free))))

(define (make-env-setters env env-var free bound closures)
  (case (length free)
    ((0)
     ;; Nothing to do.
     '())
    ((1 2)
     ;; NOTE This is a fix, therfore at least one of these values is going to be a bound closure,
     ;; NOTE so we can assume that each closure needs update in this case.
     (map (lambda (var)
            (at (get-location env)
                (make-primop-app-node
                 '&set-closure-env!
                 (list var env))))
          closures))
    (else
     ;; NOTE Otherwise we just update the env.
     (let ((args (ast-primop-app-args env)))
       (filter (compose not empty?)
               (map (lambda (arg i)
                      (if (set-member? bound (ast-symbol-value arg))
                          (at (get-location arg)
                              (make-primop-app-node
                               '&set-env!
                               (list env-var
                                     (at (get-location arg)
                                         (make-const-node
                                          (at (get-location arg)
                                              (generated
                                               (make-number-node i)))))
                                     arg)))
                          '()))
                    args
                    (iota 0 (- (length free) 1) 1)))))))
