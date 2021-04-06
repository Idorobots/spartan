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
  (ast-case expr
   ((app ,op . ,args)
    (replace expr
             (make-primop-app (get-location op)
                              '&apply
                              (map (flip convert-closures globals)
                                   (cons op args)))))
   ((lambda ,formals ,body)
    (let ((free (set-difference (get-free-vars expr)
                                globals)))
      (recreate-closure expr
                        (make-env (get-location expr) free)
                        (flip make-env-subs free)
                        globals)))
   ((fix ,bindings ,body)
    (let* ((loc (get-location expr))
           (env-var (at loc (make-gensym-node 'env)))
           (free (set-difference (set-sum (map get-free-vars bindings))
                                 globals))
           (bound (get-bound-vars expr))
           (full-env (make-env loc free))
           (actual-env (substitute (map (flip cons (compose make-nil get-location)) bound)
                                   full-env))
           (env-binding (at loc (make-binding-node env-var actual-env)))
           (env-subs (flip make-env-subs free))
           (closures (map (lambda (b)
                            (ast-update b 'val
                                        (lambda (fun)
                                          (recreate-closure fun
                                                            env-var
                                                            env-subs
                                                            globals))))
                          bindings))
           (setters (make-env-setters full-env env-var free bound (map ast-binding-var closures)))
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
             (make-primop-app loc
                              '&make-closure
                              (list env
                                    (at loc
                                        (make-lambda-node
                                         (cons env-var formals)
                                         (substitute (make-subs env-var)
                                                     (convert-closures body globals)))))))))

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

(define (make-env-setters env env-var free bound closures)
  (case (length free)
    ((0)
     ;; Nothing to do.
     '())
    ((1 2)
     ;; NOTE This is a fix, therfore at least one of these values is going to be a bound closure,
     ;; NOTE so we can assume that each closure needs update in this case.
     (map (lambda (var)
            (make-primop-app (get-location env)
                             '&set-closure-env!
                             (list var env)))
          closures))
    (else
     ;; NOTE Otherwise we just update the env.
     (let ((args (ast-primop-app-args env)))
       (filter (compose not empty?)
             (map (lambda (arg i)
                    (if (set-member? bound (ast-symbol-value arg))
                        (make-primop-app (get-location arg)
                                         '&set-env!
                                         (list env-var
                                               (at (get-location arg)
                                                   (generated
                                                    (make-number-node i)))
                                               arg))
                        '()))
                  args
                  (iota 0 (- (length free) 1) 1)))))))
