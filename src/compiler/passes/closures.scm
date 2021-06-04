;; Closure conversion.
;; Assumes macro-expanded code.

(require "../utils/utils.rkt")
(require "../utils/set.rkt")
(require "../utils/gensym.rkt")

(require "../env.rkt")
(load-once "compiler/pass.scm")
(require "../ast.rkt")
(load-once "compiler/substitute.scm")

(define closure-convert
  (pass (schema "closure-convert"
                'globals a-set?
                'ast (ast-subset? '(const symbol
                                    if do let fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (flip convert-closures (env-get env 'globals))))))

(define (make-global-definitions-list)
  (apply set
         '(nil car cadr cdr cddr list cons append concat
           equal? nil? empty? true false not
           * + - / = < <= > >=
           quotient remainder modulo random zero?
           ref deref assign!
           call/current-continuation call/reset call/shift call/handler raise
           sleep self send recv spawn task-info monitor
           assert! signal! retract! select notify-whenever
           display newline debug)))

(define (convert-closures expr globals)
  (match-ast expr
   ((app op args ...)
    (replace expr
             (make-ast-primop-app (ast-node-location op)
                  '&apply
                  (map (flip convert-closures globals)
                       (cons op args)))))
   ((lambda formals body)
    (let ((free (set->list
                 (set-difference (ast-node-free-vars expr)
                                 globals))))
      (recreate-closure expr
                        ;; NOTE This is an anonymous function, so we can't really reuse it's names location for the environment.
                        (make-env (ast-node-location expr) free '())
                        (flip make-env-subs free)
                        globals)))
   ((fix bindings body)
    (let* ((loc (ast-node-location expr))
           (env-var (make-ast-gensym loc 'env))
           (free (set->list
                  (set-difference (set-sum (map ast-node-free-vars bindings))
                                  globals)))
           (env-subs (flip make-env-subs free))
           (closures (map (lambda (b)
                            (set-ast-binding-val b
                                                 (recreate-closure (ast-binding-val b)
                                                                   env-var
                                                                   env-subs
                                                                   globals)))
                          bindings))
           (closure-vars (map ast-binding-var closures))
           (bound (ast-node-bound-vars expr))
           (full-env (make-env loc free closure-vars))
           ;; NOTE So that we don't reference undefined (yet) variables.
           (actual-env (substitute-symbols
                        (make-subs
                         (map (flip cons (compose make-nil ast-node-location))
                              (set->list bound)))
                        full-env))
           (env-binding (make-ast-binding loc env-var actual-env))
           (setters (make-env-setters full-env env-var free bound closure-vars))
           (converted-body (convert-closures body globals)))
      (generated
       (make-ast-let loc
                     (list env-binding)
                     (generated
                      (make-ast-let loc
                                    closures
                                    (generated
                                     (make-ast-do loc
                                                  (append setters
                                                          (list converted-body))))))))))
   (else (walk-ast (flip convert-closures globals) expr))))

(define (recreate-closure expr env make-subs globals)
  (let* ((formals (ast-lambda-formals expr))
         (body (ast-lambda-body expr))
         (loc (ast-node-location expr))
         (env-var (make-ast-gensym loc 'env)))
    (replace expr
             (make-ast-primop-app loc
                                  '&make-closure
                                  (list env
                                        (make-ast-lambda loc
                                                         (cons env-var formals)
                                                         (substitute-symbols
                                                          (make-subs env-var)
                                                          (convert-closures body globals))))))))

(define (make-nil loc)
  (make-ast-const loc
                  (generated
                   (make-ast-list loc '()))))

(define (make-env loc free closures)
  (case (length free)
    ((0)
     (make-nil loc))
    ((1)
     (pick-matching-var loc (car free) closures))
    ((2)
     (make-ast-primop-app loc
                          'cons
                          (map (lambda (var)
                                 (pick-matching-var loc var closures))
                               free)))
    (else
     (make-ast-primop-app loc
                          '&make-env
                          (map (lambda (var)
                                 (pick-matching-var loc var closures))
                               free)))))

(define (pick-matching-var loc name vars)
  (foldl (lambda (v acc)
           (if (equal? (ast-symbol-value v) name)
               v
               acc))
         (generated
          (make-ast-symbol loc name))
         vars))

(define (make-env-subs env free)
  (make-subs
   (case (length free)
     ((1)
      (list (cons (car free)
                  (flip replace env))))
     ((2)
      (map (lambda (var accessor)
             (cons var
                   (lambda (expr)
                     (make-ast-primop-app (ast-node-location expr)
                                          accessor
                                          (list env)))))
           free
           (list 'car 'cdr)))
     (else
      (map (lambda (var)
             (cons var
                   (lambda (expr)
                     (let ((loc (ast-node-location expr)))
                       (replace expr
                                (make-ast-primop-app loc
                                                     '&env-ref
                                                     (list env
                                                           (make-ast-const loc
                                                                           (generated
                                                                            (make-ast-number loc
                                                                                             (offset var free)))))))))))
           free)))))

(define (make-env-setters env env-var free bound closures)
  (case (length free)
    ((0)
     ;; Nothing to do.
     '())
    ((1 2)
     ;; NOTE This is a fix, therfore at least one of these values is going to be a bound closure,
     ;; NOTE so we can assume that each closure needs update in this case.
     (map (lambda (var)
            (make-ast-primop-app (ast-node-location env)
                                 '&set-closure-env!
                                 (list var env)))
          closures))
    (else
     ;; NOTE Otherwise we just update the env.
     (let ((args (ast-primop-app-args env)))
       (filter (compose not empty?)
               (map (lambda (arg i)
                      (if (set-member? bound (ast-symbol-value arg))
                          (make-ast-primop-app (ast-node-location arg)
                                               '&set-env!
                                               (list env-var
                                                     (make-ast-const (ast-node-location arg)
                                                                     (generated
                                                                      (make-ast-number (ast-node-location arg) i)))
                                                     arg))
                          '()))
                    args
                    (iota 0 (- (length free) 1) 1)))))))
