;; Continuation Passing Converter
;; Assumes macro- & letrec-expanded as well as alpha-converted code.
;; This phase contorts the code so much that it invalidates free-vars & bindings annotations.

(require "../utils/gensym.rkt")
(require "../utils/utils.rkt")

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require "../errors.rkt")

(define continuation-passing-convert
  (pass (schema "continuation-passing-convert"
                'ast (ast-subset? '(const symbol
                                          if do let fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (flip cpc (make-identity-continuation))))))

(define (make-identity-continuation)
  id)

(define (cpc expr kont)
  (case (ast-node-type expr)
    ((symbol const) (kont expr))
    ((if) (cpc-if expr kont))
    ((do) (cpc-do expr kont))
    ((let) (cpc-let expr kont))
    ((fix) (cpc-fix expr kont))
    ((lambda) (cpc-lambda expr kont))
    ((primop-app) (cpc-primop-app expr kont))
    ((app) (cpc-app expr kont))
    (else (compiler-bug "Unexpected expression passed to cpc:" expr))))

(define (cpc-if expr kont)
  (let* ((loc (ast-node-location expr))
         (ct (make-ast-gensym loc 'cont))
         (value (make-ast-gensym loc 'value))
         (rest (lambda (v)
                 (make-ast-yield loc ct v))))
    (cpc (ast-if-condition expr)
         (lambda (condition)
           (make-ast-let-1 loc
                           ct
                           (make-ast-cont loc value (kont value))
                           (replace expr
                                    (make-ast-if loc
                                                 condition
                                                 (cpc (ast-if-then expr) rest)
                                                 (cpc (ast-if-else expr) rest))))))))

(define (make-ast-let-1 loc var val body)
  (generated
   (make-ast-let loc
                 (list (generated
                        (make-ast-binding (ast-node-location var) var val)))
                 body)))

(define (make-ast-cont loc arg body)
  (generated
   (make-ast-lambda loc (list arg) body)))

(define (make-ast-yield loc cont hole)
  (make-ast-primop-app loc
                       '&yield-cont
                       (list cont hole)))

(define (cpc-do expr kont)
  (cpc-sequence (ast-do-exprs expr)
                (lambda (exprs)
                  (kont (set-ast-do-exprs expr exprs)))))

(define (cpc-sequence exprs kont)
  (if (> (length exprs) 0)
      (cpc (car exprs)
           (lambda (first)
             (cpc-sequence (cdr exprs)
                           (lambda (rest)
                             (kont (cons first rest))))))
      (kont '())))

(define (cpc-lambda expr kont)
  (let* ((loc (ast-node-location expr))
         (ct (make-ast-gensym loc 'cont)))
    (kont (-> expr
              (set-ast-lambda-formals (append (ast-lambda-formals expr)
                                              (list ct)))
              (set-ast-lambda-body (cpc (ast-lambda-body expr)
                                        (lambda (s)
                                          (make-ast-yield loc ct s))))))))

(define (cpc-primop-app expr kont)
  (let* ((args (ast-primop-app-args expr))
         (loc (ast-node-location expr))
         (value (make-ast-gensym loc 'value)))
    (cpc-sequence args
                  (lambda (args)
                    ;; NOTE The let could be ommited for non-side-effecting primops, but this way presents more
                    ;; NOTE opportunities for common subexpression elimination.
                    (make-ast-let-1 loc
                                    value
                                    (set-ast-primop-app-args expr args)
                                    (kont value))))))

(define (cpc-app expr kont)
  (let* ((loc (ast-node-location expr))
         (value (make-ast-gensym loc 'value))
         (cont (make-ast-cont loc value (kont value)))
         (args (ast-app-args expr)))
    (cpc (ast-app-op expr)
         (lambda (op)
           (cpc-sequence args
                         (lambda (args)
                           (-> expr
                               (set-ast-app-op op)
                               (set-ast-app-args (append args
                                                         (list cont))))))))))

(define (cpc-fix expr kont)
  (replace expr
           (make-ast-fix (ast-node-location expr)
                         (map (lambda (b)
                                ;; NOTE These are guaranteed to be pure lambda expressions,
                                ;; NOTE so no need to propagate the current continuation there.
                                (set-ast-binding-val b (cpc (ast-binding-val b)
                                                            (make-identity-continuation))))
                              (ast-fix-bindings expr))
                         (cpc (ast-fix-body expr)
                              kont))))

(define (cpc-let expr kont)
  (let ((bindings (ast-let-bindings expr)))
    (cpc-sequence (map ast-binding-val bindings)
                  (lambda (vals)
                    (-> expr
                        (set-ast-let-bindings (map (lambda (b val)
                                                     (set-ast-binding-val b val))
                                                   bindings
                                                   vals))
                        (set-ast-let-body (cpc (ast-let-body expr)
                                               kont)))))))
