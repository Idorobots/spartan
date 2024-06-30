#lang racket

;; Continuation Passing Converter
;; Assumes macro- & letrec-expanded as well as alpha-converted code.
;; This phase contorts the code so much that it invalidates free-vars & bindings annotations.

(require "../utils/gensym.rkt")
(require "../utils/utils.rkt")

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require "../errors.rkt")

(provide continuation-passing-convert
         make-identity-continuation
         ;; FIXME For test access.
         cpc)

(define continuation-passing-convert
  (pass (schema "continuation-passing-convert"
                'ast (ast-subset? '(const symbol if do let fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (lambda (expr)
                                 (cpc expr #f (make-identity-continuation)))))))

(define (make-identity-continuation)
  id)

(define (cpc expr kont-name kont)
  (match-ast expr
    ((const v)
     (kont expr))

    ((symbol s)
     (kont expr))

    ((if c t e)
     (let* ((loc (ast-node-location expr))
            (ct (make-ast-gensym loc 'cont))
            (value (make-ast-gensym loc 'value))
            (rest (lambda (v)
                    (make-ast-yield loc ct v))))
       (cpc c
            kont-name
            (lambda (condition)
              (make-ast-let-1 loc
                              ct
                              (make-ast-cont loc value (kont value))
                              (replace expr
                                       (make-ast-if loc
                                                    condition
                                                    (cpc t kont-name rest)
                                                    (cpc e kont-name rest))))))))

    ((do exprs ...)
     (cpc-sequence exprs
                   kont-name
                   (lambda (exprs)
                     (kont (set-ast-do-exprs expr exprs)))))

    ((let bindings body)
     (cpc-sequence (map ast-binding-val bindings)
                   kont-name
                   (lambda (vals)
                     (-> expr
                         (set-ast-let-bindings (map (lambda (b val)
                                                      (set-ast-binding-val b val))
                                                    bindings
                                                    vals))
                         (set-ast-let-body (cpc body kont-name kont))))))

    ((fix bindings body)
     (replace expr
              (make-ast-fix (ast-node-location expr)
                            (map (lambda (b)
                                   ;; NOTE These are guaranteed to be pure lambda expressions,
                                   ;; NOTE so no need to propagate the current continuation there.
                                   (set-ast-binding-val b (cpc (ast-binding-val b)
                                                               kont-name
                                                               (make-identity-continuation))))
                                 bindings)
                            (cpc body kont-name kont))))

    ((lambda formals body)
     (let* ((loc (ast-node-location expr))
            (ct (make-ast-gensym loc 'cont)))
       (kont (-> expr
                 (set-ast-lambda-formals (append formals
                                                 (list ct)))
                 (set-ast-lambda-body (cpc body
                                           ct
                                           (lambda (s)
                                             (make-ast-yield loc ct s))))))))

    ;; FIXME A hacky way to get the lexical current-continuation in the core runtime functions.
    ((primop-app '&current-continuation)
     (if (ast-node? kont-name)
         ;; NOTE We still expect the location to be the primop-app one.
         (kont (replace expr kont-name))
         (compiler-bug "Invalid `&current-continuation` application:" expr)))

    ((primop-app op args ...)
     (let* ((loc (ast-node-location expr))
            (value (make-ast-gensym loc 'value)))
       (cpc-sequence args
                     kont-name
                     (lambda (args)
                       ;; NOTE The let could be ommited for non-side-effecting primops, but this way presents more
                       ;; NOTE opportunities for common subexpression elimination.
                       (make-ast-let-1 loc
                                       value
                                       (set-ast-primop-app-args expr args)
                                       (kont value))))))

    ((app op args ...)
     (let* ((loc (ast-node-location expr))
            (value (make-ast-gensym loc 'value))
            (cont (make-ast-cont loc value (kont value))))
       (cpc op
            kont-name
            (lambda (op)
              (cpc-sequence args
                            kont-name
                            (lambda (args)
                              (-> expr
                                  (set-ast-app-op op)
                                  (set-ast-app-args (append args
                                                            (list cont))))))))))

    (else (compiler-bug "Unexpected expression passed to cpc: " expr))))

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

(define (cpc-sequence exprs kont-name kont)
  (if (> (length exprs) 0)
      (cpc (car exprs)
           kont-name
           (lambda (first)
             (cpc-sequence (cdr exprs)
                           kont-name
                           (lambda (rest)
                             (kont (cons first rest))))))
      (kont '())))
