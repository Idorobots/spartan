;; Continuation Passing Converter
;; Assumes macro- & letrec-expanded as well as alpha-converted code.
;; This phase contorts the code so much that it invalidates free-vars & bindings annotations.

(load-once "compiler/utils/gensym.scm")
(load-once "compiler/utils/utils.scm")

(load-once "compiler/env.scm")
(load-once "compiler/pass.scm")
(load-once "compiler/ast.scm")
(load-once "compiler/errors.scm")

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
         (ct (at loc (make-gensym-node 'cont)))
         (value (at loc (make-gensym-node 'value)))
         (rest (lambda (v)
                 (at loc
                     (make-yield-node ct v)))))
    (cpc (ast-if-condition expr)
         (lambda (condition)
           (at loc
               (make-let-1-node ct
                                (at loc
                                    (make-cont-node value (kont value)))
                                (replace expr
                                         (make-if-node condition
                                                       (cpc (ast-if-then expr) rest)
                                                       (cpc (ast-if-else expr) rest)))))))))

(define (make-let-1-node var val body)
  (generated
   (make-let-node (list (at (ast-node-location var)
                            (generated
                             (make-binding-node var val))))
                  body)))

(define (make-cont-node arg body)
  (generated
   (make-lambda-node (list arg) body)))

(define (make-yield-node cont hole)
  (make-primop-app-node '&yield-cont (list cont hole)))

(define (cpc-do expr kont)
  (cpc-sequence (ast-do-exprs expr)
                (lambda (exprs)
                  (kont (ast-update expr 'exprs (constantly exprs))))))

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
         (ct (at loc (make-gensym-node 'cont))))
    (kont (replace expr
                   (make-lambda-node (append (ast-lambda-formals expr)
                                             (list ct))
                                     (cpc (ast-lambda-body expr)
                                          (lambda (s)
                                            (at loc
                                                (make-yield-node ct s)))))))))

(define (cpc-primop-app expr kont)
  (let* ((args (ast-primop-app-args expr))
         (loc (ast-node-location expr))
         (value (at loc (make-gensym-node 'value))))
    (cpc-sequence args
                  (lambda (args)
                    ;; NOTE The let could be ommited for non-side-effecting primops, but this way presents more
                    ;; NOTE opportunities for common subexpression elimination.
                    (at loc
                        (make-let-1-node value (ast-update expr 'args (constantly args))
                                         (kont value)))))))

(define (cpc-app expr kont)
  (let* ((loc (ast-node-location expr))
         (value (at loc (make-gensym-node 'value)))
         (cont (at loc
                   (make-cont-node value (kont value))))
         (args (ast-app-args expr)))
    (cpc (ast-app-op expr)
         (lambda (op)
           (cpc-sequence args
                         (lambda (args)
                           (replace expr
                                    (make-app-node op
                                                   (append args
                                                           (list cont))))))))))

(define (cpc-fix expr kont)
  (replace expr
           (make-fix-node (map (lambda (b)
                                 ;; NOTE These are guaranteed to be pure lambda expressions,
                                 ;; NOTE so no need to propagate the current continuation there.
                                 (ast-update b 'val (flip cpc (make-identity-continuation))))
                               (ast-fix-bindings expr))
                          (cpc (ast-fix-body expr)
                               kont))))

(define (cpc-let expr kont)
  (let ((bindings (ast-let-bindings expr)))
    (cpc-sequence (map ast-binding-val (ast-let-bindings expr))
                  (lambda (vals)
                    (replace expr
                             (make-let-node (map (lambda (var val)
                                                   (make-binding-node var val))
                                                 (map ast-binding-var bindings)
                                                 vals)
                                            (cpc (ast-let-body expr)
                                                 kont)))))))
