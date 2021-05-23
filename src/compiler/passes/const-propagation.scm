;; Constant propagation.

(load-once "compiler/utils/utils.scm")

(load-once "compiler/substitute.scm")
(load-once "compiler/propagate.scm")
(load-once "compiler/env.scm")
(load-once "compiler/pass.scm")
(load-once "compiler/ast.scm")

(define propagate-constants
  (pass (schema "propagate-constants"
                'ast (ast-subset? '(const symbol if do let letrec fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (partial constant-propagation (make-subs '()))))))

(define (constant-propagation subs expr)
  (propagate const-binding?
             make-const-subs
             (lambda (subs expr kont)
               (ast-case expr
                ((symbol _)
                 ;; NOTE Completely replaces the expr, together with its location.
                 (replace-sub subs
                              (ast-symbol-value expr)
                              (constantly expr)))
                (else
                 (kont expr))))
             subs
             expr))

(define (const-binding? binding)
  (const-node? (ast-binding-val binding)))

(define (make-const-subs bindings subs)
  (extend-subs (map (lambda (binding)
                      (cons (ast-symbol-value (ast-binding-var binding))
                            (ast-binding-val binding)))
                    bindings)
               subs))
