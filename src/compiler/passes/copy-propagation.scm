;; Copy propagation.

(load "compiler/utils/utils.scm")

(load "compiler/propagate.scm")
(load "compiler/env.scm")
(load "compiler/pass.scm")
(load "compiler/ast.scm")

(define propagate-copies
  (pass (schema "propagate-copies"
                'ast (ast-subset? '(const symbol
                                    if do let letrec binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (partial copy-propagation '())))))

(define (copy-propagation subs expr)
  (propagate symbol-binding?
             make-copy-sub
             (lambda (subs expr kont)
               (ast-case expr
                ((symbol _)
                 (let ((s (assoc (ast-symbol-value expr) subs)))
                   (if s
                       (cdr s) ;; NOTE Completely replaces the expr, together with its location.
                       expr)))
                (else
                 (kont expr))))
             subs
             expr))

(define (symbol-binding? binding)
  (symbol-node? (ast-binding-val binding)))

(define (make-copy-sub binding)
  (cons (ast-symbol-value (ast-binding-var binding))
        (ast-binding-val binding)))