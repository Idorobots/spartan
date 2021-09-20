#lang racket

;; Value hoisting into the global scope.

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require "../utils/gensym.rkt")

(provide globalize
         ;; FIXME For test access.
         hoist-values)

(define globalize
  (pass (schema "globalize"
                'ast (ast-subset? '(const symbol if do let binding lambda primop-app)))
        (lambda (env)
          (let ((result (hoist-values (env-get env 'ast))))
            (env-set env
                     'ast '()
                     'data (car result)
                     'init (cadr result))))))

(define (hoist-values expr)
  (let* ((hoisted '())
         (push! (lambda (expr)
                  (let* ((name (gensym 'global)))
                    (set! hoisted (cons (cons name expr)
                                        hoisted))
                    (make-ast-symbol (ast-node-location expr)
                                     name))))
         (init (map-ast
                (lambda (expr)
                  (match-ast
                   expr
                   ;; NOTE Only complex const values that need construction are hoisted.
                   ((const (list))
                    expr)
                   ((const (list values ...))
                    (push! expr))
                   ((const (string value))
                    (push! expr))
                   ((const (symbol value))
                    (push! expr))
                   ;; Closures that don't capture any free variables can be hoisted as well.
                   ((primop-app '&make-closure (const (list)) _)
                    (push! expr))
                   ;; All functions should now use their closure for free variables.
                   ((lambda _ _)
                    (push! expr))
                   (else
                    expr)))
                expr)))
    (list hoisted
          init)))
