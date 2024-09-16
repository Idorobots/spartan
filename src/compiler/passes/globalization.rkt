#lang racket

;; Value hoisting into the global scope.

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require "../utils/gensym.rkt")
(require "../utils/utils.rkt")
(require "../substitute.rkt")

(provide globalize
         ;; FIXME For test access.
         hoist-values
         normalize-lambda)

(define globalize
  (pass (schema "globalize"
                'ast (ast-subset? '(const symbol if do let binding lambda primop-app)))
        (lambda (env)
          (let ((result (hoist-values (env-get env 'ast))))
            (-> env
                (env-remove 'ast)
                (env-set 'data (car result)
                         'init (cadr result)))))
        (schema "globalize output"
                'data (list-of? (a-pair? a-symbol?
                                         (ast-subset? '(const symbol if do let binding lambda primop-app))))
                'init (ast-subset? '(const symbol if do let binding primop-app)))))

(define (hoist-values expr)
  (let* ((hoisted '())
         (push! (lambda (name-hint expr)
                  (let* ((eqv (find (lambda (e)
                                       (ast-eqv? (cdr e) expr))
                                    hoisted))
                         (name (if eqv
                                   (car eqv)
                                   (let ((n (gensym name-hint)))
                                     (set! hoisted (cons (cons n expr)
                                                         hoisted))
                                     n))))
                    (generated
                     (make-ast-symbol (ast-node-location expr)
                                      name)))))
         (init (map-ast
                (lambda (expr)
                  (match-ast expr
                   ;; NOTE Only complex const values that need construction are hoisted.
                   ((const (list))
                    expr)
                   ((const (list values ...))
                    (push! 'list expr))
                   ((const (string value))
                    (push! 'string expr))
                   ((const (symbol value))
                    (push! value expr))
                   ;; Closures that don't capture any free variables can be hoisted as well.
                   ((primop-app '&make-closure (const (list)) _)
                    (push! 'closure expr))
                   ;; All functions should now use their closure for free variables.
                   ((lambda _ _)
                    (push! 'function
                           (normalize-lambda expr)))
                   (else
                    expr)))
                expr)))
    (list (reverse hoisted)
          init)))

(define (normalize-formals formals)
  (map (lambda (f i)
         ;; FIXME This should really check the body for potential name collisions.
         (set-ast-symbol-value f (string->symbol (format "arg~s" i))))
       formals
       (iota 0 (- (length formals) 1) 1)))

(define (normalize-lambda expr)
  (let* ((formals (ast-lambda-formals expr))
         (normalized (normalize-formals formals))
         (subs (map (lambda (f n)
                      (cons (ast-symbol-value f)
                            (lambda (_)
                              n)))
                    formals
                    normalized)))
    (-> expr
        (set-ast-lambda-formals normalized)
        (set-ast-lambda-body (substitute-symbols (make-subs subs)
                                                 (ast-lambda-body expr))))))
