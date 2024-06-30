#lang racket

;; Built-in macros expander implementations

(require "../utils/utils.rkt")
(require "../ast.rkt")
(require "../errors.rkt")

(require (only-in "syntax-forms.rkt"
                  valid-bindings valid-symbol))

(provide (all-defined-out))

(define (when-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'when) cond first rest ...)
    (let ((loc (ast-node-location expr)))
      (replace expr
               (make-ast-if loc
                            cond
                             (make-ast-body loc (cons first rest) "Bad `when` body syntax")
                             (make-ast-symbol loc 'nil)))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `when` syntax, expected a condition and a body to follow:")))))

(define (unless-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'unless) cond first rest ...)
    (let ((loc (ast-node-location expr)))
      (replace expr
               (make-ast-if loc
                            cond
                             (make-ast-symbol loc 'nil)
                             (make-ast-body loc (cons first rest) "Bad `unless` body syntax")))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `unless` syntax, expected a condition and a body to follow:")))))

(define (cond-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'cond) (list (symbol 'else) else-first else-rest ...))
    (make-ast-body (ast-node-location expr)
                   (cons else-first else-rest)
                   "Bad `cond` else clause"))
   ((list (symbol 'cond) (list cond branch-first branch-rest ...) rest ...)
    (let ((loc (ast-node-location expr)))
      (replace expr
               (make-ast-if loc
                            cond
                             (make-ast-body loc (cons branch-first branch-rest) "Bad `cond` clause")
                             (make-ast-list loc
                                            (cons (ast-list-car expr)
                                                  rest))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `cond` syntax, expected a list of conditional branches with a final else branch to follow:")))))

(define (and-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'and) last)
    last)
   ((list and first rest ...)
    (let ((loc (ast-node-location expr)))
      (replace expr
             (make-ast-if loc
                          first
                          (make-ast-list loc (cons and rest))
                          (make-ast-symbol loc 'false)))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `and` syntax, expected a list of expressions to follow:")))))

(define (or-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'or) last)
    last)
   ((list or first rest ...)
    (let ((loc (ast-node-location expr)))
      (replace expr
             (make-ast-if loc
                          first
                          (make-ast-symbol loc 'true)
                          (make-ast-list loc (cons or rest))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `or` syntax, expected a list of expressions to follow:")))))

(define (let*-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'let*) (list) first rest ...)
    (make-ast-body (ast-node-location expr)
                   (cons first rest)
                   "Bad `let*` body syntax"))
   ((list (symbol 'let*) (list first-binding rest-bindings ...) body ...)
    (let ((loc (ast-node-location expr)))
      (replace expr
               (make-ast-let loc
                             (valid-bindings (list first-binding) "Bad `let*` bindings syntax")
                             (make-ast-list loc
                                            (list* (make-ast-symbol loc 'let*)
                                                   (make-ast-list loc rest-bindings) ;; FIXME Could use a better location.
                                                 body))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `let*` syntax, expected a list of bindings and a body to follow:")))))

(define (letcc-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'letcc) name first rest ...)
    (let ((loc (ast-node-location expr)))
      (replace expr
               (make-ast-app loc
                             (make-ast-symbol loc 'call/current-continuation)
                             (list (make-ast-lambda loc
                                                    (list (valid-symbol name "Bad `letcc` syntax"))
                                                    (make-ast-body loc (cons first rest) "Bad `letcc` body syntax")))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `letcc` syntax, expected an identifier and a body to follow:")))))

(define (shift-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'shift) name first rest ...)
    (let ((loc (ast-node-location expr)))
      (replace expr
               (make-ast-app loc
                             (make-ast-symbol loc 'call/shift)
                             (list (make-ast-lambda loc
                                                    (list (valid-symbol name "Bad `shift` syntax"))
                                                    (make-ast-body loc (cons first rest) "Bad `shift` body syntax")))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `shift` syntax, expected an identifier and a body to follow:")))))

(define (reset-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'reset) first rest ...)
    (let ((loc (ast-node-location expr)))
      (replace expr
               (make-ast-app loc
                             (make-ast-symbol loc 'call/reset)
                             (list (make-ast-lambda loc
                                                    '()
                                                    (make-ast-body loc (cons first rest) "Bad `reset` body syntax")))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `reset` syntax, expected exactly one expression to follow:")))))

(define (handle-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'handle) subexpr handler)
    (let ((loc (ast-node-location expr)))
      (replace expr
               (make-ast-app loc
                             (make-ast-symbol loc 'call/handler)
                             (list handler
                                   (make-ast-lambda loc '() subexpr))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `handle` syntax, expected exactly two expressions to follow:")))))

(define (structure-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'structure) defs ...)
    (let* ((filtered (filter-defs defs))
           (names (map extract-definition-name filtered))
           (loc (ast-node-location expr)))
      (make-ast-body loc
                     (append defs
                             (list (make-ast-primop-app loc
                                                        '&make-structure
                                                        (map (lambda (n)
                                                               (make-ast-primop-app (ast-node-location n)
                                                                                    '&structure-binding
                                                                                    (list (make-ast-quote (ast-node-location n) n)
                                                                                          n)))
                                                             names))))
                     "Bad `structure` syntax")))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `structure` syntax, expected a module specification followed by a body:")))))

(define (filter-defs defs)
  (if (empty? defs)
      '()
      (match-ast (car defs)
       ((list (symbol def) rest ...)
        ;; FIXME Structure is a macro, so the primitive declarations are not expanded yet when this runs.
        #:when (member def '(define declare-primitive))
        (cons (car defs)
              (filter-defs (cdr defs))))
       (else
        (filter-defs (cdr defs))))))

(define (extract-definition-name expr)
  (match-ast expr
   ((list (symbol 'define) (list name rest ...) body ...)
    name)
   ((list (symbol 'define) name value ...)
    name)
   ;; FIXME Structure is a macro, so the primitive declarations are not expanded yet when this runs.
   ((list (symbol 'declare-primitive) (list name rest ...) body ...)
    name)
   ((list (symbol 'declare-primitive) name body ...)
    name)
   (else
    (raise-compilation-error
     expr
     "Bad `structure` syntax, expected a definition:"))))

(define (module-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'module) (list name deps ...) body ...)
    (let ((loc (ast-node-location expr)))
      (replace expr
               (set-ast-node-context
                (make-ast-def loc
                              name
                              (make-ast-lambda loc
                                               (map (flip valid-symbol "Bad `module` dependencies syntax") deps)
                                               (make-ast-list loc
                                                              (cons (make-ast-symbol loc 'structure)
                                                                    body))))
                "Bad `module` syntax"))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `module` syntax, expected a module specification followed by a body:")))))
