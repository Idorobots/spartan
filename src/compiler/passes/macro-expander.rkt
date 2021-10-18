#lang racket

;; Pseudo macro-expander for convenience

(require "../utils/utils.rkt")

(require (only-in "elaboration.rkt"
                  valid-bindings valid-symbol))

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require "../errors.rkt")

(provide macro-expand make-static-environment
         ;; FIXME For test access.
         expand)

(define macro-expand
  (pass (schema "macro-expand"
                'static-env non-empty-hash?
                'errors a-list?
                'ast (ast-subset? '(quote quasiquote unquote unquote-splicing
                                    number symbol string list
                                    primop-app body <error>)))
        (lambda (env)
          (let ((result (collect-errors (env-get env 'errors)
                                        (lambda ()
                                          (expand (env-get env 'static-env)
                                                  (env-get env 'ast))))))
            (env-set env
                     'ast (car result)
                     'errors (cadr result))))))

(define (expand env expr)
  (match-ast expr

   ((string _)
    expr)

   ((number _)
    expr)

   ((ast-quote _)
    expr)

   ((list (symbol 'quote) rest ...)
    expr)

   ;; FIXME This should be an expander producing plain lists.
   ((ast-quasiquote _)
    expr)

   ;; FIXME This should be an expander producing plain lists.
   ((list (symbol 'quasiquote) rest ...)
    expr)

   ;; Keyword macro expansion
   ((symbol key)
    #:when (and (environment-contains? env key)
                (expander? (environment-ref env key)))
    ;; FIXME This should expand the keyword macro instead.
    expr)

   ;; Renamed symbol
   ((symbol key)
    #:when (environment-contains? env key)
    (environment-ref env key))

    ;; Just a free var in the current static env. FIXME This likely should be a compilation error.
   ((symbol _)
    expr)

   ;; Regular macro expansion
   ((list (symbol op) rest ...)
    #:when (and (environment-contains? env op)
                (expander? (environment-ref env op)))
    (expand env
            (expand-expander op env expr)))

   (else
    (traverse-ast expand env expr))))

(define (expand-expander key env expr)
  (let ((expander (environment-ref env key)))
    ((expander-transformer expander)
     expr
     env
     (expander-environment expander))))

;; Static environment

(struct expander
  (environment
   transformer) ;; NOTE Should take 3 parameters - expr, use-env and a def-env.
  #:transparent
  #:constructor-name make-expander)

(define (make-builtin-expander transformer)
  (make-expander #f transformer))

(define (make-static-environment)
  (hasheq 'when (make-builtin-expander when-macro)
          'unless (make-builtin-expander unless-macro)
          'cond (make-builtin-expander cond-macro)
          'and (make-builtin-expander and-macro)
          'or (make-builtin-expander or-macro)
          'let* (make-builtin-expander let*-macro)
          'letcc (make-builtin-expander letcc-macro)
          'handle (make-builtin-expander handle-macro)
          'shift (make-builtin-expander shift-macro)
          'reset (make-builtin-expander reset-macro)
          'structure (make-builtin-expander structure-macro)
          'module (make-builtin-expander module-macro)))

(define (environment-contains? env key)
  (hash-has-key? env key))

(define (environment-ref env key)
  (hash-ref env key))

;; Built in expander implementations

(define (when-macro expr use-env def-env)
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

(define (unless-macro expr use-env def-env)
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

(define (cond-macro expr use-env def-env)
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

(define (and-macro expr use-env def-env)
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

(define (or-macro expr use-env def-env)
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

(define (let*-macro expr use-env def-env)
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

(define (letcc-macro expr use-env def-env)
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

(define (shift-macro expr use-env def-env)
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

(define (reset-macro expr use-env def-env)
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

(define (handle-macro expr use-env def-env)
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

(define (structure-macro expr use-env def-env)
  (match-ast expr
   ((list (symbol 'structure) defs ...)
    (let ((names (map extract-definition-name defs)))
      (let ((loc (ast-node-location expr)))
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
                       "Bad `structure` syntax"))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `structure` syntax, expected a module specification followed by a body:")))))

(define (extract-definition-name expr)
  (match-ast expr
   ((list (symbol 'define) (list name rest ...) body ...)
    name)
   ((list (symbol 'define) name value)
    name)
   (else
    (raise-compilation-error
     expr
     "Bad `structure` syntax, expected a definition:"))))

(define (module-macro expr use-env def-env)
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
