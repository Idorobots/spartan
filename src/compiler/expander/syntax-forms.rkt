#lang racket

;; Built-in syntax forms expander implementations

(require "../utils/utils.rkt")
(require "../ast.rkt")
(require "../errors.rkt")

(provide (all-defined-out))

(define (if-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'if) condition then else)
    (replace expr
             (make-ast-if (ast-node-location expr)
                          condition
                           then
                           else)))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `if` syntax, expected exactly three expressions - condition, then and else branches - to follow:")))))

(define (do-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'do) first rest ...)
    ;; NOTE User-supplied do needs to be body-expanded as well.
    (make-ast-body (ast-node-location expr)
                   (cons first rest)
                   "Bad `do` syntax"))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `do` syntax, expected at least one expression to follow:")))))

(define (lambda-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'lambda) formals first rest ...)
    (replace expr
             (make-ast-lambda (ast-node-location expr)
                              (valid-formals formals "Bad `lambda` formal arguments syntax")
                              (make-ast-body (ast-node-location expr)
                                             (cons first rest)
                                             "Bad `lambda` body syntax"))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `lambda` syntax, expected a formal arguments specification followed by a body:")))))

(define +reserved-keywords+
  '(lambda let letrec if do true false))

(define (valid-formals args prefix)
  (if (is-type? args 'list)
      (legal-formals
       (unique-formals
        (map (lambda (s)
               (valid-symbol s prefix))
             (ast-list-values args))
        prefix)
       prefix)
      (list
       (raise-compilation-error
        args
        (format "~a, expected a list of identifiers:" prefix)))))

(define (legal-formals args prefix)
  (map (lambda (f)
         (if (and (ast-symbol? f)
                  (member (ast-symbol-value f) +reserved-keywords+))
             (raise-compilation-error
              f
              (format "~a, reserved keyword `~a` used as a formal argument:" prefix (ast-symbol-value f)))
             f))
       args))

(define (unique-formals args prefix)
  (let loop ((acc '())
             (seen '())
             (args args))
    (if (empty? args)
        (reverse acc)
        (let ((first (car args))
              (rest (cdr args)))
          (match-ast first

           ((symbol '_)
            (loop (cons first acc) seen rest))

           ((symbol value)
            (loop (cons (if (member value seen)
                            (raise-compilation-error
                             first
                             (format "~a, duplicate formal argument `~a`:" prefix value))
                            first)
                        acc)
                  (cons value seen)
                  rest))

           (else
            (loop (cons first acc) seen rest)))))))

(define (valid-symbol symbol prefix)
  (if (is-type? symbol 'symbol)
      symbol
      (raise-compilation-error
       symbol
       (format "~a, expected a symbol but got a ~a instead:" prefix (ast-node-type symbol)))))

(define (let-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'let) (list first-binding rest-bindings ...) first-body rest-body ...)
    (replace expr
             (make-ast-let (ast-node-location expr)
                           (valid-bindings (cons first-binding rest-bindings) "Bad `let` bindings syntax")
                           (make-ast-body (ast-node-location expr)
                                          (cons first-body rest-body)
                                          "Bad `let` body syntax"))))
   ((list (symbol 'let) (list) first rest ...)
    (make-ast-body (ast-node-location expr)
                   (cons first rest)
                   "Bad `let` body syntax"))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `let` syntax, expected a list of bindings followed by a body:")))))

(define (letrec-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'letrec) (list first-binding rest-bindings ...) first-body rest-body ...)
    (replace expr
             (make-ast-letrec (ast-node-location expr)
                              (valid-bindings (cons first-binding rest-bindings) "Bad `letrec` bindings syntax")
                              (make-ast-body (ast-node-location expr)
                                             (cons first-body rest-body)
                                             "Bad `letrec` body syntax"))))
   ((list (symbol 'letrec) (list) first rest ...)
    (make-ast-body (ast-node-location expr)
                   (cons first rest)
                   "Bad `letrec` body syntax"))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `letrec` syntax, expected a list of bindings followed by a body:")))))

(define (valid-bindings bindings prefix)
  (legal-bindings
   (unique-bindings
    (map (lambda (b)
           (valid-binding b prefix))
         bindings)
    prefix)
   prefix))

(define (legal-bindings bindings prefix)
  (map (lambda (b)
         (let ((var (ast-binding-var b)))
           (if (and (ast-symbol? var)
                    (member (ast-symbol-value var) +reserved-keywords+))
               (let ((e (raise-compilation-error
                         b
                         (format "~a, reserved keyword `~a` used as a binding identifier:" prefix (ast-symbol-value var)))))
                 (make-ast-binding (ast-node-location b) e e))
               b)))
       bindings))

(define (unique-bindings bindings prefix)
  (let loop ((acc '())
             (seen '())
             (bindings bindings))
    (if (empty? bindings)
        (reverse acc)
        (let ((first (car bindings))
              (rest (cdr bindings)))
          (match-ast first

           ((binding (symbol '_) _)
            (loop (cons first acc) seen rest))

           ((binding (symbol value) _)
            (loop (cons (if (member value seen)
                            (let ((e (raise-compilation-error
                                      first
                                      (format "~a, duplicate binding identifier `~a`:" prefix value))))
                              (make-ast-binding (ast-node-location first) e e))
                            first)
                        acc)
                  (cons value seen)
                  rest))

           (else
            (loop (cons first acc) seen rest)))))))

(define (valid-binding binding prefix)
  (replace binding
           (if (and (is-type? binding 'list)
                    (equal? (length (ast-list-values binding)) 2))
               (make-ast-binding (ast-node-location binding)
                                 (valid-symbol (ast-list-car binding) prefix)
                                 (ast-list-nth binding 1))
               (let ((e (raise-compilation-error
                         binding
                         (format "~a, expected a pair of an identifier and a value:" prefix))))
                 (make-ast-binding (ast-node-location binding) e e)))))

(define (quote-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'quote) value)
    (replace expr
             (make-ast-quote (ast-node-location expr)
                             value)))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `quote` syntax, expected exactly one expression to follow:")))))

(define (quasiquote-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'quasiquote) value)
    (replace expr
             (make-ast-quasiquote (ast-node-location expr)
                                  value)))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `quasiquote` syntax, expected exactly one expression to follow:")))))

(define (unquote-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'unquote) value)
    (replace expr
             (make-ast-unquote (ast-node-location expr)
                               value)))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `unquote` syntax, expected exactly one expression to follow:")))))

(define (unquote-splicing-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'unquote-splicing) value)
    (replace expr
             (make-ast-unquote-splicing (ast-node-location expr)
                                        value)))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `unquote-splicing` syntax, expected exactly one expression to follow:")))))

(define (def-expander expr use-env def-env)
  (match-ast expr
   ((list (symbol 'define) (list name formals ...) first rest ...)
    (let ((func-def (ast-list-nth expr 1))
          (loc (ast-node-location expr)))
      (replace expr
               (make-ast-def loc
                             (valid-symbol name "Bad `define` syntax")
                             (generated
                              (make-ast-lambda loc
                                               (valid-formals (generated
                                                               (make-ast-list (ast-node-location func-def)
                                                                              formals))
                                                              "Bad `define` function signature syntax")
                                               (make-ast-body loc
                                                              (cons first rest)
                                                              "Bad `define` function body syntax")))))))
   ((list (symbol 'define) name value)
    (replace expr
             (make-ast-def (ast-node-location expr)
                           (valid-symbol name "Bad `define` syntax")
                            value)))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `define` syntax, expected either an identifier and an expression or a function signature and a body to follow:")))))

(define (make-app-expander expand)
  (define (app-expander expr use-env def-env)
    (match-ast expr
     ((list op args ...)
      (replace expr
               (expand use-env
                       (make-ast-app (ast-node-location expr)
                                     op
                                     args))))
     (else
      (raise-compilation-error
       expr
       "Bad call syntax, expected at least one expression within the call:"))))
  app-expander)

(define (make-body-expander expand)
  (define (body-expander expr use-env def-env)
    (match-ast expr
     ((ast-body exprs ...)
      ;; FIXME Should this pre-expand with a limited env and only one level deep, so that we get defs, syntax defs and bodies expanded?
      ;; What about macros that expand to defs?
      ;; Can this perform an expansion fix-point?
      ;; Expand all nodes, see if there are any syntax defs in scope, expand again with the extended env and repeat.
      ;; What about existing macro redefinitions? Should these be disallowed?
      ;; Introduce expansion phases?
      ;; - 0 - runtime
      ;; - 1 - macros
      ;; - 2 - macro binders
      ;; - 3 - syntax elaboration
      (let* ((pre-expanded (map (partial expand use-env) exprs))
             ;; TODO
             ;; - pre-expand should also contain def-syntax nodes that omit expansion of their internals.
             ;; - def-syntax nodes should be extracted same as definitions and put in a wrapping letrec of their own.
             ;; - The use-env should be extended with extra syntax and then the body expansion should proceed.
             (defs (extract-defs pre-expanded))
             (non-defs (extract-non-defs pre-expanded)))
        (if (> (length defs) 0)
            (generated
             (make-ast-letrec (ast-node-location expr)
                              (unique-bindings defs (ast-node-context expr))
                              (reconstruct-simple-body non-defs expr)))
            (reconstruct-simple-body pre-expanded expr))))
     (else
      (compiler-bug "Invalid ast-body object" expr))))
  body-expander)

(define (extract-defs exprs)
  (foldr (lambda (e acc)
           (match-ast e
            ((def name value)
             (cons (generated
                    (make-ast-binding (ast-node-location e) name value))
                   acc))
            (else acc)))
         '()
         exprs))

(define (extract-non-defs exprs)
  (filter (compose not ast-def?)
          exprs))

(define (reconstruct-simple-body exprs parent)
  (let ((ctx (ast-node-context* parent "Bad `do` syntax")))
    (cond ((= (length exprs) 0)
           (raise-compilation-error
            parent
            (format "~a, expected at least one non-definition expression within:" ctx)))
          ((= (length exprs) 1)
           (car exprs))
          (else
           (generated
            ;; NOTE The context should be preserved.
            (set-ast-node-context
             (make-ast-do (ast-node-location parent)
                          exprs)
             ctx))))))
