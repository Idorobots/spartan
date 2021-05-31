;; Semantic elaboration.
;; This phase checks syntax form correctness - if different syntax forms are used correctly, reserved keywords are used in the right positions, etc.

(load-once "compiler/utils/utils.scm")

(load-once "compiler/env.scm")
(load-once "compiler/pass.scm")
(load-once "compiler/ast.scm")
(load-once "compiler/errors.scm")

(define elaborate
  (pass (schema "elaborate"
                'errors a-list?
                'ast (ast-subset? '(quote quasiquote unquote unquote-splicing
                                    number symbol string list
                                    if let binding lambda app def
                                    primop-app body <error>)))
        (lambda (env)
          (let ((result (collect-errors (env-get env 'errors)
                                        (lambda ()
                                          (elaborate-unquoted (env-get env 'ast))))))
            (env-set env
                     'ast (car result)
                     'errors (cadr result))))))

(define (elaborate-unquoted expr)
  (case (ast-node-type expr)
    ((<error> quote number symbol string)
     expr)
    ((do)
     (set-ast-do-exprs expr (map elaborate-unquoted (ast-do-exprs expr))))
    ((body)
     (set-ast-body-exprs expr (map elaborate-unquoted (ast-body-exprs expr))))
    ((if)
     (-> expr
         (set-ast-if-condition (elaborate-unquoted (ast-if-condition expr)))
         (set-ast-if-then (elaborate-unquoted (ast-if-then expr)))
         (set-ast-if-else (elaborate-unquoted (ast-if-else expr)))))
    ((lambda)
     (set-ast-lambda-body expr (elaborate-unquoted (ast-lambda-body expr))))
    ((let)
     (-> expr
         (set-ast-let-body (elaborate-unquoted (ast-let-body expr)))
         (set-ast-let-bindings (map elaborate-unquoted (ast-let-bindings expr)))))
    ((letrec)
     (-> expr
         (set-ast-letrec-body (elaborate-unquoted (ast-letrec-body expr)))
         (set-ast-letrec-bindings (map elaborate-unquoted (ast-letrec-bindings expr)))))
    ((binding)
     (-> expr
         (set-ast-binding-var (elaborate-unquoted (ast-binding-var expr)))
         (set-ast-binding-val (elaborate-unquoted (ast-binding-val expr)))))
    ((def)
     (set-ast-def-value expr (elaborate-unquoted (ast-def-value expr))))
    ((quasiquote)
     (set-ast-quasiquote-expr expr (elaborate-quoted (ast-quasiquote-expr expr))))
    ((unquote)
     (set-ast-unquote-expr expr (elaborate-unquoted (ast-unquote-expr expr))))
    ((unquote-splicing)
     (set-ast-unquote-splicing-expr expr (elaborate-unquoted (ast-unquote-splicing-expr expr))))
    ((primop-app)
     (set-ast-primop-app-args expr (map elaborate-unquoted (ast-primop-app-args expr))))
    ((app)
     (elaborate-app expr))
    ((list)
     (elaborate-unquoted
      (reconstruct-syntax-forms expr)))
    (else (compiler-bug "Unrecognized expression passed to elaborate-unquoted:" expr))))

(define (elaborate-quoted expr)
  ;; NOTE We don't want the value within quasiquote to be elaborated.
  (case (ast-node-type expr)
    ((<error> quote number symbol string) expr)
    ((unquote)
     (set-ast-unquote-expr expr (elaborate-unquoted (ast-unquote-expr expr))))
    ((unquote-splicing)
     (set-ast-unquote-splicing-expr expr (elaborate-unquoted (ast-unquote-splicing-expr expr))))
    ((list)
     (let ((values (ast-list-values expr)))
       (if (and (not (empty? values))
                (is-type? (car values) 'symbol)
                (member (ast-symbol-value (car values)) '(unquote unquote-splicing)))
           (elaborate-quoted (reconstruct-unquote expr))
           (set-ast-list-values expr (map elaborate-quoted values)))))
    (else (compiler-bug "Unrecognized expression passed to elaborate-quoted:" expr))))

(define (elaborate-app expr)
  (-> expr
      (set-ast-app-op (valid-app-procedure (elaborate-unquoted (ast-app-op expr))))
      (set-ast-app-args (map elaborate-unquoted (ast-app-args expr)))))

(define (reconstruct-syntax-forms expr)
  (let ((values (ast-list-values expr)))
    (if (and (not (empty? values))
             (is-type? (car values) 'symbol))
        (case (ast-symbol-value (car values))
          ((if)
           (reconstruct-if expr))
          ((do)
           (reconstruct-do expr))
          ((lambda)
           (reconstruct-lambda expr))
          ((let letrec)
           (reconstruct-let expr))
          ((quote quasiquote)
           (reconstruct-quote expr))
          ((unquote unquote-splicing)
           (reconstruct-unquote expr))
          ((define)
           (reconstruct-def expr))
          (else
           (reconstruct-app expr)))
        (reconstruct-app expr))))

(define (reconstruct-if expr)
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

(define (reconstruct-do expr)
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

(define (reconstruct-lambda expr)
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
  '(lambda let letrec if do))

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
  (define (check-uniqueness name rest)
    (cond ((empty? rest)
           name)
          ((and (ast-symbol? name)
                (equal? (ast-symbol-value name) '_))
           name)
          ((and (ast-symbol? name)
                (ast-symbol? (car rest))
                (equal? (ast-symbol-value name)
                        (ast-symbol-value (car rest))))
           (raise-compilation-error
            (car rest)
            (format "~a, duplicate formal argument `~a`:" prefix (ast-symbol-value (car rest)))))
          (else
           (check-uniqueness name (cdr rest)))))
  (if (empty? args)
      '()
      (cons (check-uniqueness (car args) (cdr args))
            (unique-formals (cdr args) prefix))))

(define (valid-symbol symbol prefix)
  (if (is-type? symbol 'symbol)
      symbol
      (raise-compilation-error
       symbol
       (format "~a, expected a symbol but got a ~a instead:" prefix (ast-node-type symbol)))))

(define (reconstruct-let expr)
  (match-ast expr
   ((list (symbol 'let) (list first-binding rest-bindings ...) first-body rest-body ...)
    (replace expr
             (make-ast-let (ast-node-location expr)
                           (valid-bindings (cons first-binding rest-bindings) "Bad `let` bindings syntax")
                           (make-ast-body (ast-node-location expr)
                                          (cons first-body rest-body)
                                          "Bad `let` body syntax"))))
   ((list (symbol 'letrec) (list first-binding rest-bindings ...) first-body rest-body ...)
    (replace expr
             (make-ast-letrec (ast-node-location expr)
                              (valid-bindings (cons first-binding rest-bindings) "Bad `letrec` bindings syntax")
                              (make-ast-body (ast-node-location expr)
                                             (cons first-body rest-body)
                                             "Bad `letrec` body syntax"))))
   ((list head (list) first rest ...)
    (make-ast-body (ast-node-location expr)
                   (cons first rest)
                   (format "Bad `~a` body syntax" (safe-symbol-value head))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       (format "Bad `~a` syntax, expected a list of bindings followed by a body:"
               (ast-symbol-value node)))))))

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
  (define (check-uniqueness b rest)
    (let ((var (ast-binding-var b)))
      (cond ((empty? rest)
             b)
            ((and (ast-symbol? var)
                  (equal? (ast-symbol-value var) '_))
             b)
            ((and (ast-symbol? var)
                  (ast-symbol? (ast-binding-var (car rest)))
                  (equal? (ast-symbol-value var)
                          (ast-symbol-value (ast-binding-var (car rest)))))
             (let ((e (raise-compilation-error
                       (car rest)
                       (format "~a, duplicate binding identifier `~a`:" prefix (ast-symbol-value var)))))
               (make-ast-binding (ast-node-location b) e e)))
            (else
             (check-uniqueness b (cdr rest))))))
  (if (empty? bindings)
      '()
      (cons (check-uniqueness (car bindings) (cdr bindings))
            (unique-bindings (cdr bindings) prefix))))

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

(define (reconstruct-quote expr)
  (match-ast expr
   ((list (symbol 'quote) value)
    (replace expr
             (make-ast-quote (ast-node-location expr)
                             value)))
   ((list (symbol 'quasiquote) value)
    (replace expr
             (make-ast-quasiquote (ast-node-location expr)
                                  value)))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       (format "Bad `~a` syntax, expected exactly one expression to follow:"
               (ast-symbol-value node)))))))

(define (reconstruct-unquote expr)
  (match-ast expr
   ((list (symbol 'unquote) value)
    (replace expr
             (make-ast-unquote (ast-node-location expr)
                               value)))
   ((list (symbol 'unquote-splicing) value)
    (replace expr
             (make-ast-unquote-splicing (ast-node-location expr)
                                        value)))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       (format "Bad `~a` syntax, expected exactly one expression to follow:"
               (ast-symbol-value node)))))))

(define (reconstruct-def expr)
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

(define (reconstruct-app expr)
  (match-ast expr
   ((list op args ...)
    (replace expr
             (make-ast-app (ast-node-location expr)
                           op
                           args)))
   (else
    (raise-compilation-error
     expr
     "Bad call syntax, expected at least one expression within the call:"))))

(define (valid-app-procedure op)
  (let ((type (ast-node-type op)))
    (if (member type '(symbol if do body lambda let letrec app primop-app))
        op
        (raise-compilation-error
         op
         (format "Bad call syntax, expected an expression that evaluates to a procedure but got a ~a instead:" type)))))
