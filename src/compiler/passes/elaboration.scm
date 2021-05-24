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
    ((<error> quote number symbol string) expr)
    ((do body)
     (ast-update expr 'exprs (partial map elaborate-unquoted)))
    ((if)
     (foldl (lambda (field acc)
              (ast-update acc field elaborate-unquoted))
            expr
            '(condition then else)))
    ((lambda) (ast-update expr 'body elaborate-unquoted))
    ((let letrec) (ast-update (ast-update expr 'body elaborate-unquoted) 'bindings (partial map elaborate-unquoted)))
    ((binding) (ast-update (ast-update expr 'var elaborate-unquoted) 'val elaborate-unquoted))
    ((def)
     (ast-update expr 'value elaborate-unquoted))
    ((quasiquote)
     (ast-update expr 'value elaborate-quoted))
    ((unquote unquote-splicing)
     (ast-update expr 'value elaborate-unquoted))
    ((primop-app)
     (ast-update expr 'args (partial map elaborate-unquoted)))
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
    ((unquote unquote-splicing)
     (ast-update expr 'value elaborate-unquoted))
    ((list)
     (let ((values (ast-list-values expr)))
       (if (and (not (empty? values))
                (is-type? (car values) 'symbol)
                (member (ast-symbol-value (car values)) '(unquote unquote-splicing)))
           (elaborate-quoted (reconstruct-unquote expr))
           (ast-update expr 'value (partial map elaborate-quoted)))))
    (else (compiler-bug "Unrecognized expression passed to elaborate-quoted:" expr))))

(define (elaborate-app expr)
  (ast-update (ast-update expr 'op (lambda (op)
                                     (valid-app-procedure (elaborate-unquoted op))))
              'args
              (partial map elaborate-unquoted)))

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
  (ast-case expr
   ((list 'if ,condition ,then ,else)
    (replace expr
             (make-ast-if condition
                           then
                           else)))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `if` syntax, expected exactly three expressions - condition, then and else branches - to follow:")))))

(define (reconstruct-do expr)
  (ast-case expr
   ((list 'do ,first . ,rest)
    (replace expr
             ;; NOTE User-supplied do needs to be body-expanded as well.
             (make-ast-body (cons first rest) "Bad `do` syntax")))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `do` syntax, expected at least one expression to follow:")))))

(define (reconstruct-lambda expr)
  (ast-case expr
   ((list 'lambda ,formals ,first . ,rest)
    (replace expr
             (make-ast-lambda (valid-formals formals "Bad `lambda` formal arguments syntax")
                               (at (ast-node-location expr)
                                   (make-ast-body (cons first rest) "Bad `lambda` body syntax")))))
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
  (ast-case expr
   ((list 'let (list ,first-binding . ,rest-bindings) ,first-body . ,rest-body)
    (replace expr
             (make-ast-let (valid-bindings (cons first-binding rest-bindings) "Bad `let` bindings syntax")
                            (at (ast-node-location expr)
                                (make-ast-body (cons first-body rest-body) "Bad `let` body syntax")))))
   ((list 'letrec (list ,first-binding . ,rest-bindings) ,first-body . ,rest-body)
    (replace expr
             (make-ast-letrec (valid-bindings (cons first-binding rest-bindings) "Bad `letrec` bindings syntax")
                               (at (ast-node-location expr)
                                   (make-ast-body (cons first-body rest-body) "Bad `letrec` body syntax")))))
   ((list _ () ,first . ,rest)
    (replace expr
             (make-ast-do (cons first rest))))
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
                 (at (ast-node-location b)
                     (make-ast-binding e e)))
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
               (at (ast-node-location b)
                   (make-ast-binding e e))))
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
               (make-ast-binding (valid-symbol (ast-list-car binding) prefix)
                                  (ast-list-nth binding 1))
               (let ((e (raise-compilation-error
                         binding
                         (format "~a, expected a pair of an identifier and a value:" prefix))))
                 (at (ast-node-location binding)
                     (make-ast-binding e e))))))

(define (reconstruct-quote expr)
  (ast-case expr
   ((list 'quote ,value)
    (replace expr
             (make-ast-quote value)))
   ((list 'quasiquote ,value)
    (replace expr
             (make-ast-quasiquote value)))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       (format "Bad `~a` syntax, expected exactly one expression to follow:"
               (ast-symbol-value node)))))))

(define (reconstruct-unquote expr)
  (ast-case expr
   ((list 'unquote ,value)
    (replace expr
             (make-ast-unquote value)))
   ((list 'unquote-splicing ,value)
    (replace expr
             (make-ast-unquote-splicing value)))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       (format "Bad `~a` syntax, expected exactly one expression to follow:"
               (ast-symbol-value node)))))))

(define (reconstruct-def expr)
  (ast-case expr
   ((list 'define (list ,name . ,formals) ,first . ,rest)
    (let ((func-def (ast-list-nth expr 1)))
      (replace expr
               (make-ast-def (valid-symbol name "Bad `define` syntax")
                              (at (ast-node-location expr)
                                  (generated
                                   (make-ast-lambda (valid-formals (at (ast-node-location func-def)
                                                                        (generated
                                                                         (make-ast-list formals)))
                                                                    "Bad `define` function signature syntax")
                                                     (at (ast-node-location expr)
                                                         (make-ast-body (cons first rest) "Bad `define` function body syntax")))))))))
   ((list 'define ,name ,value)
    (replace expr
             (make-ast-def (valid-symbol name "Bad `define` syntax")
                            value)))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `define` syntax, expected either an identifier and an expression or a function signature and a body to follow:")))))

(define (reconstruct-app expr)
  (ast-case expr
   ((list ,op . ,args)
    (replace expr
             (make-ast-app op args)))
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
