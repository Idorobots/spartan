;; Semantic elaboration.
;; This phase checks syntax form correctness - if different syntax forms are used correctly, reserved keywords are used in the right positions, etc.

(load "compiler/utils/utils.scm")

(load "compiler/env.scm")
(load "compiler/ast.scm")
(load "compiler/errors.scm")

(define (elaborate env)
  (let ((result (collect-errors (env-get env 'errors)
                                (lambda ()
                                  (elaborate-unquoted (env-get env 'ast))))))
    (env-set env
             'ast (car result)
             'errors (cadr result))))

(define (elaborate-unquoted expr)
  (case (get-type expr)
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
  (case (get-type expr)
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
             (make-if-node condition
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
             (make-body-node (cons first rest) "Bad `do` syntax")))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `do` syntax, expected at least one expression to follow:")))))

(define (reconstruct-lambda expr)
  (ast-case expr
   ((list 'lambda ,formals ,first . ,rest)
    (replace expr
             (make-lambda-node (valid-formals formals "Bad `lambda` formal arguments syntax")
                               (at (get-location expr)
                                   (make-body-node (cons first rest) "Bad `lambda` body syntax")))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `lambda` syntax, expected a formal arguments specification followed by a body:")))))

(define (valid-formals args prefix)
  (if (is-type? args 'list)
      (map (lambda (e)
             (valid-symbol e prefix))
           (ast-list-values args))
      (list
       (raise-compilation-error
        args
        (format "~a, expected a list of identifiers:" prefix)))))

(define (valid-symbol symbol prefix)
  (if (is-type? symbol 'symbol)
      symbol
      (raise-compilation-error
       symbol
       (format "~a, expected a symbol but got a ~a instead:" prefix (get-type symbol)))))

(define (reconstruct-let expr)
  (ast-case expr
   ((list 'let (list ,first-binding . ,rest-bindings) ,first-body . ,rest-body)
    (replace expr
             (make-let-node (valid-bindings (cons first-binding rest-bindings) "Bad `let` bindings syntax")
                            (at (get-location expr)
                                (make-body-node (cons first-body rest-body) "Bad `let` body syntax")))))
   ((list 'letrec (list ,first-binding . ,rest-bindings) ,first-body . ,rest-body)
    (replace expr
             (make-letrec-node (valid-bindings (cons first-binding rest-bindings) "Bad `letrec` bindings syntax")
                               (at (get-location expr)
                                   (make-body-node (cons first-body rest-body) "Bad `letrec` body syntax")))))
   ((list _ () ,first . ,rest)
    (replace expr
             (make-do-node (cons first rest))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       (format "Bad `~a` syntax, expected a list of bindings followed by a body:"
               (ast-symbol-value node)))))))

(define (valid-bindings bindings prefix)
  (map (lambda (b)
         (valid-binding b prefix))
       bindings))

(define (valid-binding binding prefix)
  (replace binding
           (if (and (is-type? binding 'list)
                    (equal? (length (ast-list-values binding)) 2))
               (make-binding-node (valid-symbol (ast-list-car binding) prefix)
                                  (ast-list-nth binding 1))
               (let ((e (raise-compilation-error
                         binding
                         (format "~a, expected a pair of an identifier and a value:" prefix))))
                 (make-binding-node e e)))))

(define (reconstruct-quote expr)
  (ast-case expr
   ((list 'quote ,value)
    (replace expr
             (make-quote-node value)))
   ((list 'quasiquote ,value)
    (replace expr
             (make-quasiquote-node value)))
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
             (make-unquote-node value)))
   ((list 'unquote-splicing ,value)
    (replace expr
             (make-unquote-splicing-node value)))
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
               (make-def-node (valid-symbol name "Bad `define` syntax")
                              (at (get-location expr)
                                  (generated
                                   (make-lambda-node (valid-formals (at (get-location func-def)
                                                                        (generated
                                                                         (make-list-node formals)))
                                                                    "Bad `define` function signature syntax")
                                                     (at (get-location expr)
                                                         (make-body-node (cons first rest) "Bad `define` function body syntax")))))))))
   ((list 'define ,name ,value)
    (replace expr
             (make-def-node (valid-symbol name "Bad `define` syntax")
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
             (make-app-node op args)))
   (else
    (raise-compilation-error
     expr
     "Bad call syntax, expected at least one expression within the call:"))))

(define (valid-app-procedure op)
  (let ((type (get-type op)))
    (if (member type '(symbol if do body lambda let letrec app primop-app))
        op
        (raise-compilation-error
         op
         (format "Bad call syntax, expected an expression that evaluates to a procedure but got a ~a instead:" type)))))
