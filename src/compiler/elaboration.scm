;; Semantic elaboration.
;; This phase checks syntax form correctness - if different syntax forms are used correctly, reserved keywords are used in the right positions, etc.

(load "compiler/utils.scm")
(load "compiler/env.scm")
(load "compiler/errors.scm")
(load "compiler/tree-ast.scm")

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
    ((do)
     (ast-update expr 'exprs (partial map elaborate-unquoted)))
    ((if)
     (foldl (lambda (field acc)
              (ast-update acc field elaborate-unquoted))
            expr
            '(condition then else)))
    ((lambda) (ast-update expr 'body (partial maybe-map elaborate-unquoted)))
    ((let) (ast-update (ast-update expr
                                   'bindings
                                   (partial map
                                            (lambda (b)
                                              (cons (car b)
                                                    (elaborate-unquoted (cdr b))))))
                       'body
                       (partial maybe-map elaborate-unquoted)))
    ((letrec) (ast-update (ast-update expr
                                      'bindings
                                      (partial map
                                               (lambda (b)
                                                 (cons (car b)
                                                       (elaborate-unquoted (cdr b))))))
                          'body
                          (partial maybe-map elaborate-unquoted)))
    ((def)
     (ast-update expr 'value elaborate-unquoted))
    ((quasiquote)
     (ast-update expr 'value elaborate-quoted))
    ((unquote unquote-splicing)
     (fail-unquote expr))
    ((primop-app)
     (ast-update expr 'args (partial map elaborate-unquoted)))
    ((app)
     (elaborate-app expr))
    ((list)
     (elaborate-unquoted
      (reconstruct-syntax-forms expr)))
    (else (compiler-bug))))

(define (elaborate-quoted expr)
  (case (get-type expr)
    ((<error> quote number symbol string) expr)
    ((unquote unquote-splicing)
     (ast-update expr 'value elaborate-unquoted))
    ((list)
     (let ((rec (maybe-reconstruct-syntax-forms expr)))
       (if rec
           (elaborate-quoted rec)
           (ast-update expr 'value (partial map elaborate-quoted)))))
    (else (compiler-bug))))

(define (elaborate-app expr)
  (ast-update (ast-update expr 'op (lambda (op)
                                     (valid-app-procedure (elaborate-unquoted op))))
              'args
              (partial map elaborate-unquoted)))

(define (fail-unquote expr)
  (raise-compilation-error
   (get-location expr)
   ;; NOTE Misplaced `<error>`, haha.
   (format "Misplaced `~a`, expected to be enclosed within a `quasiquote`:" (get-type expr))))

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
           (fail-unquote
            (reconstruct-unquote expr)))
          ((define)
           (reconstruct-def expr))
          (else
           (reconstruct-app expr)))
        (reconstruct-app expr))))

(define (maybe-reconstruct-syntax-forms expr)
  (let ((values (ast-list-values expr)))
    (and (not (empty? values))
         (is-type? (car values) 'symbol)
         (member (ast-symbol-value (car values)) '(unquote unquote-splicing))
         (reconstruct-unquote expr))))

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
       (get-location node)
       "Bad `if` syntax, expected exactly three expressions - condition, then and else branches - to follow:")))))

(define (reconstruct-do expr)
  (ast-case expr
   ((list 'do ,first . ,rest)
    (replace expr
             (make-do-node (cons first rest))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       (get-location node)
       "Bad `do` syntax, expected at least one expression to follow:")))))

(define (reconstruct-lambda expr)
  (ast-case expr
   ((list 'lambda ,formals ,first . ,rest)
    (replace expr
             (make-lambda-node (valid-formals formals "Bad `lambda` formal arguments syntax")
                               (wrap-with-do (cons first rest) "Bad `lambda` body syntax"))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       (get-location node)
       "Bad `lambda` syntax, expected a formal arguments specification followed by a body:")))))

(define (valid-formals args prefix)
  (if (is-type? args 'list)
      (map (lambda (e)
             (valid-symbol e prefix))
           (ast-list-values args))
      (list
       (raise-compilation-error
        (get-location args)
        (format "~a, expected a list of identifiers:" prefix)))))

(define (valid-symbol symbol prefix)
  (if (is-type? symbol 'symbol)
      symbol
      (raise-compilation-error
       (get-location symbol)
       (format "~a, expected a symbol but got a ~a instead:" prefix (get-type symbol)))))

(define (reconstruct-let expr)
  (ast-case expr
   ((list 'let (list ,first-binding . ,rest-bindings) ,first-body . ,rest-body)
    (replace expr
             (make-let-node (valid-bindings (cons first-binding rest-bindings) "Bad `let` bindings syntax")
                            (wrap-with-do (cons first-body rest-body) "Bad `let` body syntax"))))
   ((list 'letrec (list ,first-binding . ,rest-bindings) ,first-body . ,rest-body)
    (replace expr
             (make-letrec-node (valid-bindings (cons first-binding rest-bindings) "Bad `letrec` bindings syntax")
                               (wrap-with-do (cons first-body rest-body) "Bad `letrec` body syntax"))))
   ((list _ () ,first . ,rest)
    (replace expr
             (make-do-node (cons first rest))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       (get-location node)
       (format "Bad `~a` syntax, expected a list of bindings followed by a body:"
               (ast-symbol-value node)))))))

(define (valid-bindings bindings prefix)
  (map (lambda (b)
         (valid-binding b prefix))
       bindings))

(define (valid-binding binding prefix)
  (if (and (is-type? binding 'list)
           (equal? (length (ast-list-values binding)) 2))
      (cons (valid-symbol (ast-list-car binding) prefix)
            (ast-list-nth binding 1))
      (cons (make-error-node)
            (raise-compilation-error
             (get-location binding)
             (format "~a, expected a pair of an identifier and a value:" prefix)))))

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
       (get-location node)
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
       (get-location node)
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
                                                     (wrap-with-do (cons first rest) "Bad `define` function body syntax"))))))))
   ((list 'define ,name ,value)
    (replace expr
             (make-def-node (valid-symbol name "Bad `define` syntax")
                            value)))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       (get-location node)
       "Bad `define` syntax, expected either an identifier and an expression or a function signature and a body to follow:")))))

(define (reconstruct-app expr)
  (ast-case expr
   ((list ,op . ,args)
    (replace expr
             (make-app-node op args)))
   (else
    (raise-compilation-error
     (get-location expr)
     "Bad call syntax, expected at least one expression within the call:"))))

(define (valid-app-procedure op)
  (let ((type (get-type op)))
    (if (member type '(symbol if do lambda let letrec app primop-app))
        op
        (raise-compilation-error
         (get-location op)
         (format "Bad call syntax, expected an expression that evaluates to a procedure but got a ~a instead:" type)))))
