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
    ((lambda) (ast-update expr 'body elaborate-unquoted))
    ((let) (ast-update (ast-update expr
                                   'bindings
                                   (partial map
                                            (lambda (b)
                                              (cons (car b)
                                                    (elaborate-unquoted (cdr b))))))
                       'body
                       elaborate-unquoted))
    ((letrec) (ast-update (ast-update expr
                                      'bindings
                                      (partial map
                                               (lambda (b)
                                                 (cons (car b)
                                                       (elaborate-unquoted (cdr b))))))
                          'body
                          elaborate-unquoted))
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
  (cond ((ast-matches? expr '(if _ _ _))
         (replace expr
                  (make-if-node (ast-list-nth expr 1)
                                (ast-list-nth expr 2)
                                (ast-list-nth expr 3))))
        (else
         (let ((node (ast-list-car expr)))
           (raise-compilation-error
            (get-location node)
            "Bad `if` syntax, expected exactly three expressions - condition, then and else branches - to follow:")))))

(define (reconstruct-do expr)
  (cond ((ast-matches? expr '(do _ . _))
         (replace expr
                  (make-do-node (ast-list-cdr expr))))
        (else
         (let ((node (ast-list-car expr)))
           (raise-compilation-error
            (get-location node)
            "Bad `do` syntax, expected at least one expression to follow:")))))

(define (reconstruct-lambda expr)
  (cond ((ast-matches? expr '(lambda _ _ . _))
         (replace expr
                  (make-lambda-node (valid-formals (ast-list-nth expr 1)
                                                   "Bad `lambda` formal arguments syntax")
                                    (wrap-body (cdr (ast-list-cdr expr))))))
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

(define (wrap-body exprs)
  (if (> (length exprs) 1)
      ;; NOTE The body spans all the expressions within it.
      (at (location (get-location-start (car exprs))
                    (get-location-end (last exprs)))
          (generated
           (make-do-node exprs)))
      (car exprs)))

(define (reconstruct-let expr)
  (cond ((ast-matches? expr '(let (_ . _) _ . _))
         (replace expr
                  (make-let-node (valid-bindings (ast-list-nth expr 1) "Bad `let` bindings syntax")
                                 (wrap-body (cdr (ast-list-cdr expr))))))
        ((ast-matches? expr '(letrec (_ . _) _ . _))
         (replace expr
                  (make-letrec-node (valid-bindings (ast-list-nth expr 1) "Bad `letrec` bindings syntax")
                                    (wrap-body (cdr (ast-list-cdr expr))))))
        ((ast-matches? expr '(_ () _ . _))
         (replace expr
                  (make-do-node (cdr (ast-list-cdr expr)))))
        (else
         (let ((node (ast-list-car expr)))
           (raise-compilation-error
            (get-location node)
            (format "Bad `~a` syntax, expected a list of bindings followed by a body:"
                    (ast-symbol-value node)))))))

(define (valid-bindings bindings prefix)
  (map (lambda (b)
         (valid-binding b prefix))
       (ast-list-values bindings)))

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
  (cond ((ast-matches? expr ''_)
         (replace expr
                  (make-quote-node (ast-list-nth expr 1))))
        ((ast-matches? expr '`_)
         (replace expr
                   (make-quasiquote-node (ast-list-nth expr 1))))
        (else
         (let ((node (ast-list-car expr)))
           (raise-compilation-error
            (get-location node)
            (format "Bad `~a` syntax, expected exactly one expression to follow:"
                    (ast-symbol-value node)))))))

(define (reconstruct-unquote expr)
  (cond ((ast-matches? expr ',_)
         (replace expr
                  (make-unquote-node (ast-list-nth expr 1))))
        ((ast-matches? expr ',@_)
         (replace expr
                  (make-unquote-splicing-node (ast-list-nth expr 1))))
        (else
         (let ((node (ast-list-car expr)))
           (raise-compilation-error
            (get-location node)
            (format "Bad `~a` syntax, expected exactly one expression to follow:"
                    (ast-symbol-value node)))))))

(define (reconstruct-def expr)
  (cond ((ast-matches? expr '(define (_ . _) _ . _))
         (let* ((func-def (ast-list-nth expr 1))
                (name (ast-list-car func-def))
                (formals (ast-list-cdr func-def))
                (body (cdr (ast-list-cdr expr))))
           (replace expr
                    (make-def-node (valid-symbol name "Bad `define` syntax")
                                   (at (get-location expr)
                                       (generated
                                        (make-lambda-node (valid-formals (at (get-location func-def)
                                                                             (generated
                                                                              (make-list-node formals)))
                                                                         "Bad `define` function signature syntax")
                                                          (wrap-body body))))))))
        ((and (ast-matches? expr '(define _ _)))
         (replace expr
                  (make-def-node (valid-symbol (ast-list-nth expr 1)
                                               "Bad `define` syntax")
                                 (ast-list-nth expr 2))))
        (else
         (let ((node (ast-list-car expr)))
           (raise-compilation-error
            (get-location node)
            "Bad `define` syntax, expected either an identifier and an expression or a function signature and a body to follow:")))))

(define (reconstruct-app expr)
  (cond ((ast-matches? expr '(_ . _))
         (replace expr
                  (make-app-node (ast-list-car expr)
                                 (ast-list-cdr expr))))
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
