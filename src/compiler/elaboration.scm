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
     (elaborate-sequence expr))
    ((list)
     (elaborate-syntax-forms expr))
    ((quasiquote)
     (elaborate-quasiquote expr))
    ((primop-app)
     (elaborate-primop expr))
    ((unquote unquote-splicing)
     (raise-compilation-error
      (get-location expr)
      (format "Misplaced `~a`, expected to be enclosed within a `quasiquote`:" (get-type expr))))
    (else (compiler-bug))))

(define (elaborate-quoted expr)
  (case (get-type expr)
    ((<error> quote number symbol string) expr)
    ((list)
     (maybe-elaborate-syntax-forms expr))
    ((unquote unquote-splicing)
     (elaborate-unquote expr))
    (else (compiler-bug))))

(define (elaborate-quasiquote expr)
  (ast-update expr 'value elaborate-quoted))

(define (elaborate-primop expr)
  (ast-update (ast-update expr 'op elaborate-unquoted)
              'args
              (partial map elaborate-unquoted)))

(define (elaborate-sequence expr)
  (ast-update expr 'exprs (partial map elaborate-unquoted)))

(define (elaborate-unquote expr)
  (ast-update expr 'value elaborate-unquoted))

(define (maybe-elaborate-syntax-forms expr)
  (let ((values (ast-get expr 'value)))
    (if (and (not (empty? values))
             (is-type? (car values) 'symbol)
             (member (ast-get (car values) 'value) '(unquote unquote-splicing)))
        (elaborate-quote expr)
        (ast-update expr 'value (partial map elaborate-quoted)))))

(define (elaborate-syntax-forms expr)
  (let ((values (ast-get expr 'value)))
    (if (and (not (empty? values))
             (is-type? (car values) 'symbol))
        (case (ast-get (car values) 'value)
          ((if)
           (elaborate-if expr))
          ((do)
           (elaborate-do expr))
          ((lambda)
           (elaborate-lambda expr))
          ((let letrec)
           (elaborate-let expr))
          ((quote quasiquote unquote unquote-splicing)
           (elaborate-quote expr))
          ((define)
           (elaborate-def expr))
          (else
           (elaborate-app expr)))
        (elaborate-app expr))))

(define (elaborate-if expr)
  (cond ((ast-matches? expr '(if _ _ _))
         (replace expr
                  (make-if-node (elaborate-unquoted (ast-list-nth expr 1))
                                (elaborate-unquoted (ast-list-nth expr 2))
                                (elaborate-unquoted (ast-list-nth expr 3)))))
        (else
         (let ((node (ast-list-nth expr 0)))
           (raise-compilation-error
            (get-location node)
            "Bad `if` syntax, expected exactly three expressions - condition, then and else branches - to follow:")))))

(define (elaborate-do expr)
  (cond ((ast-matches? expr '(do _ . _))
         (elaborate-sequence
          (replace expr
                  (make-do-node (cdr (ast-get expr 'value))))))
        (else
         (let ((node (ast-list-nth expr 0)))
           (raise-compilation-error
            (get-location node)
            "Bad `do` syntax, expected at least one expression to follow:")))))

(define (elaborate-lambda expr)
  (cond ((ast-matches? expr '(lambda _ _ . _))
         (replace expr
                  (make-lambda-node (valid-formals (ast-list-nth expr 1)
                                                   "Bad `lambda` formal arguments syntax")
                                    (wrap-body (cddr (ast-get expr 'value))))))
        (else
         (let ((node (ast-list-nth expr 0)))
           (raise-compilation-error
            (get-location node)
            "Bad `lambda` syntax, expected a formal arguments specification followed by a body:")))))

(define (valid-formals args prefix)
  (if (is-type? args 'list)
      (map (lambda (e)
             (valid-symbol e prefix))
           (ast-get args 'value))
      (list
       (raise-compilation-error
        (get-location args)
        (format "~a, expected a list of identifiers:" prefix)))))

(define (valid-symbol symbol prefix)
  (if (is-type? symbol 'symbol)
      symbol
      (raise-compilation-error
       (get-location symbol)
       (format "~a, expected a symbol but got a ~a instead:" prefix (ast-get symbol 'type)))))

(define (wrap-body exprs)
  (if (> (length exprs) 1)
      ;; NOTE The body spans all the expressions within it.
      (at (location (car (get-location (car exprs)))
                    (cdr (get-location (list-ref exprs (- (length exprs) 1)))))
          (generated
           (make-do-node (map elaborate-unquoted
                              exprs))))
      (elaborate-unquoted
       (car exprs))))

(define (elaborate-let expr)
  (cond ((ast-matches? expr '(let (_ . _) _ . _))
         (replace expr
                  (make-let-node (valid-bindings (ast-list-nth expr 1) "Bad `let` bindings syntax")
                                 (wrap-body (cddr (ast-get expr 'value))))))
        ((ast-matches? expr '(letrec (_ . _) _ . _))
         (replace expr
                  (make-letrec-node (valid-bindings (ast-list-nth expr 1) "Bad `letrec` bindings syntax")
                                    (wrap-body (cddr (ast-get expr 'value))))))
        ((ast-matches? expr '(_ () _ . _))
         (replace expr
                  (make-do-node (map elaborate-unquoted
                                     (cddr (ast-get expr 'value))))))
        (else
         (let ((node (ast-list-nth expr 0)))
           (raise-compilation-error
            (get-location node)
            (format "Bad `~a` syntax, expected a list of bindings followed by a body:"
                    (ast-get node 'value)))))))

(define (valid-bindings bindings prefix)
  (map (lambda (b)
         (valid-binding b prefix))
       (ast-get bindings 'value)))

(define (valid-binding binding prefix)
  (if (and (is-type? binding 'list)
           (equal? (length (ast-get binding 'value)) 2))
      (cons (valid-symbol (ast-list-nth binding 0) prefix)
            (elaborate-unquoted (ast-list-nth binding 1)))
      (cons (make-error-node)
            (raise-compilation-error
             (get-location binding)
             (format "~a, expected a pair of an identifier and a value:" prefix)))))

(define (elaborate-quote expr)
  (cond ((ast-matches? expr ''_)
         (replace expr
                  (make-quote-node (ast-list-nth expr 1))))
        ((ast-matches? expr '`_)
         (elaborate-quasiquote
          (replace expr
                   (make-quasiquote-node (ast-list-nth expr 1)))))
        ((ast-matches? expr ',_)
         (elaborate-unquote
          (replace expr
                  (make-unquote-node (ast-list-nth expr 1)))))
        ((ast-matches? expr ',@_)
         (elaborate-unquote
          (replace expr
                  (make-unquote-splicing-node (ast-list-nth expr 1)))))
        (else
         (let ((node (ast-list-nth expr 0)))
           (raise-compilation-error
            (get-location node)
            (format "Bad `~a` syntax, expected exactly one expression to follow:"
                    (ast-get node 'value)))))))

(define (elaborate-def expr)
  (cond ((ast-matches? expr '(define (_ . _) _ . _))
         (let* ((func-def (ast-list-nth expr 1))
                (name (ast-list-nth func-def 0))
                (formals (cdr (ast-get func-def 'value)))
                (body (cddr (ast-get expr 'value))))
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
                                 (elaborate-unquoted
                                  (ast-list-nth expr 2)))))
        (else
         (let ((node (ast-list-nth expr 0)))
           (raise-compilation-error
            (get-location node)
            "Bad `define` syntax, expected either an identifier and an expression or a function signature and a body to follow:")))))

(define (elaborate-app expr)
  (cond ((ast-matches? expr '(_ . _))
         (replace expr
                  (make-app-node (valid-app-procedure (elaborate-unquoted (ast-list-nth expr 0)) "Bad call syntax")
                                 (map elaborate-unquoted
                                      (cdr (ast-get expr 'value))))))
        (else
         (raise-compilation-error
          (get-location expr)
          "Bad call syntax, expected at least one expression within the call:"))))

(define (valid-app-procedure op prefix)
  (let ((type (get-type op)))
    (if (member type '(symbol if do lambda let letrec app primop-app))
      op
      (raise-compilation-error
       (get-location op)
       (format "~a, expected an expression that evaluates to a procedure but got a ~a instead:" prefix type)))))
