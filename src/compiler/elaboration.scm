;; Semantic elaboration.
;; This phase checks syntax form correctness - if different syntax forms are used correctly, reserved keywords are used in the right positions, etc.

(load "compiler/utils.scm")
(load "compiler/env.scm")
(load "compiler/errors.scm")
(load "compiler/tree-ast.scm")

(define (elaborate env)
  (let ((result (collect-errors (env-get env 'errors)
                                (lambda ()
                                  (map-ast elaborate-syntax-forms
                                           id
                                           (env-get env 'ast))))))
    (env-set env
             'ast (car result)
             'errors (cadr result))))

(define (elaborate-syntax-forms expr)
  (if (is-type? expr 'list)
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
               (elaborate-define expr))
              (else expr))
            expr))
      expr))

(define (elaborate-if expr)
  (cond ((ast-matches? expr '(if _ _ _))
         (replace expr
                  (make-if-node (ast-list-nth expr 1)
                                (ast-list-nth expr 2)
                                (ast-list-nth expr 3))))
        (else
         (let ((node (ast-list-nth expr 0)))
           (raise-compilation-error
            (get-location node)
            "Bad `if` syntax, expected exactly three expressions - condition, then and else branches - to follow:")))))

(define (elaborate-do expr)
  (cond ((ast-matches? expr '(do _ . _))
         (replace expr
                  (make-do-node (cdr (ast-get expr 'value)))))
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
           (make-do-node exprs)))
      (car exprs)))

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
                  (make-do-node (cddr (ast-get expr 'value)))))
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
            (ast-list-nth binding 1))
      (cons (make-error-node)
            (raise-compilation-error
             (get-location binding)
             (format "~a, expected a pair of an identifier and a value:" prefix)))))

(define (elaborate-quote expr)
  (cond ((ast-matches? expr ''_)
         (let ((value (ast-list-nth expr 1)))
           (replace expr
               (make-quote-node value))))
        ((ast-matches? expr '`_)
         (let ((value (ast-list-nth expr 1)))
           (replace expr
               (make-quasiquote-node value))))
        ((ast-matches? expr ',_)
         (let ((value (ast-list-nth expr 1)))
           (replace expr
               (make-unquote-node value))))
        ((ast-matches? expr ',@_)
         (let ((value (ast-list-nth expr 1)))
           (replace expr
               (make-unquote-splicing-node value))))
        (else
         (let ((node (ast-list-nth expr 0)))
           (raise-compilation-error
            (get-location node)
            (format "Bad `~a` syntax, expected exactly one expression to follow:"
                    (ast-get node 'value)))))))

(define (elaborate-define expr)
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
                                 (ast-list-nth expr 2))))
        (else
         (let ((node (ast-list-nth expr 0)))
           (raise-compilation-error
            (get-location node)
            "Bad `define` syntax, expected either an identifier and an expression or a function signature and a body to follow:")))))
