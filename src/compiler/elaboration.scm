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
  (cond ((and (ast-matches? expr '(lambda _ _ . _))
              (valid-formals? (ast-list-nth expr 1)))
         (replace expr
                  (make-lambda-node (ast-get (ast-list-nth expr 1) 'value)
                                    (wrap-body (cddr (ast-get expr 'value))))))
        ((ast-matches? expr '(lambda _ _ . _))
         (let ((node (ast-list-nth expr 1)))
           (raise-compilation-error
            (get-location node)
            "Bad formal arguments specification, expected a list of identifiers:")))
        (else
         (let ((node (ast-list-nth expr 0)))
           (raise-compilation-error
            (get-location node)
            "Bad `lambda` syntax, expected a formal arguments specification followed by a body:")))))

(define (wrap-body exprs)
  (if (> (length exprs) 1)
      ;; NOTE The body spans all the expressions within it.
      (at (location (car (get-location (car exprs)))
                    (cdr (get-location (list-ref exprs (- (length exprs) 1)))))
          (generated
           (make-do-node exprs)))
      (car exprs)))

(define (valid-formals? args)
  (and (is-type? args 'list)
       (foldl (lambda (arg acc)
                (and acc (is-type? arg 'symbol)))
              #t
              (ast-get args 'value))))

(define (elaborate-let expr)
  (cond ((and (ast-matches? expr '(let (_ . _) _ . _))
              (valid-bindings? (ast-list-nth expr 1)))
         (replace expr
                  (make-let-node (unwrap-bindings (ast-list-nth expr 1))
                                 (wrap-body (cddr (ast-get expr 'value))))))
        ((and (ast-matches? expr '(letrec (_ . _) _ . _))
              (valid-bindings? (ast-list-nth expr 1)))
         (replace expr
                  (make-letrec-node (unwrap-bindings (ast-list-nth expr 1))
                                    (wrap-body (cddr (ast-get expr 'value))))))
        ((ast-matches? expr '(_ () _ . _))
         (replace expr
                  (make-do-node (cddr (ast-get expr 'value)))))
        ((ast-matches? expr '(_ _ _ . _))
         (let ((node (ast-list-nth expr 1)))
           (raise-compilation-error
            (get-location node)
            "Bad bindings format, expected a list of (identifier <value>) pairs:")))
        (else
         (let ((node (ast-list-nth expr 0)))
           (raise-compilation-error
            (get-location node)
            (format "Bad `~a` syntax, expected a list of bindings followed by a body:"
                    (ast-get node 'value)))))))

(define (unwrap-bindings list)
  (map (lambda (sublist)
         (let ((exprs (ast-get sublist 'value)))
           (cons (car exprs)
                 (cadr exprs))))
       (ast-get list 'value)))

(define (valid-bindings? args)
  (and (is-type? args 'list)
       (foldl (lambda (arg acc)
                (and acc (valid-binding? arg)))
              #t
              (ast-get args 'value))))

(define (valid-binding? arg)
  (and (is-type? arg 'list)
       (equal? (length (ast-get arg 'value)) 2)
       (is-type? (ast-list-nth arg 0) 'symbol)))

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
