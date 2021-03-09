;; Semantic elaboration.
;; This phase checks syntax form correctness - if different syntax forms are used correctly, reserved keywords are used in the right positions, etc.

(load "compiler/utils.scm")
(load "compiler/env.scm")
(load "compiler/errors.scm")
(load "compiler/tree-ast.scm")

(define (elaborate-semantics env)
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
              ((quote quasiquote unquote unquote-splicing)
               (elaborate-quote expr))
              (else expr))
            expr))
      expr))

(define (elaborate-if expr)
  (cond ((ast-matches? expr '(if _ _ _))
         (at (get-location expr)
             (make-if-node (ast-list-nth expr 1)
                           (ast-list-nth expr 2)
                           (ast-list-nth expr 3))))
        (else
         (let* ((node (ast-list-nth expr 0)))
           (raise-compilation-error
            (get-location node)
            "Bad `if` syntax, expected exactly three expressions - condition, then and else branches - to follow:")))))

(define (elaborate-quote expr)
  (cond ((ast-matches? expr ''_)
         (let ((value (ast-list-nth expr 1)))
           (at (get-location expr)
               (make-quote-node value))))
        ((ast-matches? expr '`_)
         (let ((value (ast-list-nth expr 1)))
           (at (get-location expr)
               (make-quasiquote-node value))))
        ((ast-matches? expr ',_)
         (let ((value (ast-list-nth expr 1)))
           (at (get-location expr)
               (make-unquote-node value))))
        ((ast-matches? expr ',@_)
         (let ((value (ast-list-nth expr 1)))
           (at (get-location expr)
               (make-unquote-splicing-node value))))
        (else
         (let* ((node (ast-list-nth expr 0)))
           (raise-compilation-error
            (get-location node)
            (format "Bad `~a` syntax, expected exactly 1 expression to follow:"
                    (ast-get node 'value)))))))
