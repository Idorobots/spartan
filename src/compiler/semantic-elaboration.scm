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

        ((or (ast-matches? expr (cons 'quote '_))
             (ast-matches? expr (cons 'quasiquote '_))
             (ast-matches? expr (cons 'unquote '_))
             (ast-matches? expr (cons 'unquote-splicing '_)))
         (let* ((node (ast-list-nth expr 0)))
           (raise-compilation-error
            (get-location node)
            (format "Bad `~a` syntax, expected exactly 1 value to follow:"
                    (ast-get node 'value 'quote)))))

        (else
         expr)))
