;; Parse tree linting.

(load "compiler/utils.scm")
(load "compiler/env.scm")
(load "compiler/errors.scm")
(load "compiler/tree-ast.scm")

(define (elaborate-syntax env)
  (let* ((errors (ref '()))
         (result (with-handlers
                     ((syntax-error?
                       (lambda (error)
                         (push! errors error)
                         ;; NOTE Continue analysis with a special "error" object.
                         ((syntax-error-restart error)
                          (at (syntax-error-location error)
                              (generated
                               (make-error-node)))))))
                   (map-ast (compose expand-quote expand-structure-refs)
                            validate-parse-tree
                            (env-get env 'ast)))))
    (env-set env
             'ast result
             'errors (deref errors))))

(define (validate-parse-tree expr)
  (case (ast-get expr 'type compiler-bug)
    ('invalid-symbol (raise-syntax-error
                      (get-location expr)
                      (format "Invalid symbol `~a` specified at:"
                              (ast-get expr 'value compiler-bug))))
    ('unmatched-token (raise-syntax-error
                       (get-location expr)
                       "Unmatched parentheses - expected an opening `(` to come before:"))
    ('unterminated-list (raise-syntax-error
                         (get-location expr)
                         "Unterminated list - expected a closing `)` to follow:"))
    ('unterminated-string (raise-syntax-error
                           (get-location expr)
                           "Unterminated string literal - expected a closing `\"` to follow:"))
    ('unterminated-quote (raise-syntax-error
                          (get-location expr)
                          (format "No expression following `~a`:"
                                  (ast-get expr 'value compiler-bug))))
    (else expr)))

(define (expand-quote expr)
  (case (ast-get expr 'type compiler-bug)
    ('plain-quote (tag-contents 'quote expr))
    ('quasiquote (tag-contents 'quasiquote expr))
    ('unquote (tag-contents 'unquote expr))
    ('unquote-splicing (tag-contents 'unquote-splicing expr))
    (else expr)))

(define (tag-contents tag expr)
  (let ((loc (get-location expr)))
    (at loc
        (generated
         (make-list-node
          (list (at loc
                    (generated (make-symbol-node tag)))
                (ast-get expr 'value compiler-bug)))))))

(define (expand-structure-refs expr)
  (case (ast-get expr 'type compiler-bug)
    ('structure-ref (let ((parts (ast-get expr 'value compiler-bug))
                          (loc (get-location expr)))
                      (foldl (lambda (part acc)
                               (at loc
                                   (generated
                                    ;; FIXME This ought to be a primop application instead.
                                    (make-list-node
                                     (list (generated (wrap-symbol loc '&structure-ref))
                                           acc
                                           (at loc
                                               (generated
                                                (make-quote-node part))))))))
                             (wrap-symbol loc (car parts))
                             (map (partial wrap-symbol loc)
                                  (cdr parts)))))
    (else expr)))

(define (wrap-symbol loc s)
  (at loc
      (make-symbol-node s)))
