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
                   (validate-parse-tree (env-get env 'ast)))))
    (env-set env
             'ast result
             'errors (deref errors))))

(define (validate-parse-tree expr)
  (map-ast id
           (lambda (expr)
             (case (ast-get expr 'type 'undefined)
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
           expr))
