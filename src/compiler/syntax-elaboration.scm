;; Syntax elaboration.
;; This phase checks parse tree well-formedness - if all lists and strings are terminated properly, quote syntax is properly formatted, etc. It doesn't check if the parsed tree makes sense yet.

(load "compiler/utils.scm")
(load "compiler/env.scm")
(load "compiler/errors.scm")
(load "compiler/tree-ast.scm")

(define (elaborate-syntax env)
  (let ((result (collect-errors (env-get env 'errors)
                                (lambda ()
                                  (map-ast expand-structure-refs
                                           validate-parse-tree
                                           (env-get env 'ast))))))
    (env-set env
             'ast (car result)
             'errors (cadr result))))

(define (validate-parse-tree expr)
  (case (get-type expr)
    ('invalid-symbol (raise-compilation-error
                      (get-location expr)
                      (format "Invalid symbol `~a` specified at:"
                              (ast-get expr 'value compiler-bug))))
    ('unmatched-token (raise-compilation-error
                       (get-location expr)
                       "Unmatched parentheses - expected an opening `(` to come before:"))
    ('unterminated-list (raise-compilation-error
                         (get-location expr)
                         "Unterminated list - expected a closing `)` to follow:"))
    ('unterminated-string (raise-compilation-error
                           (get-location expr)
                           "Unterminated string literal - expected a closing `\"` to follow:"))
    ('unterminated-quote (raise-compilation-error
                          (get-location expr)
                          (format "No expression following `~a`:"
                                  (ast-get expr 'value compiler-bug))))
    (else expr)))

(define (expand-structure-refs expr)
  (case (get-type expr)
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
