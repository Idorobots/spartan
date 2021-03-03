;; Parse tree linting.

(load "compiler/utils.scm")
(load "compiler/errors.scm")
(load "compiler/tree-ast.scm")

(define (validate expr input)
  (let* ((errors (ref '()))
         (result (with-handlers
                     ((syntax-error?
                       (lambda (error)
                         (let* ((location (syntax-error-location error))
                               (what (syntax-error-what error))
                               (restart (syntax-error-restart error))
                               (error (format-error input location what)))
                           (display error)
                           (newline)
                           (assign! errors (cons error (deref errors)))
                           ;; NOTE Continue analysis with a special "error" object.
                           (restart (at location
                                        (generated
                                         (make-error-node))))))))
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
                            expr))))
    (if (empty? (deref errors))
        result
        ;; FIXME Properly stop the compiler pipeline.
        (make-error-node))))
