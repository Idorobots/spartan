;; Parse tree linting.

(load "compiler/utils.scm")
(load "compiler/env.scm")
(load "compiler/errors.scm")
(load "compiler/tree-ast.scm")

(define (validate env)
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
                   (validate-correct-parse (env-get env 'ast)))))
    (env-set env
             'ast
             (if (empty? (deref errors))
                 result
                 (begin
                   (map (partial report-error env)
                        (sort (deref errors)
                              (lambda (a b)
                                (location<? (syntax-error-location a)
                                            (syntax-error-location b)))))
                   ;; FIXME Properly stop the compiler pipeline.
                   (make-error-node))))))

(define (validate-correct-parse expr)
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

(define (report-error env error)
  (let* ((location (syntax-error-location error))
         (what (syntax-error-what error)))
    (display (format-error (env-get env 'module)
                           (env-get env 'input)
                           location
                           what))
    (newline)))
