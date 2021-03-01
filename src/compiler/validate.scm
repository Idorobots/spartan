;; Parse tree linting.

(load "compiler/utils.scm")
(load "compiler/errors.scm")
(load "compiler/tree-ast.scm")

(define (validate expr input)
  (with-handlers
      ((syntax-error?
        (lambda (error)
          (let ((location (syntax-error-location error))
                (what (syntax-error-what error))
                (restart (syntax-error-restart error)))
            (display (format-error input location what))
            (newline)
            ;; NOTE Continue analysis with a special "error" object.
            (restart (at location
                         (generated
                          (make-error-node))))))))
    (map-ast id
             (lambda (expr)
               (case (ast-get expr 'type 'undefined)
                 ('unterminated-string (raise-syntax-error
                                        expr
                                        "Unterminated string literal - expected a closing `\"` to follow:"))
                 ('unterminated-list (raise-syntax-error
                                      expr
                                      "Unterminated list - expected a closing `)` to follow:"))
                 ('unterminated-quote (raise-syntax-error
                                       expr
                                       (format "No expression following `~a`:"
                                               (ast-get expr 'value compiler-bug))))
                 (else expr)))
             expr)))
