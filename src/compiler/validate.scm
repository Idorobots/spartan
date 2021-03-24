;; Final frontend code validation.

(load "compiler/utils/set.scm")
(load "compiler/utils/utils.scm")

(load "compiler/env.scm")
(load "compiler/tree-ast.scm")
(load "compiler/errors.scm")

(define (validate env)
  (let ((result (collect-errors (env-get env 'errors)
                                (lambda ()
                                  (let ((expr (env-get env 'ast))
                                        (globals (env-get env 'globals)))
                                    (validate-ast (get-undefined-vars expr globals)
                                                  expr))))))
    (env-set env
             'ast (car result)
             'errors (cadr result))))

(define (get-undefined-vars expr globals)
  (set-difference (get-free-vars expr)
                  (apply set globals)))

(define (validate-ast free-vars expr)
  (case (get-type expr)
    ((quote) expr)
    ((lambda)
     (ast-update expr 'body (partial validate-ast
                                     (set-difference free-vars
                                                     (get-bound-vars expr)))))
    ((let)
     (ast-update (ast-update expr
                             'bindings
                             (partial map
                                      (lambda (b)
                                        (cons (car b)
                                              (validate-ast free-vars
                                                            (cdr b))))))
                 'body
                 (partial validate-ast
                          (set-difference free-vars
                                          (get-bound-vars expr)))))
    ((letrec)
     (let ((bound (set-difference free-vars
                                  (get-bound-vars expr))))
       (ast-update (ast-update expr
                               'bindings
                               (partial map
                                        (lambda (b)
                                          (cons (car b)
                                                (validate-ast bound
                                                              (cdr b))))))
                   'body
                   (partial validate-ast bound))))
    ((symbol)
     (let ((value (ast-symbol-value expr)))
       (if (and (not (generated? expr))
                (set-member? free-vars value))
           (raise-compilation-error
            (get-location expr)
            (format "Undefined variable `~a`:" value))
           expr)))
    ((def)
     (raise-compilation-error
      (get-location expr)
      (format "~a, not allowed in this context:" (get-context* expr "Bad `define` syntax"))))
    (else
     (walk-ast (partial validate-ast free-vars)
               expr))))
