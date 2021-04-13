;; Final frontend code validation.

(load "compiler/utils/set.scm")
(load "compiler/utils/utils.scm")

(load "compiler/env.scm")
(load "compiler/ast.scm")
(load "compiler/errors.scm")

(load "compiler/passes/freevars.scm") ;; FIXME This is only imported for get-fv.

(define (validate env)
  (let ((result (collect-errors (env-get env 'errors)
                                (lambda ()
                                  (let ((expr (env-get env 'ast))
                                        (globals (env-get env 'globals)))
                                    (validate-ast (get-undefined-vars expr globals)
                                                  (set)
                                                  expr))))))
    (env-set env
             'ast (car result)
             'errors (cadr result))))

(define (get-undefined-vars expr globals)
  (set-difference (get-fv expr)
                  (apply set globals)))

(define (validate-ast undefined unused expr)
  (case (get-type expr)
    ((quote) expr)
    ((lambda)
     (let* ((bound (get-bound-vars expr))
            (unused (set-difference bound
                                    (get-fv (ast-lambda-body expr)))))
       (ast-update (ast-update expr
                               'formals
                               (partial map
                                        (partial validate-ast
                                                 (set)
                                                 unused)))
                   'body
                   (partial validate-ast
                            (set-difference undefined bound)
                            (set)))))
    ((let)
     (let* ((bound (get-bound-vars expr))
            (unused (set-difference bound
                                    (get-fv (ast-let-body expr)))))
       (ast-update (ast-update expr
                               'bindings
                               (partial map (partial validate-ast undefined unused)))
                   'body
                   (partial validate-ast
                            (set-difference undefined bound)
                            (set)))))
    ((letrec)
     (let* ((bound (get-bound-vars expr))
            (without-bound (set-difference undefined bound))
            (unused (set-difference bound
                                    (set-union (get-fv (ast-letrec-body expr))
                                               (set-sum (map get-fv
                                                             (ast-letrec-bindings expr)))))))
       (ast-update (ast-update expr
                               'bindings
                               (partial map (partial validate-ast without-bound unused)))
                   'body
                   (partial validate-ast
                            without-bound
                            (set)))))
    ((binding)
     (ast-update (ast-update expr 'var (partial validate-ast (set) unused))
                 'val (partial validate-ast undefined (set))))
    ((symbol)
     (let ((value (ast-symbol-value expr)))
       (cond ((generated? expr) expr)
             ((set-member? undefined value)
              (raise-compilation-error
               expr
               (format "Undefined variable `~a`:" value)))
             ((equal? value '_) expr)
             ((set-member? unused value)
              (raise-compilation-error
               expr
               (format "Unused variable `~a`, rename to `_` to avoid this error:" value)))
             (else
              expr))))
    ((def)
     ;; NOTE So that we can find potential errors in misplaced defs.
     (replace (walk-ast (partial validate-ast undefined unused)
                        expr)
              (raise-compilation-error
               expr
               (format "~a, not allowed in this context:" (get-context* expr "Bad `define` syntax")))))
    (else
     (walk-ast (partial validate-ast undefined unused)
               expr))))
