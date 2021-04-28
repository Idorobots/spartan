;; Final frontend code validation.

(load-once "compiler/utils/set.scm")
(load-once "compiler/utils/utils.scm")

(load-once "compiler/env.scm")
(load-once "compiler/pass.scm")
(load-once "compiler/ast.scm")
(load-once "compiler/errors.scm")

(define validate
  (pass (schema "validate"
                'errors a-list?
                'ast (ast-subset? '(const symbol
                                    if do let letrec binding lambda app
                                    primop-app <error>)))
   (lambda (env)
     (let ((result (collect-errors (env-get env 'errors)
                                   (lambda ()
                                     (let ((expr (env-get env 'ast))
                                           (globals (env-get env 'globals)))
                                       (validate-ast (get-undefined-vars expr globals)
                                                     (set)
                                                     expr))))))
       (env-set env
                'ast (car result)
                'errors (cadr result))))))

(define (get-undefined-vars expr globals)
  (set-difference (get-free-vars expr)
                  (apply set globals)))

(define (validate-ast undefined unused expr)
  (ast-case expr
    ((const _)
     ;; TODO Validate number ranges, string escape sequences, unicode well-formedness etc.
     expr)
    ((lambda _ ,body)
     (let* ((bound (get-bound-vars expr))
            (unused (set-difference bound (get-free-vars body))))
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
    ((let _ ,body)
     (let* ((bound (get-bound-vars expr))
            (unused (set-difference bound (get-free-vars body))))
       (ast-update (ast-update expr
                               'bindings
                               (partial map (partial validate-ast undefined unused)))
                   'body
                   (partial validate-ast
                            (set-difference undefined bound)
                            (set)))))
    ((letrec ,bindings ,body)
     (let* ((bound (get-bound-vars expr))
            (without-bound (set-difference undefined bound))
            (unused (set-difference bound
                                    (set-union (get-free-vars body)
                                               (set-sum (map get-free-vars bindings))))))
       (ast-update (ast-update expr
                               'bindings
                               (partial map (partial validate-ast without-bound unused)))
                   'body
                   (partial validate-ast
                            without-bound
                            (set)))))
    ((binding _ _)
     (ast-update (ast-update expr 'var (partial validate-ast (set) unused))
                 'val (partial validate-ast undefined (set))))
    ((symbol '_)
     expr)
    ((symbol _)
     (let ((value (ast-symbol-value expr)))
       (cond ((generated? expr) expr)
             ((set-member? undefined value)
              (raise-compilation-error
               expr
               (format "Undefined variable `~a`:" value)))
             ((set-member? unused value)
              (raise-compilation-error
               expr
               (format "Unused variable `~a`, rename to `_` to avoid this error:" value)))
             (else
              expr))))
    ((def _ ,value)
     ;; NOTE This can still occur as a subnode of <error>, so we process it so that we can find more errors in validation.
     (let* ((bound (get-bound-vars expr))
            (unused (set-difference bound (get-free-vars value))))
       (ast-update (ast-update expr 'name (partial validate-ast (set) unused))
                   'value
                   (partial validate-ast
                            (set-difference undefined bound)
                            (set)))))
    (else
     (walk-ast (partial validate-ast undefined unused)
               expr))))
