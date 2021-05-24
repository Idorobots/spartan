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
                'globals a-set?
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
                                                     (set)
                                                     expr))))))
       (env-set env
                'ast (car result)
                'errors (cadr result))))))

(define (get-undefined-vars expr globals)
  (set-difference (get-free-vars expr) globals))

(define (validate-ast undefined unused used-before-def expr)
  (ast-case expr
    ((const _)
     ;; TODO Validate number ranges, string escape sequences, unicode well-formedness etc.
     expr)
    ((lambda _ ,body)
     (let* ((bound (get-bound-vars expr))
            (body-fv (get-free-vars body))
            (unused (set-difference bound body-fv))
            (before-def (set-difference used-before-def body-fv)))
       (ast-update (ast-update expr
                               'formals
                               (partial map
                                        (partial validate-ast (set) unused before-def)))
                   'body
                   (partial validate-ast
                            (set-difference undefined bound)
                            (set)
                            before-def))))
    ((let _ ,body)
     (let* ((bound (get-bound-vars expr))
            (unused (set-difference bound (get-free-vars body))))
       (ast-update (ast-update expr
                               'bindings
                               (partial map
                                        (partial validate-ast undefined unused used-before-def)))
                   'body
                   (partial validate-ast
                            (set-difference undefined bound)
                            (set)
                            used-before-def))))
    ((letrec ,bindings ,body)
     (let* ((bound (get-bound-vars expr))
            (without-bound (set-difference undefined bound))
            (unused (set-difference bound
                                    (set-union (get-free-vars body)
                                               (set-sum (map get-free-vars bindings))))))
       (ast-update (ast-update expr
                               'bindings
                               (partial validate-use-before-definition bound without-bound unused used-before-def))
                   'body
                   (partial validate-ast
                            without-bound
                            (set)
                            used-before-def))))
    ((binding _ _)
     (ast-update (ast-update expr 'var (partial validate-ast (set) unused (set-difference used-before-def
                                                                                          (get-bound-vars expr))))
                 'val (partial validate-ast undefined (set) used-before-def)))
    ((symbol '_)
     expr)
    ((symbol _)
     (let ((value (ast-symbol-value expr)))
       (cond ((generated? expr) expr)
             ((set-member? undefined value)
              (raise-compilation-error
               expr
               (format "Undefined variable `~a`:" value)))
             ((set-member? used-before-def value)
              (raise-compilation-error
               expr
               (format "Variable `~a` used before its definition:" value)))
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
       (ast-update (ast-update expr 'name (partial validate-ast (set) unused used-before-def))
                   'value
                   (partial validate-ast
                            (set-difference undefined bound)
                            (set)
                            used-before-def))))
    (else
     (walk-ast (partial validate-ast undefined unused used-before-def)
               expr))))

(define (validate-use-before-definition bound undefined unused used-before-def bindings)
  ;; NOTE Simple values are considered seen since they will be extracted outward.
  (let loop ((seen (set-sum (map get-bound-vars
                                 (filter (compose (partial equal? 'simple) get-complexity)
                                         bindings))))
             (bs bindings))
    (if (empty? bs)
        '()
        (let ((b (car bs)))
          ;; NOTE Eager free vars are considered undefined unless defined in order of usage.
          (cons (validate-ast undefined
                              unused
                              (set-union used-before-def
                                         (set-difference (set-intersection (get-free-vars b)
                                                                           bound)
                                                         seen))
                              b)
                (loop (set-union seen (get-bound-vars b))
                      (cdr bs)))))))
