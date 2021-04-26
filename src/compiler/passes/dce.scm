;; Dead code ellimination.

(load "compiler/utils/utils.scm")

(load "compiler/env.scm")
(load "compiler/pass.scm")
(load "compiler/ast.scm")

(define elliminate-dead-code
  (pass (schema "elliminate-dead-code"
                'ast (ast-subset? '(const symbol
                                    if do let letrec fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast dead-code-ellimination))))

(define (dead-code-ellimination expr)
  (map-ast id
           (lambda (expr)
             (ast-case expr
              ;; NOTE These are introduced by CPC.
              ((let ((binding ,var ,val)) ,var)
               val)
              ;; NOTE Eta reduction.
              ((lambda ,args (app ,op . ,args))
               op)
              (else
               expr)))
           expr))
