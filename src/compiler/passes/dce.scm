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
              ((let ((binding (symbol ,var) ,val)) (symbol ,body))
               (if (equal? (ast-symbol-value var)
                           (ast-symbol-value body))
                   val
                   expr))
              ;; NOTE Eta reduction.
              ((lambda ,formals (app ,op . ,args))
               (if (and (equal? (length formals)
                                (length args))
                        (every? symbol-node? args)
                        (every? true?
                                (map (lambda (formal arg)
                                       (equal? (ast-symbol-value formal)
                                               (ast-symbol-value arg)))
                                     formals
                                     args)))
                   op
                   expr))
              (else
               expr)))
           expr))
