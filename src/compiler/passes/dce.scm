;; Dead code ellimination.

(load-once "compiler/utils/utils.scm")

(load-once "compiler/env.scm")
(load-once "compiler/pass.scm")
(load-once "compiler/ast.scm")

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
              ((do . ,exprs)
               (let ((final (last exprs))
                     (filtered (filter effectful?
                                       (take exprs (- (length exprs) 1)))))
                 (if (empty? filtered)
                     final
                     (replace expr
                              (make-do-node
                               (append filtered
                                       (list final)))))))
              ((if ,condition ,then ,else)
               (cond ((falsy? condition) else)
                     ((truthy? condition) then)
                     (else expr)))
              (else
               expr)))
           expr))

(define (effectful? node)
  (not (or (const-node? node)
           (symbol-node? node)
           (lambda-node? node))))

(define (falsy? node)
  ;; FIXME Implement proper booleans.
  (and (const-node? node)
       (list-node? (ast-const-value node))
       (equal? 0 (ast-list-length (ast-const-value node)))))

(define (truthy? node)
  (and (not (falsy? node))
       (or (const-node? node)
           (lambda-node? node))))
