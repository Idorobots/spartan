;; Constants annotation & quote ellimination.

(load-once "compiler/utils/utils.scm")

(load-once "compiler/env.scm")
(load-once "compiler/pass.scm")
(load-once "compiler/ast.scm")

(define annotate-constants
  (pass (schema "annotate-constants"
                'ast (ast-subset? '(quote quasiquote unquote unquote-splicing ;; NOTE These can still appear within quoted values.
                                    number symbol string list
                                    if do let letrec binding lambda app
                                    primop-app <error>)))
        (lambda (env)
          (env-update env 'ast wrap-constants))))

(define (wrap-constants expr)
  (case (ast-node-type expr)
    ((quote)
     (replace expr
              (make-const-node
               (plainify-quote (ast-quoted-expr expr)))))
   ((number string)
    (replace expr
             (make-const-node expr)))
   (else
    (walk-ast wrap-constants expr))))

(define (plainify-quote expr)
  (case (ast-node-type expr)
    ((quote quasiquote unquote unquote-splicing)
    ;; NOTE Within `quote` all the semantic AST nodes have to be dumbed down to plain old data.
     (replace expr
              (generated
               (make-list-node
                (list (at (get-location expr)
                          (generated
                           (make-symbol-node (ast-node-type expr))))
                      (ast-quoted-expr expr))))))
    (else
     (walk-ast plainify-quote expr))))
