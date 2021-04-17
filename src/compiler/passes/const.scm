;; Constants annotation & quote ellimination.

(load "compiler/utils/utils.scm")

(load "compiler/env.scm")
(load "compiler/pass.scm")
(load "compiler/ast.scm")

(define annotate-constants
  (pass (schema "annotate-constants"
                'ast (ast-subset? '(quote quasiquote unquote unquote-splicing ;; NOTE These can still appear within quoted values.
                                    number symbol string list
                                    if do let letrec binding lambda app
                                    primop-app <error>)))
        (lambda (env)
          (env-update env 'ast wrap-constants))))

(define (wrap-constants expr)
  (case (get-type expr)
   ((quote)
    (walk-ast plainify-quote expr))
   ((number string)
    (replace-with-quoted expr))
   (else
    (walk-ast wrap-constants expr))))

(define (plainify-quote expr)
  (case (get-type expr)
    ((quote quasiquote unquote unquote-splicing)
    ;; NOTE Within `quote` all the semantic AST nodes have to be dumbed down to plain old data.
     (replace expr
              (generated
               (make-list-node
                (list (at (get-location expr)
                          (generated
                           (make-symbol-node (get-type expr))))
                      (ast-quoted-expr expr))))))
    (else
     (walk-ast plainify-quote expr))))

(define (replace-with-quoted original)
  (replace original
           (generated
            (make-quote-node original))))
