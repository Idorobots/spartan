;; A crude quasiquote expander.

(load "compiler/utils/utils.scm")

(load "compiler/env.scm")
(load "compiler/pass.scm")
(load "compiler/ast.scm")
(load "compiler/errors.scm")

(define quasiquote-expand
  (pass (schema 'errors a-list?
                'ast (ast-subset? '(quote quasiquote unquote unquote-splicing
                                    number symbol string list
                                    if do let letrec binding lambda app
                                    primop-app <error> <location>)))
        (lambda (env)
          (let ((result (collect-errors (env-get env 'errors)
                                        (lambda ()
                                          (expand-quasiquote (env-get env 'ast))))))
            (env-set env
                     'ast (car result)
                     'errors (cadr result))))))

(define (expand-quasiquote expr)
  (case (get-type expr)
    ((quote) expr)
    ((quasiquote)
     (expand-no-splicing (ast-quoted-expr expr)
                         expr))
    ((unquote unquote-splicing)
     (raise-compilation-error
      expr
      (format "Misplaced `~a`, expected to be enclosed within a `quasiquote`:" (get-type expr))))
    (else
     (walk-ast expand-quasiquote expr))))

(define (expand-no-splicing expr context)
  (if (unquote-splicing-node? expr)
      (raise-compilation-error
       expr
       "Misplaced `unquote-splicing`, expected to be enclosed within a spliceable value:")
      (expand-splicing expr context)))

(define (expand-splicing expr context)
  (case (get-type expr)
    ((quote number string) expr)
    ((quasiquote) (expand-quasiquote expr))
    ((unquote unquote-splicing) (ast-quoted-expr expr))
    ((symbol) (reconstruct-quoted-value
               expr
               context))
    ((list) (reconstruct-quoted-list
             (ast-list-values expr)
             expr))
    (else (compiler-bug "Unexpected quasiquote expansion expression:" expr))))

(define (reconstruct-quoted-value expr context)
  (at (get-location context)
      (generated
       (make-quote-node
        expr))))

(define (reconstruct-quoted-list exprs context)
  (if (empty? exprs)
      (reconstruct-quoted-value
       (at (get-location context)
           (generated
            (make-list-node '())))
       context)
      ((if (unquote-splicing-node? (car exprs))
           make-concat
           make-cons)
       (expand-splicing (car exprs) (car exprs))
       (reconstruct-quoted-list (cdr exprs) context)
       context)))

(define (make-concat a b context)
  (at (get-location context)
      (generated
       (make-app-node
        (at (get-location context)
            (generated (make-symbol-node 'concat)))
        (list a b)))))

(define (make-cons a b context)
  (at (get-location context)
      (generated
       (make-app-node
        (at (get-location context)
            (generated (make-symbol-node 'cons)))
        (list a b)))))
