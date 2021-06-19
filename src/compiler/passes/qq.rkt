#lang racket

;; A crude quasiquote expander.

(require "../utils/utils.rkt")

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require "../errors.rkt")

(provide quasiquote-expand
         ;; FIXME For test access.
         expand-quasiquote)

(define quasiquote-expand
  (pass (schema "quasiquote-expand"
                'errors a-list?
                'ast (ast-subset? '(quote quasiquote unquote unquote-splicing
                                    number symbol string list
                                    if do let letrec binding lambda app
                                    primop-app <error>)))
        (lambda (env)
          (let ((result (collect-errors (env-get env 'errors)
                                        (lambda ()
                                          (expand-quasiquote (env-get env 'ast))))))
            (env-set env
                     'ast (car result)
                     'errors (cadr result))))))

(define (expand-quasiquote expr)
  (case (ast-node-type expr)
    ((quote) expr)
    ((quasiquote)
     (expand-no-splicing (ast-quasiquote-expr expr)
                         expr))
    ((unquote unquote-splicing)
     (raise-compilation-error
      expr
      (format "Misplaced `~a`, expected to be enclosed within a `quasiquote`:" (ast-node-type expr))))
    (else
     (walk-ast expand-quasiquote expr))))

(define (expand-no-splicing expr context)
  (if (ast-unquote-splicing? expr)
      (raise-compilation-error
       expr
       "Misplaced `unquote-splicing`, expected to be enclosed within a spliceable value:")
      (expand-splicing expr context)))

(define (expand-splicing expr context)
  (case (ast-node-type expr)
    ((quote number string) expr)
    ((quasiquote) (expand-quasiquote expr))
    ((unquote) (ast-unquote-expr expr))
    ((unquote-splicing) (ast-unquote-splicing-expr expr))
    ((symbol) (reconstruct-quoted-value
               expr
               context))
    ((list) (reconstruct-quoted-list
             (ast-list-values expr)
             expr))
    (else (compiler-bug "Unexpected quasiquote expansion expression:" expr))))

(define (reconstruct-quoted-value expr context)
  (generated
   (make-ast-quote (ast-node-location context)
                   expr)))

(define (reconstruct-quoted-list exprs context)
  (if (empty? exprs)
      (reconstruct-quoted-value
       (generated
        (make-ast-list (ast-node-location context)
                       '()))
       context)
      ((if (ast-unquote-splicing? (car exprs))
           make-concat
           make-cons)
       (expand-splicing (car exprs) (car exprs))
       (reconstruct-quoted-list (cdr exprs) context)
       context)))

(define (make-concat a b context)
  (make-ast-primop-app (ast-node-location context)
                       'concat
                       (list a b)))

(define (make-cons a b context)
  (make-ast-primop-app (ast-node-location context)
                       'cons
                       (list a b)))
