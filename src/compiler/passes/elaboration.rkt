#lang racket

;; Semantic elaboration.
;; This phase checks syntax form correctness - if different syntax forms are used correctly, reserved keywords are used in the right positions, etc.

(require "../utils/utils.rkt")
(require "../expander/elaborators.rkt")
(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require "../errors.rkt")

(provide elaborate
         ;; FIXME Used by some other passes.
         unique-bindings valid-bindings valid-symbol
         ;; FIXME For test access.
         elaborate-unquoted +reserved-keywords+)

(define elaborate
  (pass (schema "elaborate"
                'errors a-list?
                'ast (ast-subset? '(quote quasiquote unquote unquote-splicing
                                    number symbol string list
                                    if let letrec binding lambda app def
                                    primop-app body <error>)))
        (lambda (env)
          (let ((result (collect-errors (env-get env 'errors)
                                        (lambda ()
                                          (elaborate-unquoted (env-get env 'ast))))))
            (env-set env
                     'ast (car result)
                     'errors (cadr result))))))

(define (elaborate-unquoted expr)
  (case (ast-node-type expr)
    ((<error> quote number symbol string)
     expr)
    ((do)
     (set-ast-do-exprs expr (map elaborate-unquoted (ast-do-exprs expr))))
    ((body)
     (set-ast-body-exprs expr (map elaborate-unquoted (ast-body-exprs expr))))
    ((if)
     (-> expr
         (set-ast-if-condition (elaborate-unquoted (ast-if-condition expr)))
         (set-ast-if-then (elaborate-unquoted (ast-if-then expr)))
         (set-ast-if-else (elaborate-unquoted (ast-if-else expr)))))
    ((lambda)
     (set-ast-lambda-body expr (elaborate-unquoted (ast-lambda-body expr))))
    ((let)
     (-> expr
         (set-ast-let-body (elaborate-unquoted (ast-let-body expr)))
         (set-ast-let-bindings (map elaborate-unquoted (ast-let-bindings expr)))))
    ((letrec)
     (-> expr
         (set-ast-letrec-body (elaborate-unquoted (ast-letrec-body expr)))
         (set-ast-letrec-bindings (map elaborate-unquoted (ast-letrec-bindings expr)))))
    ((binding)
     (-> expr
         (set-ast-binding-var (elaborate-unquoted (ast-binding-var expr)))
         (set-ast-binding-val (elaborate-unquoted (ast-binding-val expr)))))
    ((def)
     (set-ast-def-value expr (elaborate-unquoted (ast-def-value expr))))
    ((quasiquote)
     (set-ast-quasiquote-expr expr (elaborate-quoted (ast-quasiquote-expr expr))))
    ((unquote)
     (set-ast-unquote-expr expr (elaborate-unquoted (ast-unquote-expr expr))))
    ((unquote-splicing)
     (set-ast-unquote-splicing-expr expr (elaborate-unquoted (ast-unquote-splicing-expr expr))))
    ((primop-app)
     (set-ast-primop-app-args expr (map elaborate-unquoted (ast-primop-app-args expr))))
    ((app)
     (elaborate-app expr))
    ((list)
     (elaborate-unquoted
      (reconstruct-syntax-forms expr)))
    (else (compiler-bug "Unrecognized expression passed to elaborate-unquoted:" expr))))

(define (elaborate-quoted expr)
  ;; NOTE We don't want the value within quasiquote to be elaborated.
  (case (ast-node-type expr)
    ((<error> quote number symbol string)
     expr)
    ((unquote)
     (set-ast-unquote-expr expr (elaborate-unquoted (ast-unquote-expr expr))))
    ((unquote-splicing)
     (set-ast-unquote-splicing-expr expr (elaborate-unquoted (ast-unquote-splicing-expr expr))))
    ((list)
     (let ((values (ast-list-values expr)))
       (if (and (not (empty? values))
                (is-type? (car values) 'symbol))
           (elaborate-quoted ((if (equal? (ast-symbol-value (car values))
                                          'unquote-splicing)
                                  reconstruct-unquote-splicing
                                  reconstruct-unquote)
                              expr
                              #f
                              #f))
           (set-ast-list-values expr (map elaborate-quoted values)))))
    (else (compiler-bug "Unrecognized expression passed to elaborate-quoted:" expr))))

(define (elaborate-app expr)
  (-> expr
      (set-ast-app-op (elaborate-unquoted (ast-app-op expr)))
      (set-ast-app-args (map elaborate-unquoted (ast-app-args expr)))))

(define (reconstruct-syntax-forms expr)
  (let ((values (ast-list-values expr)))
    (if (and (not (empty? values))
             (is-type? (car values) 'symbol))
        (case (ast-symbol-value (car values))
          ((if)
           (reconstruct-if expr #f #f))
          ((do)
           (reconstruct-do expr #f #f))
          ((lambda)
           (reconstruct-lambda expr #f #f))
          ((let)
           (reconstruct-let expr #f #f))
          ((letrec)
           (reconstruct-letrec expr #f #f))
          ((quote)
           (reconstruct-quote expr #f #f))
          ((quasiquote)
           (reconstruct-quasiquote expr #f #f))
          ((unquote)
           (reconstruct-unquote expr #f #f))
          ((unquote-splicing)
           (reconstruct-unquote-splicing expr #f #f))
          ((define)
           (reconstruct-def expr #f #f))
          (else
           (reconstruct-app expr #f #f)))
        (reconstruct-app expr #f #f))))
