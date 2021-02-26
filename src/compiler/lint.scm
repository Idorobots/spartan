;; Parse tree linting.

(load "compiler/tree-ast.scm")

(define (ast->plain expr)
  (map-ast id
           (lambda (expr)
             (case (ast-get expr 'type 'undefined)
               ('plain-quote (list 'quote (ast-get expr 'value '())))
               ('quasiquote (list 'quasiquote (ast-get expr 'value '())))
               ('unquote (list 'unquote (ast-get expr 'value '())))
               ('unquote-splicing (list 'unquote-splicing (ast-get expr 'value '())))
               (else (ast-get expr 'value '()))))
           expr))

(define (validate expr)
  ;; FIXME Actually lint the code.
  (ast->plain expr))
