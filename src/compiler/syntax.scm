;; Syntax expansion module.

(load "compiler/qq.scm")
(load "compiler/ast.scm")
(load "compiler/utils.scm")

(define (syntax-expand expr)
  (walk (lambda (expr)
          (cond ((value-define? expr)
                 expr)

                ((define? expr)
                 (make-val-define (define-name expr)
                              (make-lambda (define-args expr)
                                           (make-do (define-body* expr)))))

                ('else
                 expr)))
        id
        expr))
