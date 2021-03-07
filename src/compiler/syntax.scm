;; Syntax expansion module.

(load "compiler/qq.scm")
(load "compiler/ast.scm")
(load "compiler/utils.scm")

(define (syntax-expand expr)
  (walk (lambda (expr)
          (cond ((lambda? expr)
                 (make-lambda (lambda-args expr)
                              (make-do (lambda-body* expr))))

                ((value-define? expr)
                 expr)

                ((define? expr)
                 (make-val-define (define-name expr)
                              (make-lambda (define-args expr)
                                           (make-do (define-body* expr)))))

                ((let? expr)
                 (make-let (let-bindings expr)
                           (make-do (let-body* expr))))

                ((letrec? expr)
                 (make-letrec (letrec-bindings expr)
                              (make-do (let-body* expr))))

                ((letcc? expr)
                 (make-letcc (letcc-var expr)
                             (make-do (let-body* expr))))

                ('else
                 expr)))
        id
        expr))
