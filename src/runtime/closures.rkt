#lang racket

;; Runtime closure creation.

(provide (struct-out closure) make-env env-ref set-env! apply-closure)

(struct closure ((env #:mutable) fun) #:transparent #:constructor-name make-closure)

(define-syntax-rule (make-env vals ...)
  (vector vals ...))

(define-syntax-rule (env-ref e offset)
  (vector-ref e offset))

(define-syntax-rule (set-env! e offset val)
  (vector-set! e offset val))

(define-syntax-rule (apply-closure c args ...)
  (let ((clo c))
    ((closure-fun clo) (closure-env clo) args ...)))
