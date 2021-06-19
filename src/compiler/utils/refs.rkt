#lang racket

(provide ref deref assign! push! pop!)

;; Mutable references

(define (ref x)
  (make-vector 1 x))

(define (deref ref)
  (vector-ref ref 0))

(define (assign! ref x)
  (vector-set! ref 0 x))

(define (push! ref x)
  (assign! ref (cons x (deref ref))))

(define (pop! ref x)
  (let ((x (deref ref)))
    (assign! ref (cdr x))
    (car x)))
