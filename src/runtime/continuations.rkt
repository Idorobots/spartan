#lang racket

;; Continuations:

(require "closures.rkt")
(require "processes.rkt")

(provide (struct-out resumable) resume can-resume?
         kont-counter reset-kont-counter! dec-kont-counter!
         ;; NOTE For easier testing
         cpsfy)

;; NOTE Used in compiler-generated code under &yield-cont primop.
(struct resumable (cont arg) #:transparent #:constructor-name make-resumable)

(define (resume c)
  (apply-closure (resumable-cont c) (resumable-arg c)))

(define (can-resume? thing)
  (closure? (resumable-cont thing)))

(define +kont-hops+ 200)
(define *kont-counter* +kont-hops+)

(define (kont-counter)
  *kont-counter*)

(define (reset-kont-counter!)
  (set! *kont-counter* +kont-hops+))

(define (dec-kont-counter!)
  (set! *kont-counter* (- *kont-counter* 1)))

(define (cpsfy f)
  (lambda args
    (make-resumable
     (last args)
     (apply f
            (take args
                  (- (length args) 1))))))
