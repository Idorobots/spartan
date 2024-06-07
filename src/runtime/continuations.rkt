#lang racket

;; Continuations:

(require "../compiler/utils/utils.rkt")
(require "closures.rkt")
(require "processes.rkt")

(provide (struct-out resumable) &yield-cont resume can-resume?
         kont-counter reset-kont-counter! dec-kont-counter!
         cpsfy)

(struct resumable (cont arg) #:transparent)

;; NOTE Used in compiler-generated code not via bootstrap.
(define (&yield-cont cont hole)
  (resumable cont hole))

(define (resume thing)
  (&apply (resumable-cont thing) (resumable-arg thing)))

(define (can-resume? thing)
  (closure? (resumable-cont thing)))

(define (cpsfy f)
  (lambda args
    (&yield-cont (last args)
                 (apply f
                        (take args
                              (- (length args) 1))))))

(define +kont-hops+ 200)
(define *kont-counter* +kont-hops+)

(define (kont-counter)
  *kont-counter*)

(define (reset-kont-counter!)
  (set! *kont-counter* +kont-hops+))

(define (dec-kont-counter!)
  (set! *kont-counter* (- *kont-counter* 1)))
