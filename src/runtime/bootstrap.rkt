#lang racket

;; The bootstrap code.

(require "../rete/rete.rkt")
(require "continuations.rkt")
(require "closures.rkt")

(provide notify-whenever
         __nil __true __false __yield __recur
         __list
         ;; FIXME For test access.
         bootstrap)

(define (bootstrap f)
  (make-closure
   '()
   (lambda args
     (make-resumable
      (last args)
      (apply f
             (take (cdr args)
                   ;; NOTE Args minus env and cont.
                   (- (length args) 2)))))))

;; Built-in values
(define __nil '())
(define __true #t)
(define __false #f)

(define __yield
  (make-closure
   '()
   (lambda (_ cont v ignored)
     (make-resumable cont v))))

(define __recur
  (make-closure
   '()
   (lambda (_ fun cont)
     (make-resumable (make-closure
                      (cons fun cont)
                      (lambda (fun/cont _)
                        (apply-closure (car fun/cont) (cdr fun/cont))))
                     '()))))

;; List
;; FIXME There's currently no vararg function support, so this can't be implemented in Spartan.
(define __list (bootstrap list))

;; RBS bootstrap:
(define (notify-whenever who pattern)
  (whenever pattern
            ;; FIXME We can't use Spartan functions, since they yield execution.
            (lambda (b)
              (send who b))))
