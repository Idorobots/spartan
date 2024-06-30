#lang racket

;; The bootstrap code.

(require "../rete/rete.rkt")
(require "continuations.rkt")
(require "closures.rkt")
(require "processes.rkt")

(provide delay-milliseconds suspend trampoline __yield __list whenever-trampoline)

;; Continuations bootstrap

;; Forms a new resumable continuation that runs a thunk.
(define (suspend thunk)
  (make-resumable
   (make-closure
    '()
    (lambda (_ thunk)
      (apply-closure thunk
                     (make-closure
                      '()
                      (lambda (_ v)
                        ;; NOTE Suspends with a top-level continuation.
                        v)))))
   thunk))

;; Executes a resumable until exhaustion.
(define (trampoline resumable)
  (if (resumable? resumable)
      (trampoline (resume resumable))
      resumable))

;; Replaces the current continuation with a new one.
(define __yield
  (make-closure
   '()
   (lambda (_ cont v ignored)
     (make-resumable cont v))))

;; List
;; FIXME There's currently no vararg function support, so this can't be implemented in Spartan.
(define __list
  (make-closure
   '()
   (lambda args
     (make-resumable
      (last args)
      (take (cdr args)
            ;; NOTE Args minus env and cont.
            (- (length args) 2))))))

;; RBS
(define (whenever-trampoline pattern fun)
  (whenever pattern
            (lambda (b)
              ;; FIXME This can "steal" the continuation and not play nicely with the scheduler.
              (trampoline
               (suspend
                (make-closure
                 '()
                 (lambda (e cont)
                   (apply-closure fun b cont))))))))

;; Other stuff

(define (delay-milliseconds ms)
  (sleep (/ ms 1000.0)))
