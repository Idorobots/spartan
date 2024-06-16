#lang racket

;; The bootstrap code.

(require "../compiler/utils/utils.rkt")
(require "../compiler/utils/refs.rkt")
(require "../rete/rete.rkt")
(require "continuations.rkt")
(require "delimited.rkt")
(require "closures.rkt")
(require "actor.rkt")
(require "monitor.rkt")
(require "processes.rkt")
(require "scheduler.rkt")

(provide nil? notify-whenever
         __nil __true __false __yield __recur
         __list
         __assertBANG __signalBANG __retractBANG __select __notify_whenever
         __monitor
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

;; Built-in primops
(define nil? null?)

;; List
(define __list (bootstrap list))

;; RBS bootstrap:
(define __assertBANG (bootstrap assert!))
(define __signalBANG (bootstrap signal!))
(define __retractBANG (bootstrap retract!))
(define __select (bootstrap select))

(define (notify-whenever who pattern)
  (whenever pattern
            ;; FIXME We can't use Spartan functions, since they yield execution.
            (lambda (b)
              (send who b))))

(define __notify_whenever (bootstrap notify-whenever))

;; Misc:
(define __monitor (make-closure
                   '()
                   (lambda (_ timeout cont)
                     (task-info)
                     (sleep timeout)
                     (make-resumable
                      (make-closure
                       (cons timeout cont)
                       (lambda (timeout/cont v)
                         (apply-closure __monitor (car timeout/cont) (cdr timeout/cont))))
                      '()))))
