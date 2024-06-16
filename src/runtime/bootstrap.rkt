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
         __nil __true __false __yield
         __list
         __sleep __self __send __spawn __recv __task_info __monitor
         __assertBANG __signalBANG __retractBANG __select __notify_whenever
         __display __newline __random __debug
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

;; Built-in primops
(define nil? null?)

;; List
(define __list (bootstrap list))

;; Actor model:
(define __self (bootstrap self))
(define __send (bootstrap send))
(define __spawn (bootstrap spawn))

(define __sleep (make-closure
                 '()
                 (lambda (_ t cont)
                   (sleep t)
                   (make-resumable cont t))))

(define __recv (make-closure
                '()
                (lambda (_ cont)
                  (let ((r (recv)))
                    (if (car r)
                        ;; If a message is received, return the message.
                        (make-resumable cont (cdr r))
                        ;; Else, setup a continuation that attempts to re-fetch the message.
                        (make-resumable
                         (make-closure
                          cont
                          (lambda (cont v)
                            (apply-closure __recv cont)))
                         '()))))))

(define __task_info (bootstrap task-info))
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
(define __display (bootstrap display))
(define __newline (bootstrap newline))
(define __random (bootstrap random))
(define __debug (bootstrap (lambda args
                             (pretty-print args)
                             (newline))))
