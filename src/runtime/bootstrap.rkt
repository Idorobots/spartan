#lang racket

;; The bootstrap code.

(require "../compiler/utils/utils.rkt")
(require "../compiler/utils/refs.rkt")
(require "../rete/rete.rkt")
(require "continuations.rkt")
(require "delimited.rkt")
(require "closures.rkt")
(require "exceptions.rkt")
(require "actor.rkt")
(require "monitor.rkt")

(provide nil? notify-whenever
         __nil __true __false __not __car __cdr __cadr __cddr __list __cons __append __concat __equalQUEST __nilQUEST
         __MULT __PLUS ___ __DIV __zeroQUEST __modulo __quotient __remainder __EQUAL __LESS __LESSEQUAL __GREATER __GREATEREQUAL
         __ref __deref __assignBANG __callDIVcurrent_continuation __callDIVreset __callDIVshift __callDIVhandler __raise
         __sleep __self __send __spawn __recv __task_info __monitor
         __assertBANG __signalBANG __retractBANG __select __notify_whenever
         __display __newline __random __debug
         ;; FIXME For test access.
         bootstrap)

(define bootstrap (compose closurize cpsfy))

;; Built-in primops

(define nil? null?)

;; Built-in functions

(define __nil '())
(define __true #t)
(define __false #f)
(define __not (bootstrap not))

(define __car (bootstrap car))
(define __cadr (bootstrap car))
(define __cdr (bootstrap cdr))
(define __cddr (bootstrap cddr))

(define __list (bootstrap list))
(define __cons (bootstrap cons))
(define __append (bootstrap append))
(define __concat (bootstrap append))

(define __equalQUEST (bootstrap equal?))
(define __nilQUEST (bootstrap nil?))
(define __emptyQUEST (bootstrap empty?))

(define __MULT (bootstrap *))
(define __PLUS (bootstrap +))
(define ___ (bootstrap -))
(define __DIV (bootstrap /))
(define __zeroQUEST (bootstrap zero?))

(define __modulo (bootstrap modulo))
(define __quotient (bootstrap quotient))
(define __remainder (bootstrap remainder))

(define __EQUAL (bootstrap =))
(define __LESS (bootstrap <))
(define __LESSEQUAL (bootstrap <=))
(define __GREATER (bootstrap >))
(define __GREATEREQUAL (bootstrap >=))

(define __ref (bootstrap ref))
(define __deref (bootstrap deref))
(define __assignBANG (bootstrap assign!))

;; Continuations:
(define __callDIVcurrent_continuation (closurize
                                       (lambda (f cont)
                                         (&apply f (closurize
                                                    (lambda (v _)
                                                      (&apply cont v)))
                                                 cont))))

(define __callDIVreset (closurize
                        (lambda (f cont)
                          (&push-delimited-continuation! cont)
                          (&apply f
                                  (closurize
                                   (lambda (v)
                                     (&apply (&pop-delimited-continuation!)
                                             v)))))))

(define __callDIVshift (closurize
                        (lambda (f cont)
                          (&apply f
                                  (closurize
                                     (lambda (v ct2)
                                       (&push-delimited-continuation! ct2)
                                       (&apply cont v)))
                                  (closurize
                                   (lambda (v)
                                     (&apply (&pop-delimited-continuation!)
                                             v)))))))

;; Exceptions:
(define __callDIVhandler (closurize
                          (lambda (handler f cont)
                            (let* ((curr-handler (&error-handler))
                                   (new-handler (closurize
                                                 (lambda (error restart)
                                                   (&set-error-handler! curr-handler)
                                                   (&apply handler error restart cont)))))
                              (&set-error-handler! new-handler)
                              (&apply f
                                      (closurize
                                       (lambda (v)
                                         (&set-error-handler! curr-handler)
                                         (&apply cont v))))))))

(define __raise (closurize
                 (lambda (e cont)
                   (let ((curr-handler (&error-handler)))
                     (&apply curr-handler
                             e
                             (closurize
                              (lambda (v _)
                                (&set-error-handler! curr-handler)
                                (&apply cont v))))))))

;; Actor model:
(define __self (bootstrap self))
(define __send (bootstrap send))
(define __spawn (bootstrap spawn))

(define __sleep (closurize
                 (lambda (t cont)
                   (sleep t)
                   (&yield-cont cont t))))

(define __recv (closurize
                (lambda (cont)
                  (let ((r (recv)))
                    (if (car r)
                        ;; If a message is received, return the message.
                        (&yield-cont cont (cdr r))
                        ;; Else, setup a continuation that attempts to re-fetch the message.
                        (&yield-cont (closurize
                                      (lambda (_)
                                        (&apply __recv cont)))
                                     '()))))))

(define __task_info (bootstrap task-info))
(define __monitor (closurize
                   (lambda (timeout cont)
                     (task-info)
                     (sleep timeout)
                     (&yield-cont (closurize
                                   (lambda (_)
                                     (&apply __monitor timeout cont)))
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
