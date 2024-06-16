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
         __nil __true __false __not __car __cdr __cadr __cddr __list __cons __append __concat __equalQUEST __nilQUEST
         __MULT __PLUS ___ __DIV __zeroQUEST __modulo __quotient __remainder __EQUAL __LESS __LESSEQUAL __GREATER __GREATEREQUAL
         __ref __deref __assignBANG __callDIVcurrent_continuation __callDIVreset __callDIVshift __callDIVhandler __raise
         __sleep __self __send __spawn __recv __task_info __monitor
         __assertBANG __signalBANG __retractBANG __select __notify_whenever
         __display __newline __random __debug
         __yield
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

;; Built-in functions
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
(define __callDIVcurrent_continuation (make-closure
                                       '()
                                       (lambda (_ f cont)
                                         (apply-closure
                                          f
                                          (make-closure
                                           cont
                                           (lambda (cont v _)
                                             (make-resumable cont v)))
                                          cont))))

(define __callDIVreset (make-closure
                        '()
                        (lambda (_ f cont)
                          (push-delimited-continuation! cont)
                          (apply-closure
                           f
                           (make-closure
                            '()
                            (lambda (_ v)
                              (make-resumable (pop-delimited-continuation!) v)))))))

(define __callDIVshift (make-closure
                        '()
                        (lambda (_ f cont)
                          (apply-closure
                           f
                           (make-closure
                            cont
                            (lambda (cont v ct2)
                              (push-delimited-continuation! ct2)
                              (make-resumable cont v)))
                           (make-closure
                            '()
                            (lambda (_ v)
                              (make-resumable (pop-delimited-continuation!) v)))))))

;; Exceptions:
(define __callDIVhandler (make-closure
                          '()
                          (lambda (_ handler f cont)
                            (let* ((curr-handler (uproc-error-handler (current-task)))
                                   (state (list handler cont curr-handler))
                                   (new-handler (make-closure
                                                 state
                                                 (lambda (handler/cont/curr-handler error restart _)
                                                   (set-uproc-error-handler! (current-task) (caddr handler/cont/curr-handler))
                                                   (apply-closure (car handler/cont/curr-handler)
                                                                  error
                                                                  restart
                                                                  (cadr handler/cont/curr-handler))))))
                              (set-uproc-error-handler! (current-task) new-handler)
                              (apply-closure
                               f
                               (make-closure
                                (cons cont curr-handler)
                                (lambda (cont/curr-handler v)
                                  (set-uproc-error-handler! (current-task) (cdr cont/curr-handler))
                                  (make-resumable (car cont/curr-handler) v))))))))

(define __raise (make-closure
                 '()
                 (lambda (_ err cont)
                   (let ((curr-handler (uproc-error-handler (current-task))))
                     (apply-closure
                      curr-handler
                      err
                      (make-closure
                       (cons cont curr-handler)
                       (lambda (cont/curr-handler v _)
                         (set-uproc-error-handler! (current-task) (cdr cont/curr-handler))
                         (make-resumable (car cont/curr-handler) v)))
                      cont)))))

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
