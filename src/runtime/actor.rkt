#lang racket

;; Actor model runtime stuff:

(require "continuations.rkt")
(require "closures.rkt")
(require "processes.rkt")
(require "scheduler.rkt")
(require "closures.rkt")

(provide sleep self send recv spawn)

(define (sleep time)
  (inc-uproc-rtime! (current-task)
                    time)
  time)

(define (self)
  (uproc-pid (current-task)))

(define (send pid msg)
  (let ((t (find-task pid)))
    ;; FIXME Throw exception when pid isn't found.
    (uproc-enqueue-msg! t msg)
    (when (equal? (uproc-state t) 'waiting-4-msg)
      (set-uproc-rtime! t (current-milliseconds))
      (enqueue-task! t))
    pid))

(define (recv)
  (let ((p (current-task)))
    (if (uproc-msg-queue-empty? p)
        (begin (set-uproc-state! p 'waiting-4-msg)
               (cons #f '()))
        (cons #t (uproc-dequeue-msg! p)))))

(define (spawn fun)
  (let ((kont (make-closure
               '()
               (lambda (e v)
                 (set-uproc-state! (current-task)
                                   'halted)
                 v))))
    (spawn-task! (make-resumable
                  (make-closure
                   '()
                   (lambda (e _)
                     (apply-closure fun kont)))
                  '())
                 (make-closure
                  '()
                  (lambda (e err _)
                    (display ";; Task finished due to unhandled error: ")
                    (display err)
                    (newline)
                    (apply-closure kont err))))))
