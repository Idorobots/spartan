#lang racket

;; Actor model runtime stuff:

(require "continuations.rkt")
(require "closures.rkt")
(require "processes.rkt")
(require "scheduler.rkt")
(require "closures.rkt")

(provide send spawn)

(define (send pid msg)
  (let ((t (find-task pid)))
    ;; FIXME Throw exception when pid isn't found.
    (uproc-enqueue-msg! t msg)
    (when (equal? (uproc-state t) 'waiting-4-msg)
      (set-uproc-rtime! t (current-milliseconds))
      (enqueue-task! t))
    pid))

(define (spawn fun)
  (let ((kont (make-closure
               '()
               (lambda (_ v)
                 (set-uproc-state! (current-task)
                                   'halted)
                 v))))
    (spawn-task! (make-resumable
                  (make-closure
                   kont
                   (lambda (kont fun)
                     (apply-closure fun kont)))
                  fun)
                 (make-closure
                  kont
                  (lambda (kont err restart _)
                    (display ";; Task finished due to unhandled error: ")
                    (display err)
                    (newline)
                    (apply-closure kont err))))))
