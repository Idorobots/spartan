#lang racket

;; Actor model runtime stuff:

(require "continuations.rkt")
(require "closures.rkt")
(require "processes.rkt")
(require "scheduler.rkt")
(require "closures.rkt")

(provide send spawn)

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
