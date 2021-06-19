#lang racket

;; Delimited continuations:

(require "scheduler.rkt")
(require "processes.rkt")

(provide &push-delimited-continuation! &pop-delimited-continuation!)

(define (&push-delimited-continuation! cont)
  (set-uproc-delimited-continuations! (current-task)
                                      (cons cont (uproc-delimited-continuations (current-task)))))

(define (&pop-delimited-continuation!)
  (let* ((stack (uproc-delimited-continuations (current-task)))
         (top (car stack)))
    (set-uproc-delimited-continuations! (current-task) (cdr stack))
    top))
