#lang racket

;; Delimited continuations:

(require "scheduler.rkt")
(require "processes.rkt")

(provide push-delimited-continuation! pop-delimited-continuation!)

(define-syntax-rule (push-delimited-continuation! cont)
  (let ((task (current-task)))
    (set-uproc-delimited-continuations! task
                                        (cons cont (uproc-delimited-continuations task)))))

(define-syntax-rule (pop-delimited-continuation!)
  (let* ((task (current-task))
         (stack (uproc-delimited-continuations task)))
    (set-uproc-delimited-continuations! task (cdr stack))
    (car stack)))
