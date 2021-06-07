#lang racket

;; Exception handling stuff.

(require "processes.rkt")
(require "scheduler.rkt")

(provide &error-handler &set-error-handler!)

(define (&error-handler)
  (uproc-error-handler (current-task)))

(define (&set-error-handler! handler)
  (set-uproc-error-handler! (current-task) handler))
