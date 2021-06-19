#lang racket

;; An implementation of a priority queue.

(require data/heap) ;; NOTE The only external library in this codebase.

(provide (all-from-out data/heap)
         priority-queue queue-enqueue queue-dequeue queue-min queue-empty?)

(define (priority-queue comparator)
  (make-heap comparator))

(define (queue-enqueue q thing)
  (heap-add! q thing)
  q)

(define (queue-dequeue q)
  (heap-remove-min! q)
  q)

(define (queue-min q)
  (heap-min q))

(define (queue-empty? q)
  (= (heap-count q) 0))
