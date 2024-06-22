#lang racket

;; An implementation of a priority queue.

(provide (struct-out queue)
         priority-queue queue-enqueue queue-dequeue queue-min queue-empty?)

(struct queue (comparator items))

(define (priority-queue comparator)
  (queue comparator '()))

(define (queue-empty? q)
  (empty? (queue-items q)))

(define (queue-min q)
  ;; NOTE Assumes non-empty queue.
  (car (queue-items q)))

(define (queue-dequeue q)
  ;; NOTE Assumes non-empty queue.
  (queue (queue-comparator q)
         (cdr (queue-items q))))

(define (queue-enqueue q thing)
  (let ((new-items (let loop ((i (queue-items q)))
                     (cond ((empty? i)
                            (list thing))
                           (((queue-comparator q) thing (car i))
                            (cons thing i))
                           (else
                            (cons (car i)
                                  (loop (cdr i))))))))
    (queue (queue-comparator q)
           new-items)))
