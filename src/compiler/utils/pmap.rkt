#lang racket

(require racket/future)

(provide pmap
         future-map
         thread-pool-map
         spawn-thread-pool)

(define +parallelism+ 0)

(define *thread-pool* #f)

(define (pmap fun lst)
  (cond ((= +parallelism+ 0)
         (map fun lst))
        ((= +parallelism+ 1)
         (future-map fun lst))
        (else
         (thread-pool-map (or *thread-pool*
                              (begin
                                (set! *thread-pool* (spawn-thread-pool +parallelism+))
                                *thread-pool*))
                          fun
                          lst))))

(define (future-map fun lst)
  (map touch
       (map (lambda (pass)
              (future
               (lambda ()
                 (fun pass))))
            lst)))

;; FIXME This is actually greenthreading... *facepalm*
(define (thread-pool-map thread-pool fun lst)
  ;; NOTE Assigns work in round-robin fashion.
  (let loop ((values lst)
             (threads *thread-pool*))
    (cond ((null? threads)
           (loop values *thread-pool*))
          ((null? values)
           '())
          (else
           (thread-send (car threads)
                        (lambda ()
                          (fun (car values))))
           (loop (cdr values)
                 (cdr threads)))))
  ;; NOTE Doesn't care about the order at all.
  (let loop ((values lst))
    (if (null? values)
        '()
        (cons (thread-receive)
              (loop (cdr values))))))

(define (spawn-thread-pool threads)
  (let ((t (current-thread)))
    (map (lambda _
           (thread
            (lambda ()
              (let loop ()
                (let ((workload (thread-receive)))
                  (thread-send t (workload))
                  (loop))))))
         (make-list threads #f))))
