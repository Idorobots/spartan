#lang racket

;; The scheduler:

(require "../compiler/utils/refs.rkt")
(require "priority-queue.rkt")
(require "closures.rkt")
(require "processes.rkt")
(require "continuations.rkt")

(provide current-task running-tasks find-task reset-tasks! spawn-task! wake-task! enqueue-task!
         ;; FIXME For test access.
         next-task dequeue-next-task! executable? execute! execute-step!)

;; Some configuration constants:
(define +n-reductions+ 100)       ;; Default number of reductions.
(define +idle-timeout+ 50)        ;; Time to wait between run queue polls.
(define +initial-state+ 'waiting) ;; Initial state of uProcs.

;; Task management:
(define *current-task* (ref '()))
(define *task-list* (ref '()))

(define (current-task)
  (deref *current-task*))

(define (running-tasks)
  (deref *task-list*))

(define (task-queue)
  (priority-queue (lambda (t1 t2)
                    (<= (uproc-vtime t1)
                        (uproc-vtime t2)))))

(define *run-queue* (ref (task-queue)))

(define (reset-tasks! tasks)
  (assign! *current-task* '())
  (assign! *task-list* tasks)
  (assign! *run-queue*
           (foldl (lambda (task q)
                    (queue-enqueue q task))
                  (task-queue)
                  tasks)))

(define (spawn-task! priority thunk handler)
  (let* ((kont (make-closure
                '()
                (lambda (_ v)
                  (set-uproc-state! (current-task)
                                    'halted)
                  v)))
         (t (make-uproc priority
                        (make-resumable
                         (make-closure
                          kont
                          (lambda (kont fun)
                            (apply-closure fun kont)))
                         thunk)
                        (make-closure
                         (cons handler kont)
                         (lambda (handler/kont err restart _)
                           (apply-closure (car handler/kont)
                                          err
                                          restart
                                          (cdr handler/kont))))
                        (current-milliseconds)
                        +initial-state+)))
    (add-task! t)
    (enqueue-task! t)
    (uproc-pid t)))

(define (add-task! task)
  (assign! *task-list*
           (cons task (deref *task-list*))))

(define (task-queue-empty?)
  (queue-empty? (deref *run-queue*)))

(define (wake-task! task)
  (when (equal? (uproc-state task) 'waiting-4-msg)
    (set-uproc-rtime! task (current-milliseconds))
    (enqueue-task! task)))

(define (enqueue-task! task)
  (assign! *run-queue*
           (queue-enqueue (deref *run-queue*)
                          task))
  (set-uproc-state! task 'waiting))

(define (dequeue-next-task!)
  (let ((n (next-task)))
    (assign! *current-task* n)
    (assign! *run-queue*
             (queue-dequeue (deref *run-queue*)))
    (set-uproc-state! n 'running)
    n))

(define (next-task)
  (queue-min (deref *run-queue*)))

(define (find-task pid)
  (findf (lambda (t)
           (equal? (uproc-pid t) pid))
         (deref *task-list*)))

;; Task execution:
(define (executable? task)
  (let ((c (uproc-continuation task)))
    (and (resumable? c)
         (can-resume? c))))

(define (execute!)
  (execute-loop! '()))

(define (execute-loop! acc)
  (if (task-queue-empty?)
      acc
      (let ((n (next-task)))
        (wait-until-ready n)
        (dequeue-next-task!)
        (execute-step! n)
        (let ((s (task-state n)))
          (cond ((equal? s 'halted)
                 (execute-loop! (cons (extract-result n)
                                      acc)))

                ((equal? s 'waiting-4-msg)
                 ;; NOTE Don't schedule until task is notified.
                 (execute-loop! acc))

                ('else
                 (enqueue-task! n)
                 (execute-loop! acc)))))))

(define (extract-result task)
  (uproc-continuation task))

(define (task-state task)
  (if (executable? task)
      (uproc-state task)
      'halted))

(define (wait-until-ready task)
  (let ((t (current-milliseconds)))
    (unless (ready? task t)
      ;; TODO Do useful stuff here.
      (sleep (/ +idle-timeout+ 1000))
      (wait-until-ready task))))

(define (ready? task t)
  (>= t (uproc-rtime task)))

(define (execute-step! task)
  (execute-uproc-step! task)
  task)

(define (execute-uproc-step! uproc)
  (let ((start (current-milliseconds))
        (_ (resume-execution! uproc +n-reductions+))
        (stop (current-milliseconds)))
    (inc-uproc-rtime! uproc (- stop start))))

(define (resume-execution! uproc n-reductions)
  (let ((cont (uproc-continuation uproc)))
    (when (and (ready? uproc (current-milliseconds))
               (equal? (uproc-state uproc) 'running)
               (> n-reductions 0)
               (resumable? cont)
               (can-resume? cont))
      (set-uproc-continuation! uproc (resume cont))
      (resume-execution! uproc (- n-reductions 1)))))
