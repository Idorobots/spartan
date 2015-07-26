;; The scheduler:

(load "runtime/priority-queue.scm")
(load "runtime/processes.scm")

;; Some configuration constants:
(define +priority+ 100)     ;; Default priority.
(define +n-reductions+ 100) ;; Default number of reductions.

;; Task management:
(define *current-task* (ref nil))
(define *task-list* (ref nil))

(define (running-tasks)
  (deref *task-list*))

(define (task-queue)
  (priority-queue (lambda (t1 t2)
                    (<= (uproc-vtime t1)
                        (uproc-vtime t2)))))

(define *run-queue* (ref (task-queue)))

(define (reset-tasks! tasks)
  (assign! *task-list* tasks)
  (assign! *run-queue*
           (foldl (lambda (task q)
                    (queue-enqueue q task))
                  (task-queue)
                  tasks)))

(define (add-task! task)
  (assign! *task-list*
           (cons task (deref *task-list*))))

(define (task-queue-empty?)
  (queue-empty? (deref *run-queue*)))

(define (enqueue-task! task)
  (assign! *run-queue*
           (queue-enqueue (deref *run-queue*)
                          task)))

(define (dequeue-next-task!)
  (let ((n (next-task)))
    (assign! *current-task* n)
    (assign! *run-queue*
             (queue-dequeue (deref *run-queue*)))
    n))

(define (next-task)
  (queue-min (deref *run-queue*)))

;; Task execution:
(define (executable? task)
  ;; NOTE We could see ports added some time in the future.
  (cond ((uproc? task)
         (let ((c (uproc-continuation task)))
           (and (resumable? c)
                (can-resume? c))))))

(define (halted? task)
  ;; FIXME Actually check uproc state.
  (not (executable? task)))

(define (execute!)
  (execute-loop! nil))

(define (execute-loop! acc)
  (if (task-queue-empty?)
      acc
      (let ((n (dequeue-next-task!)))
        (execute-step! n)
        (if (not (halted? n))
            (do (enqueue-task! n)
                (execute-loop! acc))
            (let ((r (uproc-continuation n)))
              (display ";; Task finished with result:")
              (newline)
              (display r)
              (newline)
              (execute-loop! (cons r acc)))))))

(define (execute-step! task)
  ;; NOTE We could see ports added some time in the future.
  (cond ((uproc? task) (execute-uproc-step! task)))
  task)

(define (execute-uproc-step! uproc)
  (let ((start (current-milliseconds))
        (cont (resume-loop (uproc-continuation uproc) +n-reductions+))
        (stop (current-milliseconds)))
    (set-uproc-continuation! uproc cont)
    (inc-uproc-rtime! uproc (- stop start))))

;; FIXME This should accept a task instead.
(define (run cont)
  (reset-tasks! (list (uproc +priority+
                             (&yield-cont (lambda (_)
                                            cont)
                                          nil)
                             (current-milliseconds))))
  (car (execute!)))
