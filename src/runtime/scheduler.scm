;; The scheduler:

(load "runtime/priority-queue.scm")
(load "runtime/processes.scm")

;; Task management:
(define *current-task* (ref nil))
(define *task-list* (ref nil))

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
(define (execute-step! task)
  ;; NOTE We could see ports added some time in the future.
  (cond ((uproc? task)
         (set-uproc-continuation! task
                                  (resume (uproc-continuation task)))
         (inc-uproc-vtime! task 1)))
  task)

(define (executable? task)
  ;; NOTE We could see ports added some time in the future.
  (cond ((uproc? task) (resumable? (uproc-continuation task)))))

(define (halted? task)
  ;; FIXME Actually check uproc state.
  (not (executable? task)))

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

(define (execute!)
  (execute-loop! nil))

;; FIXME This should accept task list istead.
(define (run cont)
  (reset-tasks! (list (uproc (&yield-cont (lambda (_)
                                            cont)
                                          nil)
                             0)))
  (car (execute!)))
