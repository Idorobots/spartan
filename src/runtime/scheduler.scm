;; The scheduler:

(load "runtime/processes.scm")

;; Task management:
(define *current-task* (ref nil))
(define *task-list* (ref nil))

(define (reset-task-list! tasks)
  (assign! *task-list* tasks))

(define (task-list-empty?)
  (empty? (deref *task-list*)))

(define (add-task! task)
  (assign! *task-list*
           (append (deref *task-list*)
                   (list task))))

(define (pop-task!)
  (let ((n (next-task)))
    (assign! *task-list*
             (cdr (deref *task-list*)))
    n))

(define (next-task)
  (car (deref *task-list*)))

;; Task execution:
(define (execute-step! uproc)
  (set-uproc-continuation! uproc
                           (resume (uproc-continuation uproc)))
  uproc)

(define (executable? task)
  (resumable? (uproc-continuation task)))

(define (execute-loop! acc)
  (if (task-list-empty?)
      acc
      (let ((n (pop-task!)))
        (if (executable? n)
            (do (assign! *current-task* n)
                (add-task! (execute-step! n))
                (execute-loop! acc))
            (let ((r (uproc-continuation n)))
              (display ";; uProc finished with result:")
              (newline)
              (display r)
              (newline)
              (execute-loop! (cons r acc)))))))

(define (execute!)
  (execute-loop! nil))

;; FIXME This should accept task list istead.
(define (run cont)
  (reset-tasks! (list (uproc cont 0)))
  ;; FIXME Returns only the first result.
  (car (execute!)))
