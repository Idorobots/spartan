;; The scheduler:

(load "runtime/processes.scm")

(define (task-list . tasks)
  tasks)

(define (add-task tasks task)
  (append tasks (list task)))

(define (pop-task tasks)
  (cdr tasks))

(define (next-task tasks)
  (car tasks))

(define (step uproc)
  (set-uproc-continuation! uproc
                           (resume (uproc-continuation uproc)))
  uproc)

(define (executable? task)
  (resumable? (uproc-continuation task)))

(define (execute tasks)
  (if (empty? tasks)
      nil
      (let ((n (next-task tasks))
            (rest (pop-task tasks)))
        (if (executable? n)
            (execute (add-task rest (step n)))
            (let ((result (uproc-continuation n)))
              (display ";; uProc finished with result:")
              (newline)
              (display result)
              (newline)
              (cons result (execute rest)))))))

;; FIXME This should accept task list istead.
(define (run cont)
  (let ((tasks (task-list (uproc cont))))
    ;; FIXME Returns only the first result.
    (car (execute tasks))))
