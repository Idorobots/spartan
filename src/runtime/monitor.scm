;; A task debug monitor.

(load "runtime/processes.scm")
(load "runtime/scheduler.scm")

(define (paddify thing padding)
  (let ((s (format "~a" thing)))
    (string-append (make-string (- padding (string-length s))
                                #\ )
                   s)))

(define (display-line . args)
  (display ";;")
  (map (lambda (a)
         (display (paddify a 15)))
       args)
  (newline))

(define (task-info)
  (map (lambda (task)
         (display-line "PID" "priority" "state" "VTime" "RTime")
         (display-line (uproc-pid task)
                       (uproc-priority task)
                       (uproc-state task)
                       (uproc-vtime task)
                       (uproc-rtime task)))
       (running-tasks))
  nil)
