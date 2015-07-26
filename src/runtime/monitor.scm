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
         (let ((pid (uproc-pid task))
               (p (uproc-priority task))
               (vt (uproc-vtime task))
               (rt (uproc-rtime task)))
           (display-line "PID" "priority" "VTime" "RTime")
           (display-line pid p vt rt)
           task))
       (running-tasks))
  nil)
