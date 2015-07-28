;; Actor model runtime stuff:

(load "runtime/processes.scm")
(load "runtime/scheduler.scm")

(define (wait time) ;; NOTE Can't be called the same as Scheme sleep. :(
  (inc-uproc-rtime! (current-task)
                    time)
  time)

(define (self)
  (uproc-pid (current-task)))

(define (send pid msg)
  (let ((t (find-task pid)))
    ;; FIXME Throw exception when pid isn't found.
    (uproc-enqueue-msg! t msg)
    (when (equal? (uproc-state t) 'waiting-4-msg)
      (set-uproc-rtime! t (current-milliseconds))
      (enqueue-task! t))
    pid))

(define (recv)
  (let ((p (current-task)))
    (if (uproc-msg-queue-empty? p)
        (do (set-uproc-state! p 'waiting-4-msg)
            (cons #f nil))
        (cons #t (uproc-dequeue-msg! p)))))

(define (spawn fun)
  (let ((kont (lambda (v)
                (set-uproc-state! (current-task)
                                  'halted)
                v)))
    (spawn-task! (&yield-cont (lambda (_)
                                (fun kont))
                              nil)
                 (lambda (e _)
                   (display ";; Task finished due to unhandled error: ")
                   (display e)
                   (newline)
                   (kont e)))))
