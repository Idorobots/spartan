;; Exception handling stuff.

(load-once "runtime/processes.scm")
(load-once "runtime/scheduler.scm")

(define (&error-handler)
  (uproc-error-handler (current-task)))

(define (&set-error-handler! handler)
  (set-uproc-error-handler! (current-task) handler))
