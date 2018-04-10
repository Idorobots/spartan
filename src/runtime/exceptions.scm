;; Exception handling stuff.

(load "runtime/processes.scm")
(load "runtime/scheduler.scm")

(define (&error-handler)
  (uproc-error-handler (current-task)))

(define (&set-error-handler! handler)
  (set-uproc-error-handler! (current-task) handler))
