;; Exception handling stuff.

(load "runtime/processes.scm")
(load "runtime/scheduler.scm")

(define (&uproc-error-handler)
  (uproc-error-handler (current-task)))

(define (&set-uproc-error-handler! handler)
  (set-uproc-error-handler! (current-task) handler))
