;; Exception handling stuff.

(load "runtime/processes.scm")
(load "runtime/scheduler.scm")

(define (&uproc-error-handler)
  (uproc-error-handler (current-task)))

