;; Process scheduling tests.

;; Can step a process:
(assert (not (executable? (uproc 100 nil 0))))
(assert (executable? (uproc 100 (&yield-cont id nil) 0)))
(assert (uproc? (execute-step! (uproc 100 (&yield-cont id nil) 0))))

;; Can modify task list:
(let ((t (uproc 100 nil 0)))
  (assert (do (reset-tasks! (list t))
              (next-task))
          t))

(let* ((t1 (uproc 100 nil 1))
       (t2 (uproc 100 nil 2)))
  (assert (do (reset-tasks! (list t1 t2))
              (next-task))
          t1)
  (assert (do (reset-tasks! (list t1 t2))
              (dequeue-next-task!)
              (next-task))
          t2))

;; Can run compiled values.
(assert (run '23) 23)

;; Can run stuff.
(assert (run '(= (* 3 2) (+ 3 3))))

;; Can as easily resume stuff.
(assert (resume (resume (resume (do-expr '(= (* 3 2) (+ 3 3)))))))

;; Can do the same for do-string
(assert (resume (resume (resume (do-string "(= (* 3 2) (+ 3 3))")))))
