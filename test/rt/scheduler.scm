;; Process scheduling tests.

;; Can step a process:
(assert (not (executable? (uproc nil 0))))
(assert (executable? (uproc (&yield-cont nil nil) 0)))
(assert (uproc? (execute-step! (uproc (&yield-cont id nil) 0))))

;; Can modify task list:
(let ((t (uproc nil 0)))
  (assert (do (reset-tasks! (list t))
              (next-task))
          t))

(let* ((t1 (uproc nil 1))
       (t2 (uproc nil 2)))
  (assert (do (reset-tasks! (list t1 t2))
              (next-task))
          t1)
  (assert (do (reset-tasks! (list t1 t2))
              (dequeue-next-task!)
              (next-task))
          t2))

;; Can run values.
(assert (run 23) 23)
(assert (run nil) nil)

;; Can run compiled values.
(assert (run (do-expr '23)) 23)

;; Can run stuff.
(assert (run (do-expr '(= (* 3 2) (+ 3 3)))))

;; Can as easily resume stuff.
(assert (resume (resume (resume (do-expr '(= (* 3 2) (+ 3 3)))))))

;; Can do the same for do-string
(assert (run (do-string "23")) 23)
(assert (run (do-string "(= (* 3 2) (+ 3 3))")))
(assert (resume (resume (resume (do-string "(= (* 3 2) (+ 3 3))")))))

;; TODO Can run several tasks.
