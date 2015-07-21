;; Process scheduling tests.

;; Can modify task list:
(let ((t (&micro-proc nil)))
  (assert (next-task (task-list t))
          t))

(let* ((t1 (&micro-proc 1))
       (t2 (&micro-proc 2))
       (tasks (task-list t1 t2)))
  (assert (next-task tasks) t1)
  (assert (next-task (pop-task tasks)) t2))

;; Can step a process:
(assert (not (executable? (&micro-proc nil))))
(assert (executable? (&micro-proc (&yield-cont nil nil))))
(assert (micro-proc? (step (&micro-proc (&yield-cont id nil)))))

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
