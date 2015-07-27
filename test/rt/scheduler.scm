;; Process scheduling tests.

;; Can step a process:
(assert (not (executable? (uproc 100 nil nil 0 'waiting))))
(assert (executable? (uproc 100 (&yield-cont id nil) nil 0 'waiting)))
(assert (uproc? (execute-step! (uproc 100 (&yield-cont id nil) nil 0 'waiting))))

;; Can modify task list:
(let ((t (uproc 100 nil nil 0 'waiting)))
  (assert (do (reset-tasks! (list t))
              (next-task))
          t))

(let* ((t1 (uproc 100 nil nil 1 'waiting))
       (t2 (uproc 100 nil nil 2 'waiting)))
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

;; Can do the same for do-string.
(assert (resume (resume (resume (do-string "(= (* 3 2) (+ 3 3))")))))

;; Runing stuff changes state.
(let ((p (uproc 100 nil nil 0 'waiting)))
  (reset-tasks! (list p))
  (assert (uproc-state p) 'waiting)
  (dequeue-next-task!)
  (assert (uproc-state p) 'running))
