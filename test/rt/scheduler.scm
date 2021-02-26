;; Process scheduling tests.

(describe
 "scheduler"
 (it "Can step a process:"
     (assert (not (executable? (uproc 100 nil nil 0 'waiting))))
     (assert (executable? (uproc 100 (&yield-cont (&make-closure (&make-env) id) nil) nil 0 'waiting)))
     (assert (uproc? (execute-step! (uproc 100 (&yield-cont (&make-closure (&make-env) id) nil) nil 0 'waiting)))))

 (it "Can modify task list:"
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
               t2)))

 (it "Can as easily resume stuff."
     (assert (resume
              (resume
               (resume
                (&apply __MULT 3 2 (&make-closure
                                    (&make-env)
                                    (lambda (_ mult)
                                      (&apply __PLUS 3 3 (&make-closure
                                                          (&make-env mult)
                                                          (lambda (e plus)
                                                            (&apply __EQUAL (&env-ref e 0) plus (&make-closure
                                                                                                 (&make-env)
                                                                                                 (lambda (_ v) v))))))))))))))

 (it "Can run compiled code."
     (assert (run '23) 23)
     (assert (run '(= (* 3 2) (+ 3 3)))))

 (it "Runing stuff changes state."
     (let ((p (uproc 100 nil nil 0 'waiting)))
       (reset-tasks! (list p))
       (assert (uproc-state p) 'waiting)
       (dequeue-next-task!)
       (assert (uproc-state p) 'running))))
