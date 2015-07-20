;; Resume tests:

;; Can run values.
(assert (run 23) 23)

;; Can run compiled values.
(assert (run (do-expr '23)) 23)

;; Can run stuff.
(assert (run (do-expr '(= (* 3 2) (+ 3 3)))))

;; Can as easily resume stuff.
(assert (resume (resume (resume (do-expr '(= (* 3 2) (+ 3 3)))))))
