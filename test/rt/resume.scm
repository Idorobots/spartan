;; Resume tests:

;; Can run values.
(assert (run 23) 23)

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
