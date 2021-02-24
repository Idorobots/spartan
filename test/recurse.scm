;; Recursion tests

(describe
 "Recursion"
 (it "Works fine I guess..."
     (assert (run '(letrec ((foo (lambda (n)
                                   (if 't
                                       n
                                       (foo)))))
                     (foo 23)))
             23)
     (assert (run '(letrec ((fact (lambda (n)
                                    (if (< n 2)
                                        n
                                        (* n (fact (- n 1)))))))
                     (fact 10)))
             3628800)
     (assert (run '(letrec ((foo (lambda () bar 23))
                            (bar (lambda () foo 5)))
                     (list (foo)
                           (bar))))
             '(23 5))
     (assert (run '(letrec ((even? (lambda (x)
                                     (if (= 0 x)
                                         't
                                         (odd? (- x 1)))))
                            (odd? (lambda (x)
                                    (if (= 0 x)
                                        nil
                                        (even? (- x 1))))))
                     (list (even? 7)
                           (odd? 7))))
             '(() t))))

