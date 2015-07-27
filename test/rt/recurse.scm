;; Recursion tests...

(assert (run '(letrec ((fact (lambda (n)
                               (if (< n 2)
                                   n
                                   (* n (fact (- n 1)))))))
                (fact 10)))
        3628800)

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
        '(() t))
