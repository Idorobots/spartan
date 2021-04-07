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

(describe
 "letrec conversion"
 (it "converts simple cases correctly"
     (assert (run
              '(letrec ((foo 'foo-value)
                        (bar 'bar-value))
                 (list foo bar 'body)))
             (list 'foo-value 'bar-value 'body))
     (assert (run
              '(letrec () '()))
             '())
     (assert (run
              '(letrec ((foo 23)
                        (bar (+ 5 foo)))
                 bar))
             28))

 (it "converts simple recursive functions correctly"
     (assert (run
              '(letrec ((bar (lambda (x) (+ x foo)))
                        (foo (+ 23 5)))
                 (bar 5)))
             33)
     (assert (run
              '(letrec ((foo (lambda (x) (* x 23)))
                        (bar (lambda (y) (foo y))))
                 (bar 23)))
             (* 23 23)))

 (it "converts more complex recursive functions correctly"
     (assert (run
              '(letrec ((a (lambda () (b)))
                        (b (lambda () (do (c) (d))))
                        (c (lambda () (a)))
                        (d (lambda () (e)))
                        (e (lambda () 23)))
                 (d)))
             23)
     (assert (run
              '(letrec ((even? (lambda (x)
                                 (or (zero? x)
                                     (odd? (- x 1)))))
                        (odd? (lambda (x)
                                (not (even? x)))))
                 (list (even? 23)
                       (odd? 23)
                       (even? 4)
                       (odd? 4))))
             (list #f #t #t #f))
     (assert (run
              '(letrec ((f (lambda () (even? 5)))
                        (even? (lambda (x) (or (zero? x) (odd? (- x 1)))))
                        (odd? (lambda (x) (not (even? x))))
                        (t (f)))
                 t))
             #f)
     (assert (run
              '(letrec ((one (lambda ()
                               (+ 1 (two))))
                        (two (lambda ()
                               (+ 2 (three))))
                        (three (lambda ()
                                 3)))
                 (one)))
             6)
     (assert (run
              '(letrec ((lazy-23 (cons 23 (lambda () lazy-23)))
                        (lazy-take (lambda (list n)
                                     (if (zero? n)
                                         '()
                                         (cons (car list)
                                               (lazy-take ((cdr list))
                                                          (- n 1))))))
                        (bar (lazy-take lazy-23 5)))
                 bar))
             '(23 23 23 23 23)))

 (it "handles large shared env"
     (assert (run
              ;; env free-vars: var foo bar baz
              '(letrec ((var (ref '()))
                        ;; free-vars: bar, var
                        (foo (lambda (x)
                               (let ((v (deref var)))
                                 (assign! var (cons x v))
                                 (when (nil? v)
                                   (bar 23)))))
                        ;; free-vars: baz var
                        (bar (lambda (x)
                               (assign! var (cons x (deref var)))
                               (baz 5)))
                        ;; free-vars: foo var
                        (baz (lambda (x)
                               (assign! var (cons x (deref var)))
                               (foo 13))))
                 (foo 7)
                 (deref var)))
             '(13 5 23 7)))

 (it "handles cons shared env"
     (assert (run
              ;; env free-vars: var foo
              '(letrec ((var (ref '()))
                        ;; free-vars: foo var
                        (foo (lambda (x)
                               (let ((v (deref var)))
                                 (assign! var (cons x v))
                                 (when (nil? v)
                                   (foo 23))))))
                 (foo 7)
                 (deref var)))
             '(23 7)))

 (it "handles raw value shared env"
     (assert (run
              ;; env & fun free-vars: foo
              '(letrec ((foo (lambda (x)
                               (if false
                                   (foo 23)
                                   x))))
                 (foo 7)))
             7)))
