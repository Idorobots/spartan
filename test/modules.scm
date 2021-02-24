;; Module system tests.

(describe
 "module system"
 (it "Can create a structure."
     (assert (run '(let ((s (structure
                             (define (foo x)
                               x)
                             (define (bar y)
                               y))))
                     (list (s.foo 23) (s.bar 5))))
             '(23 5)))

 (it "Structure members are recursive."
     (assert (run '(let ((s (structure
                             (define (foo x)
                               x)
                             (define (bar y)
                               (foo y)))))
                     (s.bar 23)))
             23))

 (it "Module is a parameterized structure."
     (assert (do (run '(module (foo x)
                         (define (bar)
                           x)))
                 (run '(let ((m (foo 23)))
                         (m.bar))))
             23))

 (it "Modules can take other modules as deps."
     (assert (do (run '(module (foo)
                         (define (bar) 23)))
                 (run '(module (bar f)
                         (define (foo) (f.bar))))
               (run '(let* ((f (foo))
                            (b (bar f)))
                       (b.foo))))
             23)))
