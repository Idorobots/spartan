;; Module system tests.

(require "testing.rkt")
(require "../src/main.rkt")

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
     (assert (run '(do (module (foo x)
                         (define (bar)
                           x))
                       (let ((m (foo 23)))
                         (m.bar))))
             23))

 (it "Modules can take other modules as deps."
     (assert (run '(do (module (foo)
                         (define (bar) 23))
                       (module (bar f)
                         (define (foo) (f.bar)))
                     (let* ((f (foo))
                            (b (bar f)))
                       (b.foo))))
             23)))
