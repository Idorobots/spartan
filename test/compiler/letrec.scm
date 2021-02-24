;; Letrec conversion tests.

(describe
 "SCC"
 (it "works for fancy graphs"
     ;;   a
     ;;  / \
     ;; b---c
     ;; |
     ;; d---e
     (assert (scc '(a b c d e)
                  '((a b)
                    (b d)
                    (b c)
                    (c a)
                    (d e)
                    (e)))
             '((e) (d) (a c b)))
     (assert (scc '(a b c d e)
                  '((c a)
                    (a b)
                    (b c)
                    (b d)
                    (d e)
                    (e)))
             '((e) (d) (a c b)))
     (assert (scc '(a b c d e)
                  '((d e)
                    (c a)
                    (a b)
                    (b c)
                    (b d)
                    (e)))
             '((e) (d) (a c b))))

 (it "works for disjoint graphs"
     ;;   a
     ;;  / \
     ;; b---c
     ;;
     ;; d---e
     (assert (scc '(a b c d e)
                  '((a b)
                    (b c)
                    (c a)
                    (d e)
                    (e)))
             '((a c b) (e) (d))))

 (it "works for straing lines"
     ;;   a---b---c---d---e
     (assert (scc '(a b c d e)
                  '((a b)
                    (b c)
                    (c d)
                    (d e)
                    (e)))
             '((e) (d) (c) (b) (a))))

 (it "works for cyclic graphs"
     ;;   a---b---c---d---e
     ;;    \_____________/
     (assert (scc '(a b c d e)
                  '((a b)
                    (b c)
                    (c d)
                    (d e)
                    (e a)))
             '((a e d c b)))))

(describe
 "derefy"
 (it "replaces assigned variables with derefs"
     (assert (derefy '() '())
             '())
     (assert (derefy '() 'foo)
             'foo)
     (assert (derefy '(foo) 'foo)
             '(deref foo))
     (assert (derefy '(foo) '(bar foo))
             '(bar (deref foo)))
     (assert (derefy '(foo) '(lambda (foo) foo))
             '(lambda (foo) foo))
     (assert (derefy '(foo) '(lambda (bar) foo))
             '(lambda (bar) (deref foo)))
     (assert (derefy '(foo) '(let ((bar foo)) foo))
             '(let ((bar (deref foo)))
                (deref foo)))
     (assert (derefy '(foo) '(letrec ((foo foo)) foo))
             '(letrec ((foo foo))
                foo))))

(define (eval-after-conversion f expr)
  (display "Expression:") (newline)
  (pretty-print expr) (newline)
  (display "Conversion:") (newline)
  (let ((converted (f expr)))
    (pretty-print converted) (newline)
    (display "Result:") (newline)
    (let ((result (eval
                   ;; NOTE Uses Scheme letrec to implement fix. Kinda cheating.
                   (substitute '((fix . letrec))
                                    converted))))
      (pretty-print result) (newline)
      result)))

(describe
 "ref-conversion"
 (it "creates fix operator forms"
     (assert (ref-conversion
              '(letrec ((foo (lambda () (foo)))) (foo)))
             '(fix ((foo (lambda () (foo))))
                   (foo)))
     (assert (ref-conversion
              '(letrec ((foo (lambda () (bar)))
                        (bar (lambda () (foo))))
                 (foo)))
             '(fix ((foo (lambda () (bar)))
                    (bar (lambda () (foo))))
                   (foo))))

 (it "converts simple cases correctly"
     (assert (eval-after-conversion
              ref-conversion
              '(letrec ((foo 'foo-value)
                        (bar 'bar-value))
                 'body))
             'body)
     (assert (eval-after-conversion
              ref-conversion
              '(letrec () '()))
             '())
     (assert (eval-after-conversion
              ref-conversion
              '(letrec ((foo 23)
                        (bar (+ 5 foo)))
                 bar))
             28))

 (it "converts simple recursive functions correctly"
     (assert (eval-after-conversion
              ref-conversion
              '(letrec ((bar (lambda (x) (+ x foo)))
                        (foo (+ 23 5)))
                 (bar 5)))
             33)
     (assert (eval-after-conversion
              ref-conversion
              '(letrec* ((bar (lambda (x) (+ x foo)))
                         (foo (+ 23 5)))
                 (bar 5)))
             33)
     (assert (eval-after-conversion
              ref-conversion
              '(letrec ((foo (lambda (x) (* x 23)))
                        (bar (lambda (y) (foo y))))
                 (bar 23)))
             (* 23 23)))

 (it "converts more complex recursive functions correctly"
     (assert (eval-after-conversion
              ref-conversion
              '(letrec ((a (lambda () (b)))
                        (b (lambda () (begin (c) (d))))
                        (c (lambda () (a)))
                        (d (lambda () (e)))
                        (e (lambda () 23)))
                 (d)))
             23)
     (assert (eval-after-conversion
              ref-conversion
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
     (assert (eval-after-conversion
              ref-conversion
              '(letrec ((f (lambda () (even? 5)))
                        (even? (lambda (x)(or (zero? x) (odd? (- x 1)))))
                        (odd? (lambda (x) (not (even? x))))
                        (t (f)))
                 t))
             #f)
     (assert (eval-after-conversion
              ref-conversion
              '(letrec ((one (lambda ()
                               (+ 1 (two))))
                        (two (lambda ()
                               (+ 2 (three))))
                        (three (lambda ()
                                 3)))
                 (one)))
             6)
     (assert (eval-after-conversion
              ref-conversion
              '(letrec ((lazy-23 (cons 23 (lambda () lazy-23)))
                        (lazy-take (lambda (list n)
                                     (if (zero? n)
                                         '()
                                         (cons (car list)
                                               (lazy-take ((cdr list))
                                                          (- n 1))))))
                        (bar (foldl + 0 (lazy-take lazy-23 5))))
                 bar))
             (* 5 23))))
