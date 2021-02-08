;; Letrec conversion tests.

;; Free vars computation.

(assert (free-vars '())
        '())

(assert (free-vars 'foo)
        '(foo))

(assert (free-vars '(list 23 foo))
        '(foo list))

(assert (free-vars '(lambda (foo) (list 23 foo)))
        '(list))

(assert (free-vars '(let ((f (lambda ()
                               (even? 5))))
                      (let ((t (f)))
                        t)))
        '(even?))
;; SCC

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
        '((e) (d) (a c b)))

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
        '((a c b) (e) (d)))

;;   a---b---c---d---e

(assert (scc '(a b c d e)
             '((a b)
               (b c)
               (c d)
               (d e)
               (e)))
        '((e) (d) (c) (b) (a)))

;;   a---b---c---d---e
;;    \_____________/

(assert (scc '(a b c d e)
             '((a b)
               (b c)
               (c d)
               (d e)
               (e a)))
        '((a e d c b)))

;; Assignment conversion:

(assert (derefy '() '())
        '())

(assert (derefy 'foo '())
        'foo)

(assert (derefy 'foo '(foo))
        '(deref foo))

(assert (derefy '(bar foo) '(foo))
        '(bar (deref foo)))

(assert (derefy '(let ((foo foo)) foo) '(foo))
        '(let ((foo foo))
           foo))

(assert (derefy '(lambda (foo) foo) '(foo))
        '(lambda (foo) foo))

(assert (derefy '(let ((bar foo)) foo) '(foo))
        '(let ((bar (deref foo)))
           (deref foo)))

(assert (derefy '(lambda (bar) foo) '(foo))
        '(lambda (bar) (deref foo)))

;; Conversion:

(assert (ref-conversion
         '(letrec ((foo (lambda () (foo)))) (foo)))
        '(let ((foo (lambda (foo)
                      (lambda ()
                        (let ((foo (foo foo)))
                          (foo))))))
           (let ((foo (foo foo)))
             (foo))))

(assert (ref-conversion
         '(letrec ((foo (lambda () (bar)))
                   (bar (lambda () (foo))))
            (foo)))
        '(let ((foo (lambda (foo bar)
                      (lambda ()
                        (let ((bar (bar foo bar)))
                          (bar)))))
               (bar (lambda (foo bar)
                      (lambda ()
                        (let ((foo (foo foo bar)))
                          (foo))))))
           (let ((foo (foo foo bar)))
             (foo))))

;; Extra tests:

(define (eval-after-conversion f expr)
  (display "Expression:") (newline)
  (pretty-print expr) (newline)
  (display "Conversion:") (newline)
  (let ((converted (f expr)))
    (pretty-print converted) (newline)
    (display "Result:") (newline)
    (let ((result (eval converted)))
      (pretty-print result) (newline)
      result)))

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
        28)

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
        (* 23 23))

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
        (* 5 23))
