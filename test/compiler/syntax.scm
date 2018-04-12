;; Syntax expander tests.

;; Expanding structure accessor paths works.
(assert (syntax-expand 'foo) 'foo)
(assert (syntax-expand 'foo.bar) '(&structure-ref foo 'bar))
(assert (syntax-expand 'foo.bar.baz) '(&structure-ref (&structure-ref foo 'bar) 'baz))

(assert (syntax-expand '(if a (lambda (b) b.c) d))
        '(if a (lambda (b) (&structure-ref b 'c)) d))

;; Wrapping lambda body works.
(assert (syntax-expand '(lambda (x) x))
        '(lambda (x) x))

(assert (syntax-expand '(lambda (x) x y))
        '(lambda (x) (do x y)))

(assert (syntax-expand '(lambda (x) (lambda (y) x y)))
        '(lambda (x) (lambda (y) (do x y))))

;; Expanding define works.
(assert (syntax-expand '(define foo bar))
        '(define foo bar))

(assert (syntax-expand '(define (foo x) bar))
        '(define foo (lambda (x) bar)))

(assert (syntax-expand '(define (foo x) bar x))
        '(define foo (lambda (x) (do bar x))))
