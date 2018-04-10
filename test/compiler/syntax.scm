;; Syntax expander tests.

(assert (syntax-expand 'foo) 'foo)
(assert (syntax-expand 'foo.bar) '(&structure-ref foo 'bar))
(assert (syntax-expand 'foo.bar.baz) '(&structure-ref (&structure-ref foo 'bar) 'baz))

(assert (syntax-expand '(if a (lambda (b) b.c) d))
        '(if a (lambda (b) (&structure-ref b 'c)) d))
