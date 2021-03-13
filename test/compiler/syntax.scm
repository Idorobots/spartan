;; Syntax expander tests.

(describe
 "syntax-expand"
 (it "expanding define works"
     (assert (syntax-expand '(define foo bar))
             '(define foo bar))
     (assert (syntax-expand '(define (foo x) bar))
             '(define foo (lambda (x) bar)))
     (assert (syntax-expand '(define (foo x) bar x))
             '(define foo (lambda (x) (do bar x))))))
