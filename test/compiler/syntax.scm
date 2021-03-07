;; Syntax expander tests.

(describe
 "syntax-expand"

 (it "wrapping lambda body works"
     (assert (syntax-expand '(lambda (x) x))
             '(lambda (x) x))
     (assert (syntax-expand '(lambda (x) x y))
             '(lambda (x) (do x y)))
     (assert (syntax-expand '(lambda (x) (lambda (y) x y)))
             '(lambda (x) (lambda (y) (do x y)))))

 (it "expanding define works"
     (assert (syntax-expand '(define foo bar))
             '(define foo bar))
     (assert (syntax-expand '(define (foo x) bar))
             '(define foo (lambda (x) bar)))
     (assert (syntax-expand '(define (foo x) bar x))
             '(define foo (lambda (x) (do bar x)))))

 (it "wrapping let body works"
     (assert (syntax-expand '(let ((x 23)) x))
             '(let ((x 23)) x))
     (assert (syntax-expand '(let ((x 23)) x y))
             '(let ((x 23)) (do x y)))
     (assert (syntax-expand '(letrec ((x 23)) x y))
             '(letrec ((x 23)) (do x y)))
     (assert (syntax-expand '(letcc x x y))
             '(letcc x (do x y)))))
