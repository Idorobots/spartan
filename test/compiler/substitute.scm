;; Substitution works.

(define subs '((foo . bar)))

(describe
 "substitute"
 (it "simple cases work"
     (assert (substitute subs 'faz) 'faz)
     (assert (substitute subs 'foo) 'bar)
     (assert (substitute subs '(foo bar)) '(bar bar))
     (assert (substitute subs '(foo foo)) '(bar bar))
     (assert (substitute (cons '(bar . foo) subs) '(foo bar)) '(bar foo))
     (assert (substitute (cons '(bar . foo) subs) '(a b c)) '(a b c)))
 (it "syntax forms are handled correctly"
     (assert (substitute subs '(if foo foo (not foo))) '(if bar bar (not bar)))
     (assert (substitute subs '(do foo foo (also foo))) '(do bar bar (also bar)))
     (assert (substitute subs '(do foo foo (also foo))) '(do bar bar (also bar))))
 (it "bindings are handled correctly"
     (assert (substitute subs '(letrec ((bar foo) (foo bar)) foo)) '(letrec ((bar foo) (foo bar)) foo))
     (assert (substitute subs '(let ((bar foo) (foo bar)) foo)) '(let ((bar bar) (foo bar)) foo))
     (assert (substitute subs '(do foo (lambda (foo) foo))) '(do bar (lambda (foo) foo)))
     (assert (substitute subs '(do foo (lambda (bar) foo))) '(do bar (lambda (bar) bar)))
     (assert (substitute (cons '(bar . baz) subs) '(lambda (bar) (* foo bar))) '(lambda (bar) (* bar bar)))))
