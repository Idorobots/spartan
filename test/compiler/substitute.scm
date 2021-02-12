;; Substitution works.

(define subs '((foo . bar)))

;; Simple cases work:
(assert (substitute subs 'faz) 'faz)
(assert (substitute subs 'foo) 'bar)
(assert (substitute subs '(foo bar)) '(bar bar))
(assert (substitute subs '(foo foo)) '(bar bar))
(assert (substitute (cons '(bar . foo) subs) '(foo bar)) '(bar foo))
(assert (substitute (cons '(bar . foo) subs) '(a b c)) '(a b c))

;; Syntax forms are handled correctly.
(assert (substitute subs '(define foo (list foo bar))) '(define foo (list bar bar)))
(assert (substitute subs '(if foo foo (not foo))) '(if bar bar (not bar)))
(assert (substitute subs '(do foo foo (also foo))) '(do bar bar (also bar)))
(assert (substitute subs '(do foo foo (also foo))) '(do bar bar (also bar)))
(assert (substitute subs '(set! foo bar)) '(set! foo bar))
(assert (substitute subs '(set! foo foo)) '(set! foo bar))
(assert (substitute subs '(raise foo)) '(raise bar))
(assert (substitute subs '(handle foo bar)) '(handle bar bar))
(assert (substitute subs '(handle bar foo)) '(handle bar bar))

;; Bindings are handled correctly.
(assert (substitute subs '(letrec ((bar foo) (foo bar)) foo)) '(letrec ((bar foo) (foo bar)) foo))
(assert (substitute subs '(let ((bar foo) (foo bar)) foo)) '(let ((bar bar) (foo bar)) foo))
(assert (substitute subs '(do foo (lambda (foo) foo))) '(do bar (lambda (foo) foo)))
(assert (substitute subs '(do foo (lambda (bar) foo))) '(do bar (lambda (bar) bar)))
(assert (substitute (cons '(bar . baz) subs) '(lambda (bar) (* foo bar))) '(lambda (bar) (* bar bar)))
(assert (substitute subs '(letcc cont (foo bar baz))) '(letcc cont (bar bar baz)))
(assert (substitute subs '(letcc foo (foo bar baz))) '(letcc foo (foo bar baz)))
(assert (substitute subs '(shift k (k (k foo)))) '(shift k (k (k bar))))
(assert (substitute subs '(shift foo (k (k foo)))) '(shift foo (k (k foo))))
