;; Closure conversion

;; Set operations work.
(assert (set) '())
(assert (set 'a 'b 'c) '(a b c))
(assert (set 'c 'a 'b) '(a b c))
(assert (set-difference (set 'a 'b 'c) (set 'a 'b 'c)) '())
(assert (set-difference (set 'a 'b 'c) (set 'a)) '(b c))
(assert (set-difference (set 'a 'b 'c) (set 'd)) '(a b c))
(assert (set-difference (set 'a 'b 'c) (set 'c 'd)) '(a b))
(assert (set-union (set 'a 'b 'c) (set 'a 'b 'c)) '(a b c))
(assert (set-union (set 'a 'b 'c) (set 'd)) '(a b c d))
(assert (set-union (set 'b 'c 'd) (set 'a)) '(a b c d))
(assert (set-union (set 'b 'c) (set 'a 'd)) '(a b c d))

;; Free variables computation works.
(assert (free-vars '()) '())
(assert (free-vars '23) '())
(assert (free-vars '"foo") '())
(assert (free-vars '(a b c)) '(a b c))
(assert (free-vars '(quote foo bar)) '())
(assert (free-vars '(list (quote foo bar) baz)) '(baz list))
(assert (free-vars '(define foo (list foo bar baz))) '(bar baz foo list))
(assert (free-vars '(do a b c)) '(a b c))
(assert (free-vars '(if a b c)) '(a b c))
(assert (free-vars '(letcc cont (foo bar cont))) '(bar foo))
(assert (free-vars '(letrec ((a 23) (bar a)) (* 2 bar))) '(*))
(assert (free-vars '(letrec ((a 23) (bar foo)) (* a bar))) '(* foo))
(assert (free-vars '(shift k (reset (* foo k)))) '(* foo))
(assert (free-vars '(handle (raise foo) bar)) '(bar foo))
(assert (free-vars '(lambda (x y) (+ x y))) '(+))

;; Substitution works.
(assert (substitute '((foo . bar)) 'faz) 'faz)
(assert (substitute '((foo . bar)) 'foo) 'bar)
(assert (substitute '((foo . bar)) '(foo bar)) '(bar bar))
(assert (substitute '((foo . bar) (bar . foo)) '(foo bar)) '(bar foo))
(assert (substitute '((foo . bar)) '(foo foo)) '(bar bar))
(assert (substitute '((foo . bar)) '(do foo (lambda (foo) foo))) '(do bar (lambda (bar) bar)))
(assert (substitute '((foo . bar)) '(do foo (lambda (bar) foo))) '(do bar (lambda (bar) bar)))

;; Simple cases work.
(assert (closure-convert 'foo) 'foo)
(assert (closure-convert 23) 23)
(assert (closure-convert '()), '())
(assert (closure-convert "hurr"), "hurr")

;; Convertin lambda works.

;; Converting application works.

;; Converting define works.

;; Converting do works.

;; Converting if works.
(assert (clojure-convert '(if a b c)) '(if a b c))

;; Converting letrec works.

;; Converting letcc works.

;; Converting shift/reset works.

;; Converting raise works.

;; Converting handle works.

