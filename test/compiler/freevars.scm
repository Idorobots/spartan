;; Free vars computation.

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

;; Values have no free vars.
(assert (free-vars '()) '())
(assert (free-vars ''(list foo 23)) '())
(assert (free-vars '23) '())
(assert (free-vars '"foo") '())
(assert (free-vars '(quote foo bar)) '())

;; Unquoted symbols are a free var.
(assert (free-vars 'foo) '(foo))
(assert (free-vars '(list 23 foo)) '(foo list))
(assert (free-vars '(list 23 'foo)) '(list))
(assert (free-vars '(a b c)) '(a b c))
(assert (free-vars '(list (quote foo bar) baz)) '(baz list))

;; Syntax forms work fine
(assert (free-vars '(define foo (list foo bar baz))) '(bar baz foo list))
(assert (free-vars '(do a b c)) '(a b c))
(assert (free-vars '(if foo bar baz)) '(bar baz foo))
(assert (free-vars '(do a b (do c d))) '(a b c d))
(assert (free-vars '(set! foo bar)) '(bar))
(assert (free-vars '(reset bar)) '(bar))
(assert (free-vars '(raise error)) '(error))
(assert (free-vars '(handle expression handler)) '(expression handler))
(assert (free-vars '(handle (raise foo) bar)) '(bar foo))

;; Bindings are correctly handled.
(assert (free-vars '(letrec ((a 23) (bar a)) (* 2 bar))) '(*))
(assert (free-vars '(letrec ((a 23) (bar foo)) (* a bar))) '(* foo))
(assert (free-vars '(lambda (x y) (+ x y))) '(+))
(assert (free-vars '(lambda (x) x)) '())
(assert (free-vars '(lambda (foo) (list 23 foo))) '(list))
(assert (free-vars '(lambda (bar) (list 23 foo))) '(foo list))
(assert (free-vars '(letcc cont (foo bar cont))) '(bar foo))
(assert (free-vars '(shift k (foo (k bar)))) '(bar foo))
(assert (free-vars '(shift k (reset (* foo k)))) '(* foo))

;; Complex formms are correctly handled:
(assert (free-vars '(let ((f (lambda ()
                               (even? 5))))
                      (let ((t (f)))
                        t)))
        '(even?))
