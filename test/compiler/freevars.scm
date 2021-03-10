;; Free vars computation.

(describe
 "sets"
 (it "should support various set operations"
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
     (assert (set-union (set 'b 'c) (set 'a 'd)) '(a b c d))))

(describe
 "freevars"
 (it "handles values correctly"
     (assert (free-vars '()) '())
     (assert (free-vars ''(list foo 23)) '())
     (assert (free-vars '23) '())
     (assert (free-vars '"foo") '())
     (assert (free-vars '(quote foo bar)) '()))
 (it "handles quotes correctly"
     (assert (free-vars 'foo) '(foo))
     (assert (free-vars '(list 23 foo)) '(foo list))
     (assert (free-vars '(list 23 'foo)) '(list))
     (assert (free-vars '(a b c)) '(a b c))
     (assert (free-vars '(list (quote foo bar) baz)) '(baz list)))
 (it "handles syntax forms correctly"
     (assert (free-vars '(define foo (list foo bar baz))) '(bar baz foo list))
     (assert (free-vars '(do a b c)) '(a b c))
     (assert (free-vars '(if foo bar baz)) '(bar baz foo))
     (assert (free-vars '(do a b (do c d))) '(a b c d))
     (assert (free-vars '(set! foo bar)) '(bar)))
 (it "handles bindings correctly"
     (assert (free-vars '(letrec ((a 23) (bar a)) (* 2 bar))) '(*))
     (assert (free-vars '(letrec ((a 23) (bar foo)) (* a bar))) '(* foo))
     (assert (free-vars '(lambda (x y) (+ x y))) '(+))
     (assert (free-vars '(lambda (x) x)) '())
     (assert (free-vars '(lambda (foo) (list 23 foo))) '(list))
     (assert (free-vars '(lambda (bar) (list 23 foo))) '(foo list)))
 (it "handles complex forms correctly"
     (assert (free-vars '(let ((f (lambda ()
                                    (even? 5))))
                           (let ((t (f)))
                             t)))
             '(even?))))
