;; Free vars computation.

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
     (assert (free-vars '(do a b c)) '(a b c))
     (assert (free-vars '(if foo bar baz)) '(bar baz foo))
     (assert (free-vars '(do a b (do c d))) '(a b c d)))
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
             '(even?))
     (assert (free-vars '(let ((f (cons 1 f))
                               (g 23))
                           (* 23 g h)))
             ;; NOTE f comes from the binding, h from the body, g is bound.
             '(* cons f h))
     (assert (free-vars '(letrec ((f (cons 1 f))
                                  (g 23))
                           (* 23 g h)))
             ;; NOTE f and g are bound, h comes from the body.
             '(* cons h))))
