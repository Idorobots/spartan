;; Utilities tests.

(describe
 "Mutable refs"
 (it "should dereference a value without modification"
     (define x (ref 0))
     (assert (deref x) 0)
     (assert (deref x) 0))
 (it "should allow modification of values"
     (define x (ref 0))
     (assert (deref x) 0)

     (assign! x 1)
     (assert (deref x) 1)
     (assign! x 2)
     (assert (deref x) 2)))

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
 "gensym"
 (it "should incrementally name variables"
     (gensym-reset!)
     (assert (gensym 'a) 'a1)
     (assert (gensym 'a) 'a2)
     (assert (gensym 'b) 'b3))
 (it "should allow resetting"
     (gensym-reset!)
     (assert (gensym 'a) 'a1)
     (assert (gensym 'b) 'b2)
     (gensym-reset!)
     (assert (gensym 'a) 'a1)))
