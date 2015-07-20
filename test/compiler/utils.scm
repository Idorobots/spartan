;; Utilities tests.

(define x (ref 0))
(assert (deref x) 0)

(assign! x 1)
(assert (deref x) 1)
(assign! x 2)
(assert (deref x) 2)

(gensym-reset!)
(assert (gensym 'a) '__a1)
(assert (gensym 'b) '__b2)
(gensym-reset!)
(assert (gensym 'c) '__c1)
