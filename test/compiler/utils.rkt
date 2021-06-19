#lang racket

;; Utilities tests.

(require "../testing.rkt")
(require "../../src/compiler/utils/utils.rkt")
(require "../../src/compiler/utils/io.rkt")
(require "../../src/compiler/utils/refs.rkt")
(require "../../src/compiler/utils/set.rkt")
(require "../../src/compiler/utils/gensym.rkt")
(require "../../src/compiler/utils/scc.rkt")

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
     (assert (set) (set))
     (assert (set 'a 'b 'c) (set 'a 'b 'c))
     (assert (set 'c 'a 'b) (set 'a 'b 'c))
     (assert (set-difference (set 'a 'b 'c) (set 'a 'b 'c)) (set))
     (assert (set-difference (set 'a 'b 'c) (set 'a)) (set 'b 'c))
     (assert (set-difference (set 'a 'b 'c) (set 'd)) (set 'a 'b 'c))
     (assert (set-difference (set 'a 'b 'c) (set 'c 'd)) (set 'a 'b))
     (assert (set-union (set 'a 'b 'c) (set 'a 'b 'c)) (set 'a 'b 'c))
     (assert (set-union (set 'a 'b 'c) (set 'd)) (set 'a 'b 'c 'd))
     (assert (set-union (set 'b 'c 'd) (set 'a)) (set 'a 'b 'c 'd))
     (assert (set-union (set 'b 'c) (set 'a 'd)) (set 'a 'b 'c 'd))
     (assert (set-intersection (set 'a 'b 'c)
                               (set 'x 'b 'c))
             (set 'b 'c))
     (assert (set-intersection (set 'a 'b 'c)
                               (set 'x 'y 'z))
             (set))))

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

(describe
 "SCC"
 (it "works for fancy graphs"
     ;;   a
     ;;  / \
     ;; b---c
     ;; |
     ;; d---e
     (assert (scc '(a b c d e)
                  '((a b)
                    (b d)
                    (b c)
                    (c a)
                    (d e)
                    (e)))
             '((e) (d) (a c b)))
     (assert (scc '(a b c d e)
                  '((c a)
                    (a b)
                    (b c)
                    (b d)
                    (d e)
                    (e)))
             '((e) (d) (a c b)))
     (assert (scc '(a b c d e)
                  '((d e)
                    (c a)
                    (a b)
                    (b c)
                    (b d)
                    (e)))
             '((e) (d) (a c b))))

 (it "works for disjoint graphs"
     ;;   a
     ;;  / \
     ;; b---c
     ;;
     ;; d---e
     (assert (scc '(a b c d e)
                  '((a b)
                    (b c)
                    (c a)
                    (d e)
                    (e)))
             '((a c b) (e) (d))))

 (it "works for straing lines"
     ;;   a---b---c---d---e
     (assert (scc '(a b c d e)
                  '((a b)
                    (b c)
                    (c d)
                    (d e)
                    (e)))
             '((e) (d) (c) (b) (a))))

 (it "works for cyclic graphs"
     ;;   a---b---c---d---e
     ;;    \_____________/
     (assert (scc '(a b c d e)
                  '((a b)
                    (b c)
                    (c d)
                    (d e)
                    (e a)))
             '((a e d c b)))))
