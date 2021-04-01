;; Free vars computation.

(describe
 "free-vars"
 (it "handles values correctly"
     (assert (compute-free-vars (make-number-node 23))
             (make-number-node 23))
     (assert (compute-free-vars (make-list-node '()))
             (make-list-node '()))
     (assert (compute-free-vars
              (make-quote-node
               (make-list-node
                (list (make-symbol-node 'foo)
                      (make-number-node 23)))))
             (make-quote-node
              (make-list-node
               (list (make-symbol-node 'foo)
                     (make-number-node 23)))))
     (assert (compute-free-vars (make-string-node "foo"))
             (make-string-node "foo"))
     (assert (compute-free-vars (make-error-node
                                 (make-location-node)))
             (make-error-node
              (make-location-node))))

 (it "handles syntax forms correctly"
     (assert (compute-free-vars
              (make-do-node
               (list (make-symbol-node 'a)
                     (make-symbol-node 'b)
                     (make-symbol-node 'c))))
             (free-vars
              (set 'a 'b 'c)
              (make-do-node
               (list (make-symbol-node 'a)
                     (make-symbol-node 'b)
                     (make-symbol-node 'c)))))
     (assert (compute-free-vars
              (make-if-node (make-symbol-node 'a)
                            (make-symbol-node 'b)
                            (make-symbol-node 'c)))
             (free-vars
              (set 'a 'b 'c)
              (make-if-node (make-symbol-node 'a)
                            (make-symbol-node 'b)
                            (make-symbol-node 'c))))
     (assert (compute-free-vars
              (make-app-node (make-symbol-node 'a)
                             (list (make-symbol-node 'b)
                                   (make-symbol-node 'c))))
             (free-vars
              (set 'a 'b 'c)
              (make-app-node (make-symbol-node 'a)
                             (list (make-symbol-node 'b)
                                   (make-symbol-node 'c)))))
     (assert (compute-free-vars
              (make-primop-app-node (make-symbol-node 'a)
                                    (list (make-symbol-node 'b)
                                          (make-symbol-node 'c))))
             (free-vars
              (set 'b 'c)
              (make-primop-app-node (make-symbol-node 'a)
                                    (list (make-symbol-node 'b)
                                          (make-symbol-node 'c))))))

 (it "handles bindings correctly"
     (define (b-n-f-vars bv fv expr)
       (bound-vars bv
                   (free-vars fv
                              expr)))
     (assert (compute-free-vars
              (make-lambda-node '()
                                (make-symbol-node 'x)))
             (free-vars
              (set 'x)
              (make-lambda-node '()
                                (make-symbol-node 'x))))
     (assert (compute-free-vars
              (make-lambda-node (list (make-symbol-node 'x))
                                (make-symbol-node 'x)))
             (bound-vars
              (set 'x)
              (make-lambda-node (list (make-symbol-node 'x))
                                (make-symbol-node 'x))))
     (assert (compute-free-vars
              (make-let-node (list (make-binding-node
                                    (make-symbol-node 'x)
                                    (make-symbol-node 'y)))
                             (make-symbol-node 'z)))
             (bound-vars
              (set 'x)
              (free-vars
               (set 'y 'z)
               (make-let-node (list (b-n-f-vars
                                     (set 'x) (set 'y)
                                     (make-binding-node
                                      (make-symbol-node 'x)
                                      (make-symbol-node 'y))))
                              (make-symbol-node 'z)))))
     (assert (compute-free-vars
              (make-let-node (list (make-binding-node
                                    (make-symbol-node 'x)
                                    (make-symbol-node 'y)))
                             (make-symbol-node 'x)))
             (bound-vars
              (set 'x)
              (free-vars
               (set 'y)
               (make-let-node (list (b-n-f-vars
                                     (set 'x) (set 'y)
                                     (make-binding-node
                                      (make-symbol-node 'x)
                                      (make-symbol-node 'y))))
                              (make-symbol-node 'x)))))
     (assert (compute-free-vars
              (make-let-node (list (make-binding-node
                                    (make-symbol-node 'x)
                                    (make-symbol-node 'y))
                                   (make-binding-node
                                    (make-symbol-node 'z)
                                    (make-symbol-node 'x)))
                             (make-symbol-node 'z)))
             (bound-vars
              (set 'x 'z)
              (free-vars
               (set 'x 'y)
               (make-let-node (list (b-n-f-vars
                                     (set 'x) (set 'y)
                                     (make-binding-node
                                      (make-symbol-node 'x)
                                      (make-symbol-node 'y)))
                                    (b-n-f-vars
                                     (set 'z) (set 'x)
                                     (make-binding-node
                                      (make-symbol-node 'z)
                                      (make-symbol-node 'x))))
                              (make-symbol-node 'z)))))
     (assert (compute-free-vars
              (make-letrec-node (list (make-binding-node (make-symbol-node 'x)
                                                         (make-symbol-node 'y)))
                                (make-symbol-node 'z)))
             (bound-vars
              (set 'x)
              (free-vars
               (set 'y 'z)
               (make-letrec-node (list (b-n-f-vars
                                        (set 'x) (set 'y)
                                        (make-binding-node
                                         (make-symbol-node 'x)
                                         (make-symbol-node 'y))))
                                 (make-symbol-node 'z)))))
     (assert (compute-free-vars
              (make-letrec-node (list (make-binding-node (make-symbol-node 'x)
                                                         (make-symbol-node 'y)))
                                (make-symbol-node 'x)))
             (bound-vars
              (set 'x)
              (free-vars
               (set 'y)
               (make-letrec-node (list (b-n-f-vars
                                        (set 'x) (set 'y)
                                        (make-binding-node
                                         (make-symbol-node 'x)
                                         (make-symbol-node 'y))))
                                 (make-symbol-node 'x)))))
     (assert (compute-free-vars
              (make-letrec-node (list (make-binding-node (make-symbol-node 'x)
                                                         (make-symbol-node 'y))
                                      (make-binding-node (make-symbol-node 'z)
                                                         (make-symbol-node 'x)))
                                (make-symbol-node 'z)))
             (bound-vars
              (set 'x 'z)
              (free-vars
               (set 'y)
               (make-letrec-node (list (b-n-f-vars
                                        (set 'x) (set 'y)
                                        (make-binding-node
                                         (make-symbol-node 'x)
                                         (make-symbol-node 'y)))
                                       (b-n-f-vars
                                        (set 'z) (set 'x)
                                        (make-binding-node
                                         (make-symbol-node 'z)
                                         (make-symbol-node 'x))))
                                 (make-symbol-node 'z)))))))

(describe
 "old freevars"
 (it "handles values correctly"
     (assert (free-vars-old '()) '())
     (assert (free-vars-old ''(list foo 23)) '())
     (assert (free-vars-old '23) '())
     (assert (free-vars-old '"foo") '())
     (assert (free-vars-old '(quote foo bar)) '()))
 (it "handles quotes correctly"
     (assert (free-vars-old 'foo) '(foo))
     (assert (free-vars-old '(list 23 foo)) '(foo list))
     (assert (free-vars-old '(list 23 'foo)) '(list))
     (assert (free-vars-old '(a b c)) '(a b c))
     (assert (free-vars-old '(list (quote foo bar) baz)) '(baz list)))
 (it "handles syntax forms correctly"
     (assert (free-vars-old '(do a b c)) '(a b c))
     (assert (free-vars-old '(if foo bar baz)) '(bar baz foo))
     (assert (free-vars-old '(do a b (do c d))) '(a b c d)))
 (it "handles bindings correctly"
     (assert (free-vars-old '(letrec ((a 23) (bar a)) (* 2 bar))) '(*))
     (assert (free-vars-old '(letrec ((a 23) (bar foo)) (* a bar))) '(* foo))
     (assert (free-vars-old '(lambda (x y) (+ x y))) '(+))
     (assert (free-vars-old '(lambda (x) x)) '())
     (assert (free-vars-old '(lambda (foo) (list 23 foo))) '(list))
     (assert (free-vars-old '(lambda (bar) (list 23 foo))) '(foo list)))
 (it "handles complex forms correctly"
     (assert (free-vars-old '(let ((f (lambda ()
                                        (even? 5))))
                               (let ((t (f)))
                                 t)))
             '(even?))
     (assert (free-vars-old '(let ((f (cons 1 f))
                                   (g 23))
                               (* 23 g h)))
             ;; NOTE f comes from the binding, h from the body, g is bound.
             '(* cons f h))
     (assert (free-vars-old '(letrec ((f (cons 1 f))
                                      (g 23))
                               (* 23 g h)))
             ;; NOTE f and g are bound, h comes from the body.
             '(* cons h))))
