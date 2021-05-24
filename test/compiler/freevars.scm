;; Free vars computation.

(describe
 "free-vars"
 (it "handles values correctly"
     (assert (compute-free-vars (make-ast-number 23))
             (make-ast-number 23))
     (assert (compute-free-vars (make-ast-list '()))
             (make-ast-list '()))
     (assert (compute-free-vars
              (make-ast-quote
               (make-ast-list
                (list (make-ast-symbol 'foo)
                      (make-ast-number 23)))))
             (make-ast-quote
              (make-ast-list
               (list (make-ast-symbol 'foo)
                     (make-ast-number 23)))))
     (assert (compute-free-vars (make-ast-string "foo"))
             (make-ast-string "foo"))
     (assert (compute-free-vars (make-ast-error
                                 (make-ast-location)))
             (make-ast-error
              (make-ast-location))))

 (it "handles syntax forms correctly"
     (assert (compute-free-vars
              (make-ast-do
               (list (make-ast-symbol 'a)
                     (make-ast-symbol 'b)
                     (make-ast-symbol 'c))))
             (set-ast-node-free-vars
              (set 'a 'b 'c)
              (make-ast-do
               (list (make-ast-symbol 'a)
                     (make-ast-symbol 'b)
                     (make-ast-symbol 'c)))))
     (assert (compute-free-vars
              (make-ast-if (make-ast-symbol 'a)
                           (make-ast-symbol 'b)
                           (make-ast-symbol 'c)))
             (set-ast-node-free-vars
              (set 'a 'b 'c)
              (make-ast-if (make-ast-symbol 'a)
                           (make-ast-symbol 'b)
                           (make-ast-symbol 'c))))
     (assert (compute-free-vars
              (make-ast-app (make-ast-symbol 'a)
                            (list (make-ast-symbol 'b)
                                  (make-ast-symbol 'c))))
             (set-ast-node-free-vars
              (set 'a 'b 'c)
              (make-ast-app (make-ast-symbol 'a)
                            (list (make-ast-symbol 'b)
                                  (make-ast-symbol 'c)))))
     (assert (compute-free-vars
              (at (location 5 23)
                  (make-ast-primop-app 'a
                                       (list (make-ast-symbol 'b)
                                             (make-ast-symbol 'c)))))
             (set-ast-node-free-vars
              (set 'b 'c)
              (at (location 5 23)
                  (make-ast-primop-app 'a
                                       (list (make-ast-symbol 'b)
                                             (make-ast-symbol 'c)))))))

 (it "handles bindings correctly"
     (define (b-n-f-vars bv fv expr)
       (set-ast-node-bound-vars bv
                                (set-ast-node-free-vars fv
                                                        expr)))
     (assert (compute-free-vars
              (make-ast-lambda '()
                               (make-ast-symbol 'x)))
             (set-ast-node-free-vars
              (set 'x)
              (make-ast-lambda '()
                               (make-ast-symbol 'x))))
     (assert (compute-free-vars
              (make-ast-lambda (list (make-ast-symbol 'x))
                               (make-ast-symbol 'x)))
             (set-ast-node-bound-vars
              (set 'x)
              (make-ast-lambda (list (make-ast-symbol 'x))
                               (make-ast-symbol 'x))))
     (assert (compute-free-vars
              (make-ast-let (list (make-ast-binding
                                   (make-ast-symbol 'x)
                                   (make-ast-symbol 'y)))
                            (make-ast-symbol 'z)))
             (set-ast-node-bound-vars
              (set 'x)
              (set-ast-node-free-vars
               (set 'y 'z)
               (make-ast-let (list (b-n-f-vars
                                    (set 'x) (set 'y)
                                    (make-ast-binding
                                     (make-ast-symbol 'x)
                                     (make-ast-symbol 'y))))
                             (make-ast-symbol 'z)))))
     (assert (compute-free-vars
              (make-ast-let (list (make-ast-binding
                                   (make-ast-symbol 'x)
                                   (make-ast-symbol 'y)))
                            (make-ast-symbol 'x)))
             (set-ast-node-bound-vars
              (set 'x)
              (set-ast-node-free-vars
               (set 'y)
               (make-ast-let (list (b-n-f-vars
                                    (set 'x) (set 'y)
                                    (make-ast-binding
                                     (make-ast-symbol 'x)
                                     (make-ast-symbol 'y))))
                             (make-ast-symbol 'x)))))
     (assert (compute-free-vars
              (make-ast-let (list (make-ast-binding
                                   (make-ast-symbol 'x)
                                   (make-ast-symbol 'y))
                                  (make-ast-binding
                                   (make-ast-symbol 'z)
                                   (make-ast-symbol 'x)))
                            (make-ast-symbol 'z)))
             (set-ast-node-bound-vars
              (set 'x 'z)
              (set-ast-node-free-vars
               (set 'x 'y)
               (make-ast-let (list (b-n-f-vars
                                    (set 'x) (set 'y)
                                    (make-ast-binding
                                     (make-ast-symbol 'x)
                                     (make-ast-symbol 'y)))
                                   (b-n-f-vars
                                    (set 'z) (set 'x)
                                    (make-ast-binding
                                     (make-ast-symbol 'z)
                                     (make-ast-symbol 'x))))
                             (make-ast-symbol 'z)))))
     (assert (compute-free-vars
              (make-ast-letrec (list (make-ast-binding (make-ast-symbol 'x)
                                                       (make-ast-symbol 'y)))
                               (make-ast-symbol 'z)))
             (set-ast-node-bound-vars
              (set 'x)
              (set-ast-node-free-vars
               (set 'y 'z)
               (make-ast-letrec (list (b-n-f-vars
                                       (set 'x) (set 'y)
                                       (make-ast-binding
                                        (make-ast-symbol 'x)
                                        (make-ast-symbol 'y))))
                                (make-ast-symbol 'z)))))
     (assert (compute-free-vars
              (make-ast-letrec (list (make-ast-binding (make-ast-symbol 'x)
                                                       (make-ast-symbol 'y)))
                               (make-ast-symbol 'x)))
             (set-ast-node-bound-vars
              (set 'x)
              (set-ast-node-free-vars
               (set 'y)
               (make-ast-letrec (list (b-n-f-vars
                                       (set 'x) (set 'y)
                                       (make-ast-binding
                                        (make-ast-symbol 'x)
                                        (make-ast-symbol 'y))))
                                (make-ast-symbol 'x)))))
     (assert (compute-free-vars
              (make-ast-letrec (list (make-ast-binding (make-ast-symbol 'x)
                                                       (make-ast-symbol 'y))
                                     (make-ast-binding (make-ast-symbol 'z)
                                                       (make-ast-symbol 'x)))
                               (make-ast-symbol 'z)))
             (set-ast-node-bound-vars
              (set 'x 'z)
              (set-ast-node-free-vars
               (set 'y)
               (make-ast-letrec (list (b-n-f-vars
                                       (set 'x) (set 'y)
                                       (make-ast-binding
                                        (make-ast-symbol 'x)
                                        (make-ast-symbol 'y)))
                                      (b-n-f-vars
                                       (set 'z) (set 'x)
                                       (make-ast-binding
                                        (make-ast-symbol 'z)
                                        (make-ast-symbol 'x))))
                                (make-ast-symbol 'z)))))))
