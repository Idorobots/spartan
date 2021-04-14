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
              (at (location 5 23)
                  (make-primop-app-node 'a
                                    (list (make-symbol-node 'b)
                                          (make-symbol-node 'c)))))
             (free-vars
              (set 'b 'c)
              (at (location 5 23)
                  (make-primop-app-node 'a
                                    (list (make-symbol-node 'b)
                                          (make-symbol-node 'c)))))))

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
