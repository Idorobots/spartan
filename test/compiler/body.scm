;; Implicit body expansion tests.

(describe
 "body expansion"
 (it "should only operate on `do` nodes"
     (assert (expand-body (make-lambda-node '()
                                            (make-symbol-node 'x)))
             (make-lambda-node '()
                               (make-symbol-node 'x))))

 (it "should expand well-formed body correctly"
     (assert (expand-body (at (location 1 1)
                              (make-do-node
                               (list (at (location 2 2)
                                         (make-number-node 1))
                                     (make-number-node 2)
                                     (at (location 3 3)
                                         (make-number-node 3))))))
             (at (location 1 1)
                 (make-do-node
                  (list (at (location 2 2)
                            (make-number-node 1))
                        (make-number-node 2)
                        (at (location 3 3)
                            (make-number-node 3))))))
     (assert (expand-body (at (location 1 1)
                              (make-do-node
                               (list (make-def-node (at (location 2 2)
                                                        (make-symbol-node 'foo))
                                                    (at (location 3 3)
                                                        (make-number-node 'bar)))
                                     (at (location 4 4)
                                         (make-number-node 2))
                                     (at (location 5 5)
                                         (make-number-node 3))))))
             (at (location 1 1)
                 (generated
                  (make-letrec-node (list (cons (at (location 2 2)
                                                    (make-symbol-node 'foo))
                                                (at (location 3 3)
                                                    (make-number-node 'bar))))
                                    (at (location 4 5)
                                        (generated
                                         (context "Bad `do` syntax"
                                                  (make-do-node
                                                   (list (at (location 4 4)
                                                             (make-number-node 2))
                                                         (at (location 5 5)
                                                             (make-number-node 3))))))))))))

 (it "should disallow not well-formed bodies"
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (expand-body (at (location 1 1)
                                (make-do-node
                                 (list (at (location 2 2)
                                           (make-def-node (at (location 3 3)
                                                              (make-symbol-node 'foo))
                                                          (at (location 4 4)
                                                              (make-number-node 'bar)))))))))
             "Bad `do` syntax, expected at least one non-definition expression within:"))

 (it "should preserve error context"
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (expand-body (at (location 1 1)
                                (context "Bad `lambda` body syntax"
                                         (make-do-node
                                          (list (at (location 2 2)
                                                    (make-def-node (at (location 3 3)
                                                                       (make-symbol-node 'foo))
                                                                   (at (location 4 4)
                                                                       (make-number-node 'bar))))))))))
             "Bad `lambda` body syntax, expected at least one non-definition expression within:")))
