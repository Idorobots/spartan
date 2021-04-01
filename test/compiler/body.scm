;; Implicit body expansion tests.

(describe
 "wrap-with-do"
 (it "should handle lists of expressions"
     (assert (wrap-with-do (list (at (location 1 2)
                                     (make-symbol-node 'x))
                                 (make-symbol-node 'y)
                                 (at (location 3 4)
                                     (make-symbol-node 'z)))
                           "Context")
             (at (location 1 4)
                 (generated
                  (context "Context"
                           (make-do-node
                            (list (at (location 1 2)
                                      (make-symbol-node 'x))
                                  (make-symbol-node 'y)
                                  (at (location 3 4)
                                      (make-symbol-node 'z)))))))))
 (it "should handle single expression lists"
     (assert (wrap-with-do (list (at (location 1 2)
                                     (make-symbol-node 'x)))
                           "Context")
             (at (location 1 2)
                 (generated
                  (context "Context"
                           (make-do-node
                            (list (at (location 1 2)
                                      (make-symbol-node 'x))))))))))

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
                               (list (at (location 6 6)
                                         (make-def-node (at (location 2 2)
                                                            (make-symbol-node 'foo))
                                                        (at (location 3 3)
                                                            (make-number-node 'bar))))
                                     (at (location 4 4)
                                         (make-number-node 2))
                                     (at (location 5 5)
                                         (make-number-node 3))))))
             (at (location 1 1)
                 (generated
                  (make-letrec-node (list (at (location 6 6)
                                              (generated
                                               (make-binding-node
                                                (at (location 2 2)
                                                    (make-symbol-node 'foo))
                                                (at (location 3 3)
                                                    (make-number-node 'bar))))))
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
