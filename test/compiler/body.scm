;; Implicit body expansion tests.

(describe
 "wrap-sequential"
 (it "should handle lists of expressions"
     (assert (wrap-sequential (list (at (location 1 2)
                                        (make-symbol-node 'x))
                                    (make-symbol-node 'y)
                                    (at (location 3 4)
                                        (make-symbol-node 'z))))
             (at (location 1 4)
                 (generated
                  (make-do-node
                   (list (at (location 1 2)
                             (make-symbol-node 'x))
                         (make-symbol-node 'y)
                         (at (location 3 4)
                             (make-symbol-node 'z))))))))
 (it "should handle single expression lists"
     (assert (wrap-sequential (list (at (location 1 2)
                                        (make-symbol-node 'x))))
             (at (location 1 2)
                 (make-symbol-node 'x))))
 (it "should handle single expression"
     (assert (wrap-sequential (at (location 1 2)
                                        (make-symbol-node 'x)))
             (at (location 1 2)
                 (make-symbol-node 'x)))))

(describe
 "body expansion"
 (it "should expand lambda body correctly"
     (assert (expand-body (make-lambda-node '()
                                            (list (make-symbol-node 'x))))
             (make-lambda-node '()
                               (make-symbol-node 'x)))
     (assert (expand-body (make-lambda-node '()
                                            (list (at (location 1 1)
                                                      (make-number-node 1))
                                                  (make-number-node 2)
                                                  (at (location 3 3)
                                                      (make-number-node 3)))))
             (make-lambda-node '()
                               (at (location 1 3)
                                   (generated
                                    (make-do-node
                                     (list (at (location 1 1)
                                               (make-number-node 1))
                                           (make-number-node 2)
                                           (at (location 3 3)
                                               (make-number-node 3)))))))))

 (it "should expand let body correctly"
     (assert (expand-body (make-let-node '()
                                         (list (make-symbol-node 'x))))
             (make-let-node '()
                            (make-symbol-node 'x)))
     (assert (expand-body (make-let-node '()
                                         (list (at (location 1 1)
                                                   (make-number-node 1))
                                               (make-number-node 2)
                                               (at (location 3 3)
                                                   (make-number-node 3)))))
             (make-let-node '()
                            (at (location 1 3)
                                (generated
                                 (make-do-node
                                  (list (at (location 1 1)
                                            (make-number-node 1))
                                        (make-number-node 2)
                                        (at (location 3 3)
                                            (make-number-node 3)))))))))

 (it "should expand letrec body correctly"
     (assert (expand-body (make-letrec-node '()
                                         (list (make-symbol-node 'x))))
             (make-letrec-node '()
                            (make-symbol-node 'x)))
     (assert (expand-body (make-letrec-node '()
                                         (list (at (location 1 1)
                                                   (make-number-node 1))
                                               (make-number-node 2)
                                               (at (location 3 3)
                                                   (make-number-node 3)))))
             (make-letrec-node '()
                            (at (location 1 3)
                                (generated
                                 (make-do-node
                                  (list (at (location 1 1)
                                            (make-number-node 1))
                                        (make-number-node 2)
                                        (at (location 3 3)
                                            (make-number-node 3))))))))))
