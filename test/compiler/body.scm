;; Implicit body expansion tests.

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
                                    (context "Bad `lambda` body syntax"
                                             (make-do-node
                                              (list (at (location 1 1)
                                                        (make-number-node 1))
                                                    (make-number-node 2)
                                                    (at (location 3 3)
                                                        (make-number-node 3))))))))))

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
                                 (context "Bad `let` body syntax"
                                          (make-do-node
                                           (list (at (location 1 1)
                                                     (make-number-node 1))
                                                 (make-number-node 2)
                                                 (at (location 3 3)
                                                     (make-number-node 3))))))))))

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
                                    (context "Bad `letrec` body syntax"
                                             (make-do-node
                                              (list (at (location 1 1)
                                                        (make-number-node 1))
                                                    (make-number-node 2)
                                                    (at (location 3 3)
                                                        (make-number-node 3)))))))))))
