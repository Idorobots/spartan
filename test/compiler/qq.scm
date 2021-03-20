;; Quasiquote expander tests.

(describe
 "expand-quasiquote"
 (it "should handle easy cases"
     (assert (expand-quasiquote (make-number-node 23))
             (make-number-node 23))
     (assert (expand-quasiquote (make-symbol-node 'foo))
             (make-symbol-node 'foo))
     (assert (expand-quasiquote
              (at (location 5 23)
                  (make-quasiquote-node
                   (at (location 7 13)
                       (make-symbol-node 'foo)))))
             (at (location 5 23)
                 (generated
                  (make-quote-node
                   (at (location 7 13)
                       (make-symbol-node 'foo)))))))

 (it "should handle unquote"
     (assert (expand-quasiquote
              (at (location 5 23)
                  (make-quasiquote-node
                   (make-unquote-node
                    (at (location 7 13)
                        (make-symbol-node 'foo))))))
             (at (location 7 13)
                 (make-symbol-node 'foo)))
     (assert (expand-quasiquote
              (at (location 5 23)
                  (make-quasiquote-node
                   (at (location 6 6)
                       (make-list-node
                        (list
                         (at (location 7 7)
                             (make-number-node 1))
                         (at (location 8 8)
                             (make-unquote-node
                              (at (location 9 9)
                                  (make-symbol-node 'foo))))
                         (at (location 10 10)
                             (make-number-node 3))))))))
             (at (location 6 6)
                 (generated
                  (make-app-node
                   (at (location 6 6)
                       (generated
                        (make-symbol-node 'cons)))
                   (list (at (location 7 7)
                             (make-number-node 1))
                         (at (location 6 6)
                             (generated
                              (make-app-node
                               (at (location 6 6)
                                   (generated
                                    (make-symbol-node 'cons)))
                               (list (at (location 9 9)
                                         (make-symbol-node 'foo))
                                     (at (location 6 6)
                                         (generated
                                          (make-app-node
                                           (at (location 6 6)
                                               (generated
                                                (make-symbol-node 'cons)))
                                           (list (at (location 10 10)
                                                     (make-number-node 3))
                                                 (at (location 6 6)
                                                     (generated
                                                      (make-quote-node
                                                       (at (location 6 6)
                                                           (generated
                                                            (make-list-node '()))))))))))))))))))))

 (it "should handle unquote-splicing"
     (assert (expand-quasiquote
              (at (location 5 23)
                  (make-quasiquote-node
                   (at (location 6 6)
                       (make-list-node
                        (list
                         (at (location 7 7)
                             (make-number-node 1))
                         (at (location 8 8)
                             (make-unquote-splicing-node
                              (at (location 9 9)
                                  (make-symbol-node 'foo))))
                         (at (location 10 10)
                             (make-number-node 3))))))))
             (at (location 6 6)
                 (generated
                  (make-app-node
                   (at (location 6 6)
                       (generated
                        (make-symbol-node 'cons)))
                   (list (at (location 7 7)
                             (make-number-node 1))
                         (at (location 6 6)
                             (generated
                              (make-app-node
                               (at (location 6 6)
                                   (generated
                                    (make-symbol-node 'concat)))
                               (list (at (location 9 9)
                                         (make-symbol-node 'foo))
                                     (at (location 6 6)
                                         (generated
                                          (make-app-node
                                           (at (location 6 6)
                                               (generated
                                                (make-symbol-node 'cons)))
                                           (list (at (location 10 10)
                                                     (make-number-node 3))
                                                 (at (location 6 6)
                                                     (generated
                                                      (make-quote-node
                                                       (at (location 6 6)
                                                           (generated
                                                            (make-list-node '()))))))))))))))))))))

 (it "should reject top-level unquote-splicing"
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (expand-quasiquote
                (at (location 5 23)
                    (make-quasiquote-node
                     (at (location 7 13)
                         (make-unquote-splicing-node
                          (make-symbol-node 'foo)))))))
             "Misplaced `unquote-splicing`, expected to be enclosed within a spliceable value:")))
