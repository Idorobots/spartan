;; Semantic elaboration.

(describe
 "expand-syntax-forms"
 (it "elaborates valid ifs"
     (assert (elaborate-syntax-forms (at (location 5 23)
                                         (make-list-node
                                          (list (make-symbol-node 'if)
                                                (make-number-node 23)
                                                (make-number-node 5)
                                                (make-number-node 0)))))
             (at (location 5 23)
                 (make-if-node (make-number-node 23)
                               (make-number-node 5)
                               (make-number-node 0)))))

 (it "disallows bad if syntax"
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (elaborate-syntax-forms (at (location 5 23)
                                           (make-list-node
                                            (list (at (location 7 8)
                                                      (make-symbol-node 'if))
                                                  (at (location 9 10)
                                                      (make-symbol-node 'cond))
                                                  (at (location 11 12)
                                                      (make-symbol-node 'then)))))))
             "Bad `if` syntax, expected exactly three expressions - condition, then and else branches - to follow:")
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (elaborate-syntax-forms (at (location 5 23)
                                           (make-list-node
                                            (list (at (location 7 8)
                                                      (make-symbol-node 'if))
                                                  (at (location 9 10)
                                                      (make-symbol-node 'cond)))))))
             "Bad `if` syntax, expected exactly three expressions - condition, then and else branches - to follow:")
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (elaborate-syntax-forms (at (location 5 23)
                                           (make-list-node
                                            (list (at (location 7 8)
                                                      (make-symbol-node 'if)))))))
             "Bad `if` syntax, expected exactly three expressions - condition, then and else branches - to follow:")
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (elaborate-syntax-forms (at (location 5 23)
                                           (make-list-node
                                            (list (at (location 7 8)
                                                      (make-symbol-node 'if))
                                                  (at (location 9 10)
                                                      (make-symbol-node 'cond))
                                                  (at (location 11 12)
                                                      (make-symbol-node 'then))
                                                  (at (location 11 12)
                                                      (make-symbol-node 'else))
                                                  (at (location 11 12)
                                                      (make-symbol-node '???)))))))
             "Bad `if` syntax, expected exactly three expressions - condition, then and else branches - to follow:"))

 (it "elaborates valid dos"
     (assert (elaborate-syntax-forms (at (location 5 23)
                                         (make-list-node
                                          (list (make-symbol-node 'do)
                                                (make-number-node 23)
                                                (make-number-node 5)))))
             (at (location 5 23)
                 (make-do-node
                  (list (make-number-node 23)
                        (make-number-node 5))))))

 (it "disallows bad do syntax"
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (elaborate-syntax-forms (at (location 5 23)
                                           (make-list-node
                                            (list (at (location 7 13)
                                                      (make-symbol-node 'do)))))))
             "Bad `do` syntax, expected at least one expression to follow:"))

 (it "elaborates valid quotes"
     (assert (elaborate-syntax-forms (at (location 5 23)
                                         (make-list-node
                                          (list (make-symbol-node 'quote)
                                                (make-number-node 23)))))
             (at (location 5 23)
                 (make-quote-node
                  (make-number-node 23))))
     (assert (elaborate-syntax-forms (at (location 5 23)
                                         (make-list-node
                                          (list (make-symbol-node 'unquote)
                                                (make-number-node 23)))))
             (at (location 5 23)
                 (make-unquote-node
                  (make-number-node 23))))
     (assert (elaborate-syntax-forms (at (location 5 23)
                                         (make-list-node
                                          (list (make-symbol-node 'quasiquote)
                                                (make-number-node 23)))))
             (at (location 5 23)
                 (make-quasiquote-node
                  (make-number-node 23))))
     (assert (elaborate-syntax-forms (at (location 5 23)
                                         (make-list-node
                                          (list (make-symbol-node 'unquote-splicing)
                                                (make-number-node 23)))))
             (at (location 5 23)
                 (make-unquote-splicing-node
                  (make-number-node 23)))))

 (it "disallows bad quote syntax"
     (map (lambda (q)
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-syntax-forms (at (location 5 23)
                                                  (make-list-node
                                                   (list (at (location 7 13)
                                                             (make-symbol-node q))
                                                         (make-number-node 23)
                                                         (make-number-node 5))))))
                    (format "Bad `~a` syntax, expected exactly one expression to follow:" q))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-syntax-forms (at (location 5 23)
                                                  (make-list-node
                                                   (list (at (location 7 13)
                                                             (make-symbol-node q)))))))
                    (format "Bad `~a` syntax, expected exactly one expression to follow:" q)))
          (list 'quote 'quasiquote 'unquote 'unquote-splicing))))
