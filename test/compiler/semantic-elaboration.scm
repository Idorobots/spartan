;; Semantic elaboration.

(describe
 "expand-syntax-forms"
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
                    (format "Bad `~a` syntax, expected exactly 1 value to follow:" q))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-syntax-forms (at (location 5 23)
                                                  (make-list-node
                                                   (list (at (location 7 13)
                                                             (make-symbol-node q)))))))
                    (format "Bad `~a` syntax, expected exactly 1 value to follow:" q)))
          (list 'quote 'quasiquote 'unquote 'unquote-splicing))))
