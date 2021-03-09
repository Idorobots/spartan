;; AST tests.

(describe
 "AST node"
 (it "allows setting and getting arbitrary properties"
     (assert (ast-get (ast-node 'value 23) 'value)
             23)
     (assert (ast-set (ast-node 'value 23) 'other-value 5)
             (ast-node 'value 23 'other-value 5)))

 (it "`at` can add location"
     (assert (at (location 5 23)
                 (ast-node 'value 'value))
             (ast-node 'value 'value 'location (location 5 23)))
     (assert (at (location 5 23)
                 (ast-node 'value 'value 'location (location 10 15)))
             (ast-node 'value 'value 'location (location 5 23))))

 (it "`generate` can mark node artificial"
     (assert (generated (ast-node 'value 'value))
             (ast-node 'value 'value 'generated #t))
     (assert (generated (ast-node 'value 'value 'generated #f))
             (ast-node 'value 'value 'generated #t)))

 (it "`at` preserves artificial state"
     (assert (at (location 5 23)
                 (generated (ast-node 'value 'value)))
             (ast-node 'value 'value 'location (location 5 23) 'generated #t)))

 (it "`location<?` correctly compares locations"
     (assert (location<? (location 0 0)
                         (location 0 0))
             #f)
     (assert (location<? (location 0 0)
                         (location 5 0)))
     (assert (location<? (location 23 0)
                         (location 5 0))
             #f)
     (assert (location<? (location 0 23)
                         (location 5 23)))))

(describe
 "AST map"
 (it "maps various AST nodes"
     (define ast (make-list-node (list (make-symbol-node 'foo)
                                       (make-number-node 23))))
     (assert (map-ast id id ast)
             ast)
     (assert (map-ast (lambda (_)
                        (make-number-node 23))
                      id
                      ast)
             (make-number-node 23))
     (assert (map-ast id
                      (lambda (e)
                        (if (equal? 'number (ast-get e 'type))
                            (ast-update e 'value (lambda (x) (* 2 x)))
                            e))
                      ast)
             (make-list-node (list (make-symbol-node 'foo)
                                   (make-number-node 46))))
     (assert (map-ast id
                      (lambda (e)
                        (if (equal? 'number (ast-get e 'type))
                            (ast-update e 'value (lambda (x) (+ 2 x)))
                            e))
                      (make-if-node (make-number-node 23)
                                    (make-number-node 5)
                                    (make-number-node 0)))
             (make-if-node (make-number-node 25)
                           (make-number-node 7)
                           (make-number-node 2)))
     (assert (map-ast id
                      (lambda (e)
                        (if (equal? 'number (ast-get e 'type))
                            (ast-update e 'value (lambda (x) (+ 2 x)))
                            e))
                      (make-do-node
                       (list (make-number-node 23)
                             (make-number-node 5)
                             (make-number-node 0))))
             (make-do-node
              (list (make-number-node 25)
                    (make-number-node 7)
                    (make-number-node 2)))))

 (it "preserves property values"
     (define ast (make-list-node
                  (list (at (location 5 23)
                            (make-symbol-node 'foo))
                        (generated (make-number-node 23)))))
     (assert (map-ast id id ast)
             ast)))

(describe
 "ast-matches?"
 (it "underscore matches anything"
     (assert (ast-matches? (make-symbol-node 'foo) '_))
     (assert (ast-matches? (make-number-node 23) '_))
     (assert (ast-matches? (make-list-node (list)) '_)))

 (it "symbols match only the same symbol nodes"
     (assert (ast-matches? (make-symbol-node 'foo) 'foo))
     (assert (not (ast-matches? (make-symbol-node 'bar) 'foo)))
     (assert (not (ast-matches? (make-number-node 23) 'foo)))
     (assert (not (ast-matches? (make-list-node (list)) 'foo))))

 (it "lists map the same length list nodes"
     (assert (ast-list-matches? '() '()))
     (assert (ast-list-matches? (list (make-symbol-node 'foo)
                                      (make-symbol-node 'bar))
                                '(_ _)))
     (assert (not (ast-list-matches? (list (make-symbol-node 'foo)
                                           (make-symbol-node 'bar))
                                     '(_))))
     (assert (not (ast-list-matches? (list (make-symbol-node 'foo)
                                           (make-symbol-node 'bar))
                                     '(_ _ _))))
     (assert (ast-matches? (make-list-node
                            (list (make-symbol-node 'foo)
                                  (make-symbol-node 'bar)))
                           '(_ _)))
     (assert (ast-matches? (make-list-node '())
                           '()))
     (assert (not (ast-matches? (make-list-node
                                 (list (make-symbol-node 'foo)
                                       (make-symbol-node 'bar)))
                                '(_))))
     (assert (not (ast-matches? (make-list-node
                                 (list (make-symbol-node 'foo)
                                       (make-symbol-node 'bar)))
                                '(_ _ _))))
     (assert (not (ast-matches? (make-symbol-node 'foo)
                                '(_ _)))))

 (it "allows matching multiple subexpressions"
     (assert (ast-list-matches? (list (make-symbol-node 'foo)
                                      (make-symbol-node 'bar))
                                '(_ . _)))
     (assert (ast-list-matches? (list (make-symbol-node 'foo)
                                      (make-symbol-node 'bar)
                                      (make-symbol-node 'baz)
                                      (make-symbol-node 'faz))
                                '(_ . _)))
     (assert (ast-matches? (make-list-node
                            (list (make-symbol-node 'foo)
                                  (make-symbol-node 'bar)))
                           '(_ . _)))
     (assert (ast-matches? (make-list-node
                            (list (make-symbol-node 'foo)
                                  (make-symbol-node 'bar)
                                  (make-symbol-node 'baz)
                                  (make-symbol-node 'faz)))
                           '(_ . _)))
     (assert (ast-matches? (make-list-node
                            (list (make-symbol-node 'foo)))
                           '(_ . _)))
     (assert (not (ast-matches? (make-list-node '())
                                '(_ . _))))
     (assert (not (ast-matches? (make-symbol-node 'foo)
                                '(_ . _))))))
