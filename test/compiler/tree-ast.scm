;; AST tests.

(describe
 "AST node"
 (it "allows setting and getting arbitrary properties"
     (assert (ast-get (ast-node 'value 23) 'value '())
             23)
     (assert (ast-get (ast-node 'value 23) 'other-value '())
             '())
     (assert (ast-set (ast-node 'value 23) 'other-value 5)
             (ast-node 'value 23 'other-value 5)))

 (it "`at` can add location"
     (assert (at (location 5 23)
                 (ast-node 'value 'value))
             (ast-node 'value 'value 'start 5 'end 23))
     (assert (at (location 5 23)
                 (ast-node 'value 'value 'start 10 'end 15))
             (ast-node 'value 'value 'start 5 'end 23)))

 (it "`generate` can mark node artificial"
     (assert (generated (ast-node 'value 'value))
             (ast-node 'value 'value 'generated #t))
     (assert (generated (ast-node 'value 'value 'generated #f))
             (ast-node 'value 'value 'generated #t)))

 (it "`at` preserves artificial state"
     (assert (at (location 5 23)
                 (generated (ast-node 'value 'value)))
             (ast-node 'value 'value 'start 5 'end 23 'generated #t))
     (assert (at (generated (location 5 23))
                 (ast-node 'value 'value))
             (ast-node 'value 'value 'start 5 'end 23 'generated #t))))

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
                        (if (equal? 'number (ast-get e 'type #f))
                            (ast-update e 'value (lambda (x) (* 2 x)))
                            e))
                      ast)
             (make-list-node (list (make-symbol-node 'foo)
                                   (make-number-node 46)))))

 (it "preserves property values"
     (define ast (make-list-node
                  (list (at (location 5 23)
                            (make-symbol-node 'foo))
                        (generated (make-number-node 23)))))
     (assert (map-ast id id ast)
             ast)))
