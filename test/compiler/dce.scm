;; DCE tests.

(describe
 "dead-code-ellimination"
 (it "should elliminated redundant lets"
     (check ((var gen-valid-symbol-node)
             (val gen-const-node)
             (b (gen-binding-node var val))
             (node (gen-let-node (list b) var)))
            (assert (dead-code-ellimination node)
                    val)))

 (it "should elliminate dead do statements"
     (check ((non-consts (gen-list (gen-integer 2 5) gen-valid-app-node))
             (symbols (gen-list (gen-integer 2 5) gen-valid-symbol-node))
             (consts (gen-list (gen-integer 2 5) gen-const-node))
             (node1 (apply gen-specific-do-node non-consts))
             (node2 (apply gen-specific-do-node consts))
             (node3 (apply gen-specific-do-node symbols)))
            (assert (dead-code-ellimination node1)
                    node1)
            (assert (dead-code-ellimination node2)
                    (last consts))
            (assert (dead-code-ellimination node3)
                    (last symbols)))
     (check ((const gen-const-node)
             (non-const gen-valid-app-node)
             (symbol gen-valid-symbol-node)
             (node1 (gen-specific-do-node const non-const symbol))
             (node2 (gen-specific-do-node symbol const non-const))
             (node3 (gen-specific-do-node symbol non-const const))
             (node4 (gen-specific-do-node non-const symbol const)))
            (assert-ast (dead-code-ellimination node1)
                        (do ,dce-non-const ,dce-symbol)
                        (assert dce-non-const non-const)
                        (assert dce-symbol symbol))
            (assert (dead-code-ellimination node2)
                    non-const)
            (assert-ast (dead-code-ellimination node3)
                        (do ,dce-non-const ,dce-const)
                        (assert dce-non-const non-const)
                        (assert dce-const const))
            (assert-ast (dead-code-ellimination node4)
                        (do ,dce-non-const ,dce-const)
                        (assert dce-non-const non-const)
                        (assert dce-const const))))

 (it "should elliminate dead if branches"
     (check ((then gen-valid-symbol-node)
             (els gen-valid-symbol-node)
             (cnd (gen-one-of gen-valid-symbol-node
                              gen-valid-let-node
                              gen-valid-letrec-node))
             (node (gen-if-node cnd then els)))
            (assert (dead-code-ellimination node)
                    node))
     (check ((then gen-valid-symbol-node)
             (els gen-valid-symbol-node)
             (cnd (gen-one-of (gen-specific-const-node gen-valid-symbol-node)
                              (gen-specific-const-node (gen-list-node (gen-integer 1 5)))
                              (gen-specific-const-node (gen-number-node gen-number))
                              (gen-specific-const-node (gen-string-node (gen-integer 0 20)))
                              gen-valid-lambda-node))
             (node (gen-if-node cnd then els)))
            (assert (dead-code-ellimination node)
                    then))
     (check ((then gen-valid-symbol-node)
             (els gen-valid-symbol-node)
             (cnd (gen-specific-const-node (gen-list-node 0)))
             (node (gen-if-node cnd then els)))
            (assert (dead-code-ellimination node)
                    els)))

 (it "should perform eta reduction"
     (check ((args (gen-arg-list (gen-integer 0 5)))
             (op gen-valid-symbol-node)
             (body (apply gen-app-node op args))
             (node (gen-lambda-node args body)))
            (assert (dead-code-ellimination node)
                    op))))

(describe
 "effectful?"
 (it "should recognize potentially effectful nodes"
     (check ((node (gen-one-of gen-const-node
                               gen-valid-symbol-node
                               gen-valid-lambda-node)))
            (assert (not (effectful? node))))
     (check ((node (gen-one-of gen-valid-do-node
                               gen-valid-if-node
                               gen-valid-app-node
                               gen-valid-primop-app-node
                               gen-valid-let-node
                               gen-valid-letrec-node)))
            (assert (effectful? node)))))

(describe
 "truthy? and falsy?"
 (it "falsy? should only recognize constant empty list as falsy"
     (assert (falsy? (sample (gen-specific-const-node
                              (gen-list-node 0))
                             random)))
     (check ((value (gen-one-of gen-valid-symbol-node
                                (gen-list-node (gen-integer 1 5))
                                (gen-number-node gen-number)
                                (gen-string-node (gen-integer 0 20))))
             (node (gen-specific-const-node value)))
            (assert (not (falsy? node))))
     (check ((node (gen-one-of gen-valid-lambda-node
                               gen-valid-symbol-node
                               gen-valid-do-node
                               gen-valid-if-node
                               gen-valid-let-node
                               gen-valid-letrec-node)))
            (assert (not (falsy? node)))))

 (it "truthy? should treat other static values as truthy"
     (assert (not (truthy? (sample (gen-specific-const-node
                                    (gen-list-node 0))
                                   random))))
     (check ((value (gen-one-of gen-valid-symbol-node
                                (gen-list-node (gen-integer 1 5))
                                (gen-number-node gen-number)
                                (gen-string-node (gen-integer 0 20))))
             (node (gen-one-of (gen-specific-const-node value)
                               gen-valid-lambda-node)))
            (assert (truthy? node)))
     (check ((node (gen-one-of gen-valid-do-node
                               gen-valid-symbol-node
                               gen-valid-if-node
                               gen-valid-let-node
                               gen-valid-letrec-node)))
            (assert (not (truthy? node))))))
