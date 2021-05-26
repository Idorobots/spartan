;; DCE tests.

(describe
 "dead-code-elimination"
 (it "should eliminated redundant lets"
     (check ((var gen-valid-symbol-node)
             (val gen-const-node)
             (b (gen-binding-node var val))
             (node (gen-let-node (list b) var)))
            (assert (dce (set) node)
                    val)))

 (it "should eliminate dead do statements"
     (check ((non-consts (gen-list (gen-integer 2 5) gen-valid-app-node))
             (symbols (gen-list (gen-integer 2 5) gen-valid-symbol-node))
             (consts (gen-list (gen-integer 2 5) gen-const-node))
             (node1 (apply gen-specific-do-node non-consts))
             (node2 (apply gen-specific-do-node consts))
             (node3 (apply gen-specific-do-node symbols)))
            (assert (dce (set) node1)
                    node1)
            (assert (dce (set) node2)
                    (last consts))
            (assert (dce (set) node3)
                    (last symbols)))
     (check ((const gen-const-node)
             (non-const gen-valid-app-node)
             (symbol gen-valid-symbol-node)
             (node1 (gen-specific-do-node const non-const symbol))
             (node2 (gen-specific-do-node symbol const non-const))
             (node3 (gen-specific-do-node symbol non-const const))
             (node4 (gen-specific-do-node non-const symbol const)))
            (assert-ast (dce (set) node1)
                        (do ,dce-non-const ,dce-symbol)
                        (assert dce-non-const non-const)
                        (assert dce-symbol symbol))
            (assert (dce (set) node2)
                    non-const)
            (assert-ast (dce (set) node3)
                        (do ,dce-non-const ,dce-const)
                        (assert dce-non-const non-const)
                        (assert dce-const const))
            (assert-ast (dce (set) node4)
                        (do ,dce-non-const ,dce-const)
                        (assert dce-non-const non-const)
                        (assert dce-const const))))

 (it "should eliminate dead if branches"
     (check ((then gen-valid-symbol-node)
             (els gen-valid-symbol-node)
             (cnd gen-valid-app-node)
             (node (gen-if-node cnd then els)))
            (assert (dce (set) node)
                    node))
     (check ((then gen-valid-symbol-node)
             (els gen-valid-symbol-node)
             (cnd (gen-one-of (gen-specific-const-node gen-valid-symbol-node)
                              (gen-specific-const-node (gen-list-node (gen-integer 1 5))) ;; NOTE Needs at least 1 element.
                              (gen-specific-const-node (gen-number-node gen-number))
                              (gen-specific-const-node (gen-string-node (gen-integer 0 20)))
                              gen-valid-lambda-node))
             (node (gen-if-node cnd then els)))
            (assert (dce (set) node)
                    then))
     (check ((then gen-valid-symbol-node)
             (els gen-valid-symbol-node)
             (cnd (gen-specific-const-node (gen-list-node 0)))
             (node (gen-if-node cnd then els)))
            (assert (dce (set) node)
                    els)))

 (it "should eliminate unused variables"
     (check ((var1 gen-valid-symbol)
             (sym1 (gen-symbol-node var1))
             (val1 gen-const-node)
             (var2 gen-valid-symbol)
             (sym2 (gen-symbol-node var2))
             (val2 gen-const-node)
             (b1 (gen-with-bv (gen-binding-node sym1 val1)
                              (set var1)))
             (b2 (gen-with-bv (gen-binding-node sym2 val2)
                              (set var2)))
             (body (gen-with-fv gen-const-node
                                (set var2)))
             (node (gen-let-node (list b1 b2) body)))
            (assert-ast (dce (set) node)
                        (let ((binding ,converted-sym2 ,converted-val2))
                          ,converted-body)
                        (assert converted-sym2 sym2)
                        (assert converted-val2 val2)
                        (assert converted-body body)))
     (check ((var1 gen-valid-symbol)
             (sym1 (gen-symbol-node var1))
             (val1 gen-const-node)
             (b1 (gen-with-bv (gen-binding-node sym1 val1)
                              (set var1)))
             (body (gen-with-fv gen-const-node
                                (set)))
             (node (gen-let-node (list b1) body)))
            (assert (dce (set) node) body)))

 (it "should not eliminate unused effectful variables"
     (check ((var1 gen-valid-symbol)
             (sym1 (gen-symbol-node var1))
             (val1 gen-valid-app-node)
             (b1 (gen-with-bv (gen-binding-node sym1 val1)
                              (set var1)))
             (body (gen-with-fv gen-const-node
                                (set)))
             (node (gen-with-bv (gen-let-node (list b1) body)
                                (set var1))))
            (assert (dce (set) node) node)))

 (it "should perform eta reduction"
     (check ((args (gen-arg-list (gen-integer 0 5)))
             (formals (gen-specific-list gen-symbol-node (map ast-symbol-value args)))
             (op gen-valid-symbol-node)
             (body (apply gen-app-node op args))
             (node (gen-lambda-node formals body)))
            (assert (dce (set) node)
                    op)))

 (it "should not eta-reduce self-recursive functions"
     (check ((args (gen-arg-list (gen-integer 0 5)))
             (var gen-valid-symbol)
             (op (gen-symbol-node var))
             (body (gen-with-fv (apply gen-app-node op args)
                                (set var)))
             (fun (gen-lambda-node args body))
             (binding (gen-with-bv (gen-binding-node op fun)
                                   (set var)))
             (node (gen-let-node (list binding) body)))
            (assert-ast (dce (set) node)
                        (let ((binding ,converted-var ,converted-val))
                          ,converted-body)
                        (assert converted-var op)
                        (assert converted-val op)
                        (assert converted-body body)))
     (check ((args (gen-arg-list (gen-integer 0 5)))
             (var gen-valid-symbol)
             (op (gen-symbol-node var))
             (body (gen-with-fv (apply gen-app-node op args)
                                (set var)))
             (fun (gen-lambda-node args body))
             (binding (gen-with-fv-bv (gen-binding-node op fun)
                                      (set var)
                                      (set var)))
             (node (gen-with-bv (gen-letrec-node (list binding) body)
                                (set var))))
            (assert (dce (set) node)
                    node))
     (check ((args1 (gen-arg-list (gen-integer 0 5)))
             (var1 gen-valid-symbol)
             (op1 (gen-symbol-node var1))
             (var2 gen-valid-symbol)
             (op2 (gen-symbol-node var2))
             (body1 (gen-with-fv (apply gen-app-node op2 args1)
                                 (set var2)))
             (fun1 (gen-lambda-node args1 body1))
             (binding1 (gen-with-fv-bv (gen-binding-node op1 fun1)
                                       (set var2)
                                       (set var1)))
             (args2 (gen-arg-list (gen-integer 0 5)))
             (body2 (gen-with-fv (apply gen-app-node op1 args2)
                                 (set var1)))
             (fun2 (gen-lambda-node args2 body2))
             (binding2 (gen-with-fv-bv (gen-binding-node op2 fun2)
                                       (set var1)
                                       (set var2)))
             (node (gen-with-bv (gen-letrec-node (list binding1 binding2) body1)
                                (set var1 var2))))
            (assert (dce (set) node)
                    node)))

 (it "should perform eta reduction on continuations"
     (check ((args (gen-arg-list (gen-integer 0 5)))
             (formals (gen-specific-list gen-symbol-node (map ast-symbol-value args)))
             (cont gen-valid-symbol-node)
             (body (apply gen-primop-app-node '&yield-cont cont args))
             (node (gen-lambda-node formals body)))
            (assert (dce (set) node)
                    cont))))

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
