;; Binding propagation tests.

(describe
 "reconstruct-letrec-node"
 (it "should correctly recompute free & bound vars"
     (check ((body-fv (gen-list (gen-integer 3 5) gen-valid-symbol))
             (body (gen-with-fv gen-non-value-node body-fv))
             (let-bv (take body-fv 2))
             (vars (gen-specific-list gen-symbol-node let-bv))
             (vals (gen-specific-list (lambda (_)
                                        gen-non-value-node)
                                      let-bv))
             (vals-fv (gen-list (length let-bv) gen-valid-symbol))
             (bindings (map (lambda (sym var val fv)
                              (at (ast-node-location var)
                                  (set-ast-node-free-vars (set fv)
                                                          (set-ast-node-bound-vars (set sym)
                                                                                   (make-binding-node var val)))))
                            let-bv
                            vars
                            vals
                            vals-fv))
             (parent gen-ast-node)
             (expected-fv (append (drop body-fv 2)
                                  vals-fv)))
            (assert (reconstruct-letrec-node parent '() body)
                    body)
            (let ((result (reconstruct-letrec-node parent bindings body)))
              (assert (letrec-node? result))
              (assert (ast-node-location result) (ast-node-location parent))
              (assert (ast-node-bound-vars result) (apply set let-bv))
              (assert (ast-node-free-vars result) (apply set expected-fv))
              (assert (ast-letrec-body result) body)
              (assert (ast-letrec-bindings result) bindings)))))

(describe
 "reconstruct-let-node"
 (it "should correctly recompute free & bound vars"
     (check ((body-fv (gen-list (gen-integer 3 5) gen-valid-symbol))
             (body (gen-with-fv gen-complex-node body-fv))
             (let-bv (take body-fv 2))
             (vars (gen-specific-list gen-symbol-node let-bv))
             (vals (gen-specific-list (lambda (_)
                                        gen-complex-node)
                                      let-bv))
             (vals-fv (gen-list (length let-bv) gen-valid-symbol))
             (bindings (map (lambda (sym var val fv)
                              (at (ast-node-location var)
                                  (set-ast-node-free-vars (set fv)
                                                          (set-ast-node-bound-vars (set sym)
                                                                                   (make-binding-node var val)))))
                            let-bv
                            vars
                            vals
                            vals-fv))
             (parent gen-ast-node)
             (expected-fv (append (drop body-fv 2)
                                  vals-fv)))
            (assert (reconstruct-let-node parent '() body)
                    body)
            (let ((result (reconstruct-let-node parent bindings body)))
              (assert (let-node? result))
              (assert (ast-node-location result) (ast-node-location parent))
              (assert (ast-node-bound-vars result) (apply set let-bv))
              (assert (ast-node-free-vars result) (apply set expected-fv))
              (assert (ast-let-body result) body)
              (assert (ast-let-bindings result) bindings)))))

(describe
 "reconstruct-fix-node"
 (it "should correctly recompute free & bound vars"
     (check ((body-fv (gen-list (gen-integer 3 5) gen-valid-symbol))
             (body (gen-with-fv gen-non-value-node body-fv))
             (let-bv (take body-fv 2))
             (vars (gen-specific-list gen-symbol-node let-bv))
             (vals (gen-specific-list (lambda (_)
                                        gen-non-value-node)
                                      let-bv))
             (vals-fv (gen-list (length let-bv) gen-valid-symbol))
             (bindings (map (lambda (sym var val fv)
                              (at (ast-node-location var)
                                  (set-ast-node-free-vars (set fv)
                                                          (set-ast-node-bound-vars (set sym)
                                                                                   (make-binding-node var val)))))
                            let-bv
                            vars
                            vals
                            vals-fv))
             (parent gen-ast-node)
             (expected-fv (append (drop body-fv 2)
                                  vals-fv)))
            (assert (reconstruct-fix-node parent '() body)
                    body)
            (let ((result (reconstruct-fix-node parent bindings body)))
              (assert (fix-node? result))
              (assert (ast-node-location result) (ast-node-location parent))
              (assert (ast-node-bound-vars result) (apply set let-bv))
              (assert (ast-node-free-vars result) (apply set expected-fv))
              (assert (ast-fix-body result) body)
              (assert (ast-fix-bindings result) bindings)))))

(describe
 "partition-bindings"
 (it "should correctly partition lists"
     (assert (partition-bindings even? '())
             (cons '()
                   '()))
     (assert (partition-bindings even? '(0 1 2 3 4 5 6 7 8 9))
             (cons '(0 2 4 6 8)
                   '(1 3 5 7 9)))))

(define (test-propagate subs expr)
  (propagate (compose symbol-node? ast-binding-val)
             (lambda (bindings subs)
               (extend-subs (map (lambda (binding)
                                   (cons (ast-symbol-value (ast-binding-var binding))
                                         (make-number-node 23)))
                                 bindings)
                            subs))
             (lambda (subs expr kont)
               (ast-case
                expr
                ((symbol ,value)
                 (replace-sub subs (ast-symbol-value value) (constantly expr)))
                (else
                 (kont expr))))
             (make-subs subs)
             expr))

(describe
 "propagate"
 (it "should run the transform on AST nodes"
     (check ((var gen-valid-symbol)
             (needle (gen-symbol-node var))
             (node (gen-specific-do-node gen-valid-symbol-node
                                         needle
                                         gen-valid-symbol-node))
             (subbed (gen-number-node gen-number)))
            (assert-ast (test-propagate (list (cons var subbed))
                                        node)
                        (do ,unchanged-node1
                            (number ,x)
                          ,unchanged-node2)
                        (assert unchanged-node1 (car (ast-do-exprs node)))
                        (assert x subbed)
                        (assert unchanged-node2 (caddr (ast-do-exprs node))))))

 (it "should update the substitutions using bound variables"
     (check ((var1 gen-valid-symbol)
             (sym1 (gen-symbol-node var1))
             (var2 gen-valid-symbol)
             (sym2 (gen-symbol-node var2))
             (body (gen-specific-do-node sym1 sym2))
             (node (gen-with-bv (gen-lambda-node (list sym1) body)
                                (set var1)))
             (subbed (gen-number-node gen-number)))
            (assert-ast (test-propagate (list (cons var1 subbed)
                                              (cons var2 subbed))
                                        node)
                        (lambda (_)
                          (do ,unchanged-sym1
                              (number ,subbed-value)))
                        (assert unchanged-sym1 sym1)
                        (assert subbed-value subbed)))
     (check ((var1 gen-valid-symbol)
             (sym1 (gen-symbol-node var1))
             (b1 (gen-binding-node sym1 gen-valid-symbol-node))
             (var2 gen-valid-symbol)
             (sym2 (gen-symbol-node var2))
             (b2 (gen-binding-node sym2 gen-valid-symbol-node))
             (var3 gen-valid-symbol)
             (sym3 (gen-symbol-node var3))
             (val3 (gen-app-node sym2))
             (b3 (gen-binding-node sym3 val3))
             (body (gen-specific-do-node sym1 sym2 sym3))
             (node (gen-with-bv (gen-let-node (list b1 b2 b3) body)
                                (set var1 var2 var3)))
             (subbed (gen-number-node gen-number)))
            (assert-ast (test-propagate (list (cons var1 subbed))
                                        node)
                        (let ((binding _ (app ,unchanged-value)))
                          (do (number '23) ;; NOTE Not subbed.
                              (number '23)
                            ,unchanged-node))
                        (assert unchanged-value sym2)
                        (assert unchanged-node sym3)))
     (check ((var1 gen-valid-symbol)
             (sym1 (gen-symbol-node var1))
             (b1 (gen-binding-node sym1 gen-valid-symbol-node))
             (var2 gen-valid-symbol)
             (sym2 (gen-symbol-node var2))
             (b2 (gen-binding-node sym2 gen-valid-symbol-node))
             (var3 gen-valid-symbol)
             (sym3 (gen-symbol-node var3))
             (val3 (gen-app-node sym1))
             (b3 (gen-binding-node sym3 val3))
             (body (gen-specific-do-node sym1 sym2 sym3))
             (node (gen-with-bv (gen-letrec-node (list b1 b2 b3) body)
                                (set var1 var2 var3)))
             (subbed (gen-number-node gen-number)))
            (assert-ast (test-propagate (list (cons var1 subbed))
                                        node)
                        (letrec (,unchanged-b2
                                 (binding _ (app (number '23))))
                          (do (number '23) ;; NOTE Not subbed.
                              ,unchanged-sym2
                            ,unchanged-node))
                        (assert unchanged-b2 b2)
                        (assert unchanged-sym2 sym2)
                        (assert unchanged-node sym3)))
     (check ((var1 gen-valid-symbol)
             (sym1 (gen-symbol-node var1))
             (b1 (gen-binding-node sym1 gen-valid-symbol-node))
             (var2 gen-valid-symbol)
             (sym2 (gen-symbol-node var2))
             (b2 (gen-binding-node sym2 gen-valid-symbol-node))
             (var3 gen-valid-symbol)
             (sym3 (gen-symbol-node var3))
             (val3 (gen-app-node sym1))
             (b3 (gen-binding-node sym3 val3))
             (body (gen-specific-do-node sym1 sym2 sym3))
             ;; NOTE Technically not a proper fix node, but oh well.
             (node (gen-with-bv (gen-fix-node (list b1 b2 b3) body)
                                (set var1 var2 var3)))
             (subbed (gen-number-node gen-number)))
            (assert-ast (test-propagate (list (cons var1 subbed))
                                        node)
                        (fix (,unchanged-b2
                              (binding _ (app (number '23))))
                             (do (number '23) ;; NOTE Not subbed.
                                 ,unchanged-sym2
                               ,unchanged-node))
                        (assert unchanged-b2 b2)
                        (assert unchanged-sym2 sym2)
                        (assert unchanged-node sym3))))

 (it "should not leave empty let bindings in"
     (check ((var1 gen-valid-symbol)
             (sym1 (gen-symbol-node var1))
             (b1 (gen-binding-node sym1 gen-valid-symbol-node))
             (var2 gen-valid-symbol)
             (sym2 (gen-symbol-node var2))
             (b2 (gen-binding-node sym2 gen-valid-symbol-node))
             (body (gen-specific-do-node sym1 sym2))
             (node (gen-with-bv (gen-let-node (list b1 b2) body)
                                (set var1 var2)))
             (subbed (gen-number-node gen-number)))
            (assert-ast (test-propagate '() node)
                        (do (number '23) ;; NOTE Not subbed.
                            (number '23)))))

 (it "doesn't replace const values"
     (check ((var gen-valid-symbol)
             (sym (gen-symbol-node var))
             (node (gen-specific-const-node sym))
             (subbed gen-valid-symbol-node))
            (assert (test-propagate (list (cons var subbed))
                                    node)
                    node))))
