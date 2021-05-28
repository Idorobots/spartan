;; Common subexpression elimination tests.

(define cse-able-primops '(* + - / < = car cdr cadr nil? zero?))

(describe
 "common-subexpression-elimination"
 (it "should eliminate redundant expressions"
     (check ((var1 gen-valid-symbol-node)
             (var2 gen-valid-symbol-node)
             (op (apply gen-one-of cse-able-primops))
             (sym1 gen-valid-symbol-node)
             (app1 (gen-primop-app-node op var1 var2))
             (b1 (gen-binding-node sym1 app1))
             (app2 (gen-primop-app-node op var1 var2))
             (b2 gen-valid-binding-node)
             (body (gen-let-node (list b2) app2))
             (node (gen-let-node (list b1) body)))
            (assert-ast (cse '() node)
                        (let ((binding cse-sym1-2 cse-app1))
                          (let _
                              cse-sym1))
                        #:when (ast-eqv? cse-sym1 cse-sym1-2)
                        (assert (ast-symbol-value cse-sym1) (ast-symbol-value sym1))
                        (assert (ast-node-location cse-sym1) (ast-node-location app2))
                        (assert cse-app1 app1))))

 (it "should be procedure-local"
     (check ((var1 gen-valid-symbol-node)
             (var2 gen-valid-symbol-node)
             (op (apply gen-one-of cse-able-primops))
             (sym1 gen-valid-symbol-node)
             (app1 (gen-primop-app-node op var1 var2))
             (b1 (gen-binding-node sym1 app1))
             (app2 (gen-primop-app-node op var1 var2))
             (args (gen-arg-list (gen-integer 0 5)))
             (body (gen-lambda-node args app2))
             (node (gen-let-node (list b1) body)))
            (assert-ast (cse '() node)
                        (let ((binding cse-sym1 cse-app1))
                          (lambda _
                            cse-app2))
                        (assert cse-sym1 sym1)
                        (assert cse-app1 app1)
                        (assert cse-app2 app2))))

 (it "should handle rebound variables"
     (check ((v1 gen-valid-symbol)
             (v2 gen-valid-symbol)
             (var1 (gen-symbol-node v1))
             (var2 (gen-symbol-node v2))
             (op (apply gen-one-of cse-able-primops))
             (s1 gen-valid-symbol)
             (sym1 (gen-symbol-node s1))
             (app1 (gen-primop-app-node op var1 var2))
             (b1 (gen-with-fv (gen-binding-node sym1 app1)
                              (set v1 v2)))
             (app2 (gen-primop-app-node op var1 var2))
             (b2 (gen-binding-node var1 gen-valid-symbol-node))
             (body (gen-with-bv (gen-let-node (list b2) app2)
                                (set v1)))
             (node (gen-with-bv (gen-let-node (list b1) body)
                                (set s1))))
            (assert-ast (cse '() node)
                        (let ((binding cse-sym1 cse-app1))
                          (let ((binding cse-var1 _))
                            cse-app2))
                        (assert cse-sym1 sym1)
                        (assert cse-app1 app1)
                        (assert cse-var1 var1)
                        (assert cse-app2 app2)))
     (check ((v1 gen-valid-symbol)
             (v2 gen-valid-symbol)
             (var1 (gen-symbol-node v1))
             (var2 (gen-symbol-node v2))
             (op (apply gen-one-of cse-able-primops))
             (s1 gen-valid-symbol)
             (sym1 (gen-symbol-node s1))
             (app1 (gen-primop-app-node op var1 var2))
             (b1 (gen-with-fv (gen-binding-node sym1 app1)
                              (set v1 v2)))
             (app2 (gen-primop-app-node op var1 var2))
             (b2 (gen-binding-node var1 gen-valid-symbol-node))
             (body (gen-with-bv (gen-letrec-node (list b2) app2)
                                (set v1)))
             (node (gen-with-bv (gen-let-node (list b1) body)
                                (set s1))))
            (assert-ast (cse '() node)
                        (let ((binding cse-sym1 cse-app1))
                          (letrec ((binding cse-var1 _))
                            cse-app2))
                        (assert cse-sym1 sym1)
                        (assert cse-app1 app1)
                        (assert cse-var1 var1)
                        (assert cse-app2 app2)))
     (check ((v gen-valid-symbol)
             (var (gen-symbol-node v))
             (op (apply gen-one-of cse-able-primops))
             (app (gen-primop-app-node op var var))
             (b (gen-with-fv (gen-binding-node var app)
                             (set v)))
             (node (gen-with-bv (gen-let-node (list b) app)
                                (set v))))
            (assert (cse '() node) node)))

 (it "should not optimize out letrec bindings"
     (check ((v1 gen-valid-symbol)
             (v2 gen-valid-symbol)
             (var1 (gen-symbol-node v1))
             (var2 (gen-symbol-node v2))
             (op (apply gen-one-of cse-able-primops))
             (s1 gen-valid-symbol)
             (sym1 (gen-symbol-node s1))
             (app1 (gen-primop-app-node op var1 var2))
             (b1 (gen-with-fv (gen-binding-node sym1 app1)
                              (set v1 v2)))
             (app2 (gen-primop-app-node op var1 var2))
             (node (gen-with-bv (gen-letrec-node (list b1) app2)
                                (set s1))))
            (assert-ast (cse '() node)
                        (letrec ((binding cse-sym1-2 cse-app1))
                          cse-sym1)
                        #:when (ast-eqv? cse-sym1 cse-sym1-2)
                        (assert cse-app1 app1)
                        (assert (ast-symbol-value cse-sym1) s1)
                        (assert (ast-node-location cse-sym1)
                                (ast-node-location app2))))))
