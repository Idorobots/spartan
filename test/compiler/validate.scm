;; Validation tests

(describe
 "validation"
 (it "should disallow undefined variables"
     (check ((symbols (gen-arg-list (gen-integer 1 5)))
             (undefined (apply set (map ast-symbol-value symbols)))
             (node (apply gen-specific-do-node symbols)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (validate-ast undefined (set) (set) node))
                    (format "Undefined variable `~a`:" (ast-symbol-value (car symbols))))))

 (it "should not report actual defined variables"
     (check ((args1 (gen-arg-list (gen-integer 1 3)))
             (args2 (gen-arg-list (gen-integer 1 3)))
             (fv1 (apply set (map ast-symbol-value args1)))
             (fv2 (apply set (map ast-symbol-value args2)))
             (undefined (set-union fv1 fv2))
             (body (gen-with-fv (apply gen-specific-do-node (append args1 args2))
                                undefined))
             (node (gen-with-fv-bv (gen-lambda-node args1 body)
                                   fv2
                                   fv1)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (validate-ast undefined (set) (set) node))
                    (format "Undefined variable `~a`:" (ast-symbol-value (car args2))))))

 (it "should report unused variables"
     (check ((args (gen-arg-list (gen-integer 1 5)))
             (bound (apply set (map ast-symbol-value args)))
             (body (apply gen-specific-do-node args))
             (node (gen-with-bv (gen-lambda-node args body)
                                bound)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (validate-ast (set) (set) (set) node))
                    (format "Unused variable `~a`, rename to `_` to avoid this error:" (ast-symbol-value (car args))))))

 (it "should not report unused `_`"
     (check ((args (gen-list (gen-integer 1 5) (gen-symbol-node '_)))
             (bound (apply set (map ast-symbol-value args)))
             (body (gen-number-node gen-number))
             (node (gen-with-bv (gen-lambda-node args body)
                                bound)))
            (assert (validate-ast (set) (set) (set) node)
                    node)))

 (it "should report variables used before definition"
     (check ((symbols (gen-arg-list (gen-integer 1 5)))
             (used-before-def (apply set (map ast-symbol-value symbols)))
             (node (apply gen-specific-do-node symbols)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (validate-ast (set) (set) used-before-def node))
                    (format "Variable `~a` used before its definition:" (ast-symbol-value (car symbols))))))

 (it "should not report lazy variables used before definition"
     (check ((symbols (gen-arg-list (gen-integer 1 5)))
             (used-before-def (apply set (map ast-symbol-value symbols)))
             (args (gen-arg-list (gen-integer 1 5)))
             (bound (apply set (map ast-symbol-value args)))
             (body (gen-with-fv (gen-number-node gen-number)
                                used-before-def))
             (node (gen-lambda-node args body)))
            (assert (validate-ast (set) (set) used-before-def node)
                    node))))
