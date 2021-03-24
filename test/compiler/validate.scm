;; Validation tests

(describe
 "validation"
 (it "should disallow defs"
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (validate-ast (set)
                             (set)
                             (at (location 5 23)
                                 (make-def-node (make-symbol-node 'foo)
                                                (make-number-node 23)))))
             "Bad `define` syntax, not allowed in this context:"))

 (it "should respect the def context"
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (validate-ast (set)
                             (set)
                             (make-if-node (at (location 5 23)
                                               (context "Bad `module` syntax"
                                                        (make-def-node (make-symbol-node 'foo)
                                                                       (make-number-node 23))))
                                           (make-symbol-node 'true)
                                           (make-symbol-node 'false))))
             "Bad `module` syntax, not allowed in this context:"))

 (it "should disallow undefined variables"
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (validate-ast (set 'foo 'bar)
                             (set)
                             (make-do-node
                              (list (at (location 5 23)
                                        (make-symbol-node 'foo))
                                    (make-symbol-node 'bar)))))
             "Undefined variable `foo`:"))

 (it "should not report actual defined variables"
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (validate-ast (set 'foo 'bar)
                             (set)
                             (bound-vars (set 'foo)
                                         (free-vars (set 'bar)
                                                    (make-lambda-node (list (make-symbol-node 'foo))
                                                                      (free-vars
                                                                       (set 'foo 'bar)
                                                                       (make-do-node
                                                                        (list (make-symbol-node 'foo)
                                                                              (at (location 5 23)
                                                                                  (make-symbol-node 'bar))))))))))
             "Undefined variable `bar`:"))

 (it "should report unused variables"
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (validate-ast (set)
                             (set)
                             (bound-vars (set 'x)
                                         (make-lambda-node (list (at (location 5 23)
                                                                     (make-symbol-node 'x)))
                                                           (make-number-node 23)))))
             "Unused variable `x`, rename to `_` to avoid this error:"))

 (it "should not report unused `_`"
     (assert (validate-ast (set)
                           (set)
                           (bound-vars (set '_)
                                       (make-lambda-node (list (make-symbol-node '_))
                                                         (make-number-node 23))))
             (bound-vars (set '_)
                         (make-lambda-node (list (make-symbol-node '_))
                                           (make-number-node 23))))))
