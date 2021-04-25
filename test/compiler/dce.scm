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

 (it "should perform eta reduction"
     (check ((args (gen-arg-list (gen-integer 0 5)))
             (op gen-valid-symbol-node)
             (body (apply gen-app-node op args))
             (node (gen-lambda-node args body)))
            (assert (dead-code-ellimination node)
                    op))))
