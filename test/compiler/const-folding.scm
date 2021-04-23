;; Constant folding tests.

(describe
 "constant-folding"
 (it "should not fold non-const primop-apps"
     (check ((value gen-valid-symbol-node)
             (node (gen-primop-app-node 'car value)))
            (assert (constant-folding node)
                    node)))

  (it "should not fold non-foldable primop-apps"
     (check ((vals (gen-list (gen-integer 2 5) (gen-number-node gen-number)))
             (value (apply gen-specific-list-node vals))
             (const (gen-specific-const-node value))
             (node (gen-primop-app-node 'display const)))
            (assert (constant-folding node)
                    node)))

 (it "should not fold malformed primop-apps"
     (check ((value (gen-number-node gen-number))
             (const (gen-specific-const-node value))
             (node (gen-primop-app-node 'display const)))
            (assert (constant-folding node)
                    node)))

 (it "should fold properly constant primop-apps"
     (check ((vals (gen-list (gen-integer 2 5) (gen-number-node gen-number)))
             (value (apply gen-specific-list-node vals))
             (const (gen-specific-const-node value))
             (node (gen-primop-app-node 'car const)))
            (let ((result (constant-folding node)))
              (assert-ast result
                          (const ,converted-value)
                          (assert converted-value (car vals)))
              (assert (generated? result))
              (assert (get-location result)
                      (get-location node))))))
