;; Constant annotation tests.

(define (gen-non-const-node rand)
  (sample (gen-one-of gen-valid-symbol-node
                      (gen-app-node gen-valid-symbol-node gen-valid-symbol-node gen-valid-symbol-node)
                      (gen-app-node gen-valid-symbol-node gen-non-const-node)
                      (gen-if-node gen-valid-symbol-node gen-non-const-node gen-non-const-node))
          rand))

(describe
 "wrap-constants"
 (it "should wrap raw constants with a const node"
     (check ((unquoted (gen-one-of (gen-number-node gen-number)
                                   (gen-string-node (gen-text (gen-integer 0 50))))))
            (let ((result (wrap-constants unquoted)))
              (assert-ast result
                          (const ,expr)
                          (assert expr unquoted))
              (assert (generated? result))
              (assert (ast-node-location result)
                      (ast-node-location unquoted))))
     (check ((unquoted (gen-one-of (gen-number-node gen-number)
                                   (gen-string-node (gen-text (gen-integer 0 50)))))
             (quoted (gen-quote-node unquoted)))
            (let ((result (wrap-constants quoted)))
              (assert-ast result
                          (const ,expr)
                          (assert expr unquoted))
              (assert (generated? result))
              (assert (ast-node-location result)
                      (ast-node-location quoted)))))

 (it "should not wrap anything else"
     (check ((node gen-non-const-node))
            (assert (wrap-constants node)
                    node)))

 (it "should turn quoted values into plain old data inside of quote"
     (check ((non-const gen-non-const-node)
             (quoted (gen-one-of (gen-quote-node non-const)
                                 (gen-quasiquote-node non-const)
                                 (gen-unquote-node non-const)
                                 (gen-unquote-splicing-node non-const)))
             (list (gen-specific-list-node gen-non-const-node quoted gen-non-const-node))
             (node (gen-quote-node list)))
            (assert-ast (wrap-constants node)
                        (const
                         (list ,simple-node1
                               (list ,quoted-symbol ,simple-node2)
                               ,simple-node3))
                        (assert simple-node1 (ast-list-nth list 0))
                        (assert (ast-symbol-value quoted-symbol) (ast-node-type quoted))
                        (assert simple-node2 non-const)
                        (assert simple-node3 (ast-list-nth list 2))))))
