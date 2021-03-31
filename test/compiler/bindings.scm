;; Binding annotation tests

(describe
 "compute-complexity"
 (it "should recognize simple values"
     (check ((node gen-value-node))
            (assert (compute-complexity node)
                    'simple)))
 (it "should recognize lambdas"
     (check ((node gen-valid-lambda-node))
            (assert (compute-complexity node)
                    'lambda)))
 (it "should recognize complex expressions"
     (check ((node gen-non-value-node))
            (assert (compute-complexity node)
                    'complex))))
