;; Quasiquote expander tests.

(describe
 "expand-quasiquote"
 (it "should handle easy cases"
     (check ((node (gen-one-of gen-simple-node
                               gen-complex-node)))
            (assert (expand-quasiquote node) node))
     (check ((contents gen-value-node)
             (node (gen-quasiquote-node contents)))
            (assert (expand-quasiquote node)
                    contents))
     (check ((contents (gen-specific-list-node gen-valid-symbol-node
                                               gen-valid-symbol-node
                                               gen-valid-symbol-node))
             (node (gen-quasiquote-node contents)))
            (let ((result (expand-quasiquote node)))
              (assert-ast result
                          (primop-app 'cons
                                      (ast-quote first)
                                      (primop-app 'cons
                                                  (ast-quote second)
                                                  (primop-app 'cons
                                                              (ast-quote third)
                                                              (ast-quote (list)))))
                          (assert first (ast-list-nth contents 0))
                          (assert second (ast-list-nth contents 1))
                          (assert third (ast-list-nth contents 2)))
              (assert (generated? result)))))

 (it "should handle unquote"
     (check ((contents (gen-one-of gen-simple-node
                                   gen-complex-node))
             (unquoted (gen-unquote-node contents))
             (node (gen-quasiquote-node unquoted)))
            (assert (expand-quasiquote node) contents))
     (check ((contents (gen-one-of gen-simple-node
                                   gen-complex-node))
             (unquoted (gen-unquote-node contents))
             (lst (gen-specific-list-node gen-simple-node unquoted gen-simple-node))
             (node (gen-quasiquote-node lst)))
            (let ((result (expand-quasiquote node)))
              (assert-ast result
                          (primop-app 'cons
                                      _
                                      (primop-app 'cons
                                                  converted-contents
                                                  (primop-app 'cons
                                                              _
                                                              (ast-quote (list)))))
                          (assert converted-contents contents))
              (assert (generated? result)))))

 (it "should handle unquote-splicing"
     (check ((contents (gen-one-of gen-simple-node
                                   gen-complex-node))
             (unquoted (gen-unquote-splicing-node contents))
             (lst (gen-specific-list-node gen-simple-node unquoted gen-simple-node))
             (node (gen-quasiquote-node lst)))
            (let ((result (expand-quasiquote node)))
              (assert-ast result
                          (primop-app 'cons
                                      _
                                      (primop-app 'concat
                                                  converted-contents
                                                  (primop-app 'cons
                                                              _
                                                              (ast-quote (list)))))
                          (assert converted-contents contents))
              (assert (generated? result)))))

 (it "should reject unspliceable unquote-splicing"
     (check ((contents (gen-one-of gen-simple-node
                                   gen-complex-node))
             (unquoted (gen-unquote-splicing-node contents))
             (node (gen-quasiquote-node unquoted)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (expand-quasiquote node))
                    "Misplaced `unquote-splicing`, expected to be enclosed within a spliceable value:")))

 (it "should reject top-level unquotes"
     (check ((contents (gen-one-of gen-simple-node
                                   gen-complex-node))
             (node (gen-unquote-node contents)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (expand-quasiquote node))
                    "Misplaced `unquote`, expected to be enclosed within a `quasiquote`:"))
     (check ((contents (gen-one-of gen-simple-node
                                   gen-complex-node))
             (node (gen-unquote-splicing-node contents)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (expand-quasiquote node))
                    "Misplaced `unquote-splicing`, expected to be enclosed within a `quasiquote`:"))))
