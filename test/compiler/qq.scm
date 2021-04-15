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
                                      (a-quote ,first)
                                      (primop-app 'cons
                                                  (a-quote ,second)
                                                  (primop-app 'cons
                                                              (a-quote ,third)
                                                              (a-quote (list)))))
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
             (list (gen-specific-list-node gen-simple-node unquoted gen-simple-node))
             (node (gen-quasiquote-node list)))
            (let ((result (expand-quasiquote node)))
              (assert-ast result
                          (primop-app 'cons
                                      _
                                      (primop-app 'cons
                                                  ,converted-contents
                                                  (primop-app 'cons
                                                              _
                                                              (a-quote (list)))))
                          (assert converted-contents contents))
              (assert (generated? result)))))

 (it "should handle unquote-splicing"
     (check ((contents (gen-one-of gen-simple-node
                                   gen-complex-node))
             (unquoted (gen-unquote-splicing-node contents))
             (list (gen-specific-list-node gen-simple-node unquoted gen-simple-node))
             (node (gen-quasiquote-node list)))
            (let ((result (expand-quasiquote node)))
              (assert-ast result
                          (primop-app 'cons
                                      _
                                      (primop-app 'concat
                                                  ,converted-contents
                                                  (primop-app 'cons
                                                              _
                                                              (a-quote (list)))))
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
                    "Misplaced `unquote-splicing`, expected to be enclosed within a `quasiquote`:")))

 (it "should turn quoted values into plain old data inside of quote"
     (check ((quoted (gen-one-of (gen-quote-node gen-simple-node)
                                 (gen-quasiquote-node gen-simple-node)
                                 (gen-unquote-node gen-simple-node)
                                 (gen-unquote-splicing-node gen-simple-node)))
             (list (gen-specific-list-node gen-simple-node quoted gen-simple-node))
             (node (gen-quote-node list)))
            (assert-ast (expand-quasiquote node)
                        (a-quote
                         (list ,simple-node1
                               (list ,quoted-symbol ,simple-node2)
                               ,simple-node3))
                        (assert simple-node1 (ast-list-nth list 0))
                        (assert (ast-symbol-value quoted-symbol) (get-type quoted))
                        (assert simple-node2 (ast-quoted-expr quoted))
                        (assert simple-node3 (ast-list-nth list 2))))))
