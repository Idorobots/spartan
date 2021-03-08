;; Parse tree validation tests.

(describe
 "syntax-elaboration"
 (it "`expand-structure-refs` expands symbol into proper structure accessors"
     (let ((loc (location 5 23)))
       (assert (expand-structure-refs (make-symbol-node 'foo))
               (make-symbol-node 'foo))
       (assert (expand-structure-refs (at loc
                                          (make-structure-ref-node
                                           (list 'foo 'bar))))
               (at loc
                   (generated
                    (make-list-node
                     (list (at loc
                               (generated
                                (make-symbol-node '&structure-ref)))
                           (at loc
                               (make-symbol-node 'foo))
                           (at loc
                               (generated
                                (make-quote-node
                                 (at loc
                                     (make-symbol-node 'bar))))))))))
       (assert (expand-structure-refs (at loc
                                          (make-structure-ref-node
                                           (list 'foo 'bar 'baz 'faz))))
               (at loc
                   (generated
                    (make-list-node
                     (list (at loc
                               (generated
                                (make-symbol-node '&structure-ref)))
                           (at loc
                               (generated
                                (make-list-node
                                 (list (at loc
                                           (generated
                                            (make-symbol-node '&structure-ref)))
                                       (at loc
                                           (generated
                                            (make-list-node
                                             (list (at loc
                                                       (generated
                                                        (make-symbol-node '&structure-ref)))
                                                   (at loc
                                                       (make-symbol-node 'foo))
                                                   (at loc
                                                       (generated
                                                        (make-quote-node
                                                         (at loc
                                                             (make-symbol-node 'bar)))))))))
                                       (at loc
                                           (generated
                                            (make-quote-node
                                             (at loc
                                                 (make-symbol-node 'baz)))))))))
                           (at loc
                               (generated
                                (make-quote-node
                                 (at loc
                                     (make-symbol-node 'faz))))))))))))

 (it "finds the expected errors"
     (map (lambda (filename)
            (test-file filename))
          (filter (lambda (filename)
                    (string-suffix? filename ".foo"))
                  (map (lambda (path)
                         (string-append "../test/foof/errors/"
                                        (path->string path)))
                       (directory-list "../test/foof/errors/"))))))
