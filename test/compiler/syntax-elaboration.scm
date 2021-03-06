;; Parse tree validation tests.

(describe
 "syntax-elaboration"
 (it "`expand-quote` expands shorthand quotation correctly"
     (let ((loc (location 5 23))
           (num-loc (location 7 13)))
       (assert (expand-quote (make-number-node 23))
               (make-number-node 23))
       (assert (expand-quote (at loc
                                 (make-quote-node
                                  (at num-loc
                                      (make-number-node 23)))))
               (at loc
                   (generated
                    (make-list-node
                     (list (at loc
                               (generated (make-symbol-node 'quote)))
                           (at num-loc
                               (make-number-node 23)))))))
       (assert (expand-quote (at loc
                                 (make-quasiquote-node
                                  (at num-loc
                                      (make-number-node 23)))))
               (at loc
                   (generated
                    (make-list-node
                     (list (at loc
                               (generated (make-symbol-node 'quasiquote)))
                           (at num-loc
                               (make-number-node 23)))))))
       (assert (expand-quote (at loc
                                 (make-unquote-node
                                  (at num-loc
                                      (make-number-node 23)))))
               (at loc
                   (generated
                    (make-list-node
                     (list (at loc
                               (generated (make-symbol-node 'unquote)))
                           (at num-loc
                               (make-number-node 23)))))))
       (assert (expand-quote (at loc
                                 (make-unquote-splicing-node
                                  (at num-loc
                                      (make-number-node 23)))))
               (at loc
                   (generated
                    (make-list-node
                     (list (at loc
                               (generated (make-symbol-node 'unquote-splicing)))
                           (at num-loc
                               (make-number-node 23)))))))))
 (it "finds the expected errors"
     (map (lambda (filename)
            ;; NOTE Ignores the compilation abort.
            (with-handlers ((exn:fail? id))
              (test-file filename)))
          (filter (lambda (filename)
                    (string-suffix? filename ".foo"))
                  (map (lambda (path)
                         (string-append "../test/foof/errors/"
                                        (path->string path)))
                       (directory-list "../test/foof/errors/"))))))
