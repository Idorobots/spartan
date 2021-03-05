;; Parse tree validation tests.

(describe
 "syntax-elaboration"
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
