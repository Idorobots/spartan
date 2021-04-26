;; Parse tree validation tests.

(describe
 "compiler"
 (it "finds the expected errors"
     (map (lambda (filename)
            (test-file filename))
          (filter (lambda (filename)
                    (string-suffix? filename ".foo"))
                  (map (lambda (path)
                         (string-append "../test/foof/errors/"
                                        (path->string path)))
                       (directory-list "../test/foof/errors/")))))

 (it "optimizes the output"
     (gensym-reset!)
     (assert (compile (env 'module "optimize"
                           'input (slurp "../test/foof/math.foo")))
             '(display '(5 1462731 23)))))
