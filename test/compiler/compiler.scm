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
             '(display '(5 1462731 23)))
     (assert (compile (env 'module "optimize"
                           'input "(letrec ((q (lambda () 8))
                                            (f (lambda (x) (+  x (q))))
                                            (r (lambda () (f (q))))
                                            (s (lambda () (+ (r) (f 2))))
                                            (g (lambda () (+ (r) (s))))
                                            (t (lambda () (g))))
                                     (t))"))
             ''42)))
