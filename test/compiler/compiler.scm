;; Parse tree validation tests.

(describe
 "compiler"
 (it "finds the expected errors"
     (map (lambda (filename)
            (test-file filename))
          (filter (lambda (filename)
                    (string-suffix? filename ".sprtn"))
                  (map (lambda (path)
                         (string-append "../test/sprtn/errors/"
                                        (path->string path)))
                       (directory-list "../test/sprtn/errors/")))))

 (it "optimizes the output"
     (gensym-reset!)
     (assert (compile (env 'module "optimize"
                           'input (slurp "../test/sprtn/math.sprtn")))
             '(display '(5 1462731 23)))
     (assert (compile (env 'module "optimize"
                           'input "(letrec ((fact (lambda (x)
                                                   (if (= 0 x)
                                                       1
                                                       (* x (fact (- x 1)))))))
                                    (fact 2))"))
             ''2)
     (assert (compile (env 'module "optimize"
                           'input "(letrec ((q (lambda () 8))
                                            (f (lambda (x) (+  x (q))))
                                            (r (lambda () (f (q))))
                                            (s (lambda () (+ (r) (f 2))))
                                            (g (lambda () (+ (r) (s))))
                                            (t (lambda () (g))))
                                     (t))"))
             ''42)))
