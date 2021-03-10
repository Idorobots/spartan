;; Closure conversion

(describe
 "closure-convert"
 (it "Simple cases work."
     (assert (closure-convert 'foo '()) 'foo)
     (assert (closure-convert 23 '()) 23)
     (assert (closure-convert '() '()) '())
     (assert (closure-convert "hurr" '()) "hurr")
     (assert (closure-convert '(quote foo) '()) '(quote foo)))

 (it "Converting application works."
     (assert (closure-convert '(a b c) '()) '(&apply a b c))
     (gensym-reset!)
     (assert (closure-convert '((lambda (a) a) a) '())
             '(&apply (&make-closure (&make-env)
                                     (lambda (env1 a) a))
                      a)))

 (it "Convertin lambda works."
     (gensym-reset!)
     (assert (closure-convert '(lambda () 23) '())
             '(&make-closure (&make-env)
                             (lambda (env1) 23)))
     (gensym-reset!)
     (assert (closure-convert '(lambda (foo) foo) '())
             '(&make-closure (&make-env)
                             (lambda (env1 foo) foo)))
     (gensym-reset!)
     (assert (closure-convert '(lambda (foo) (+ foo 23)) '(&apply))
             '(&make-closure (&make-env +)
                             (lambda (env1 foo) (&apply (&env-ref env1 0) foo 23))))
     (gensym-reset!)
     (assert (closure-convert '(lambda (foo) (+ foo 23)) '(+ &apply))
             '(&make-closure (&make-env)
                             (lambda (env1 foo) (+ foo 23))))
     (gensym-reset!)
     (assert (closure-convert '(lambda (foo) (foo bar baz)) '(&apply))
             '(&make-closure (&make-env bar baz)
                             (lambda (env1 foo)
                               (&apply foo (&env-ref env1 0) (&env-ref env1 1)))))
     (gensym-reset!)
     (assert (closure-convert '(lambda (x) (lambda (y) (+ x y))) '(&apply &env-ref &make-closure &make-env))
             '(&make-closure (&make-env +)
                             (lambda (env2 x)
                               (&make-closure (&make-env (&env-ref env2 0) x)
                                              (lambda (env1 y)
                                                (&apply
                                                 (&env-ref env1 0)
                                                 (&env-ref env1 1)
                                                 y))))))
     (gensym-reset!)
     (assert (closure-convert '(lambda (n cont)
                                 (let ((c (lambda (v) (&yield-cont cont v))))
                                   (if n
                                       (&yield-cont c n)
                                       (&yield-cont c n))))
                              (make-internal-applicatives))
             '(&make-closure (&make-env)
                             (lambda (env2 n cont)
                               (let ((c (&make-closure (&make-env cont)
                                                       (lambda (env1 v)
                                                         (&yield-cont (&env-ref env1 0) v)))))
                                 (if n
                                     (&yield-cont c n)
                                     (&yield-cont c n)))))))

 (it "Converting define works."
     (assert (closure-convert '(define k v) '()) '(define k v))
     (gensym-reset!)
     (assert (closure-convert '(define f (lambda (x) x)) '())
             '(define f (&make-closure (&make-env)
                                       (lambda (env1 x) x))))
     (gensym-reset!)
     (assert (closure-convert '(define f (lambda (x) f)) '())
             '(define f (&make-closure (&make-env f)
                                       (lambda (env1 x) (&env-ref env1 0))))))

 (it "Converting do works."
     (assert (closure-convert '(do a b c) '()) '(do a b c))
     (gensym-reset!)
     (assert (closure-convert '(do a (lambda (x) b) c) '())
             '(do a
                  (&make-closure (&make-env b)
                                 (lambda (env1 x) (&env-ref env1 0)))
                c)))

 (it "Converting if works."
     (assert (closure-convert '(if a b c) '()) '(if a b c))
     (gensym-reset!)
     (assert (closure-convert '(if a (lambda (x) b) c) '())
             '(if a
                  (&make-closure (&make-env b)
                                 (lambda (env1 x) (&env-ref env1 0)))
                  c)))

 (it "Converting let works."
     (assert (closure-convert '(let ((a b)) a) '()) '(let ((a b)) a))
     (gensym-reset!)
     (assert (closure-convert '(let ((a 23)) (lambda (x) a)) '(&apply))
             '(let ((a 23))
                (&make-closure (&make-env a)
                               (lambda (env1 x)
                                 (&env-ref env1 0)))))
     (gensym-reset!)
     (assert (closure-convert '(let ((a (lambda (x) x))) (a 23)) '(&apply))
             '(let ((a (&make-closure (&make-env)
                                      (lambda (env1 x)
                                        x))))
                (&apply a 23))))

 (it "Converting letrec works."
     (assert (closure-convert '(letrec ((a b)) a) '()) '(letrec ((a b)) a))
     (gensym-reset!)
     (assert (closure-convert '(letrec ((a 23)) (lambda (x) a)) '(&apply))
             '(letrec ((a 23))
                (&make-closure (&make-env a)
                               (lambda (env1 x)
                                 (&env-ref env1 0)))))
     (gensym-reset!)
     (assert (closure-convert '(letrec ((a (lambda (x) x))) (a 23)) '(&apply))
             '(letrec ((a (&make-closure (&make-env)
                                         (lambda (env1 x)
                                           x))))
                (&apply a 23))))

 (it "Converting fix works."
     (gensym-reset!)
     (assert (closure-convert '(fix ((foo (lambda () (foo))))
                                    (foo 23))
                              '(&apply))
             '(let ((env2 (&make-env '())))
                (let ((foo (&make-closure env2
                                          (lambda (env1)
                                            (&apply (&env-ref env1 0))))))
                  (do (&set-env! env2 0 foo)
                      (&apply foo 23)))))
     (gensym-reset!)
     (assert (closure-convert '(fix ((foo (lambda () (bar)))
                                     (bar (lambda () (foo))))
                                    (foo))
                              '(&apply))
             '(let ((env3 (&make-env '()))
                    (env4 (&make-env '())))
                (let ((foo (&make-closure env3
                                          (lambda (env1)
                                            (&apply (&env-ref env1 0)))))
                      (bar (&make-closure env4
                                          (lambda (env2)
                                            (&apply (&env-ref env2 0))))))
                  (do (&set-env! env4 0 foo)
                      (&set-env! env3 0 bar)
                    (&apply foo))))))

 (it "Complex examples work."
     (gensym-reset!)
     (assert (closure-convert
              '(lambda (n cont4)
                 (fact
                  fact
                  (lambda (value11)
                    (let ((fact value11))
                      23))))
              '(&yield-cont &make-env &apply &env-ref &make-closure))
             '(&make-closure
               (&make-env fact)
               (lambda (env2 n cont4)
                 (&apply
                  (&env-ref env2 0)
                  (&env-ref env2 0)
                  (&make-closure
                   (&make-env)
                   (lambda (env1 value11)
                     (let ((fact value11))
                       23)))))))
     (gensym-reset!)
     (assert (closure-convert
              '(let ((foo (lambda (foo bar)
                            (lambda ()
                              (let ((bar (bar foo bar)))
                                bar))))
                     (bar (lambda (foo bar)
                            (lambda ()
                              (let ((foo (foo foo bar)))
                                foo)))))
                 (let ((foo (foo foo bar))
                       (bar (bar foo bar)))
                   (list (foo) (bar))))
              '(&yield-cont &make-env &apply &env-ref &make-closure))
             '(let ((foo (&make-closure
                          (&make-env)
                          (lambda (env2 foo bar)
                            (&make-closure
                             (&make-env foo)
                             (lambda (env1)
                               (let ((bar (&apply bar (&env-ref env1 0) bar)))
                                 bar))))))
                    (bar (&make-closure
                          (&make-env)
                          (lambda (env4 foo bar)
                            (&make-closure
                             (&make-env bar)
                             (lambda (env3)
                               (let ((foo (&apply foo foo (&env-ref env3 0)))) foo)))))))
                (let ((foo (&apply foo foo bar))
                      (bar (&apply bar foo bar)))
                  (&apply list (&apply foo) (&apply bar)))))))
