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
             '(&apply (&make-closure '()
                                     (lambda (env1 a) a))
                      a)))

 (it "Converting lambda works."
     (gensym-reset!)
     (assert (closure-convert '(lambda () 23) '())
             '(&make-closure '()
                             (lambda (env1) 23)))
     (gensym-reset!)
     (assert (closure-convert '(lambda (foo) foo) '())
             '(&make-closure '()
                             (lambda (env1 foo) foo)))
     (gensym-reset!)
     (assert (closure-convert '(lambda (foo) (+ foo 23)) '())
             '(&make-closure +
                             (lambda (env1 foo) (&apply env1 foo 23))))
     (gensym-reset!)
     (assert (closure-convert '(lambda (foo) (foo bar baz)) '())
             '(&make-closure (&cons bar baz)
                             (lambda (env1 foo)
                               (&apply foo (&car env1) (&cdr env1)))))
     (gensym-reset!)
     (assert (closure-convert '(lambda (x) (lambda (y) (+ x y))) '())
             '(&make-closure +
                             (lambda (env2 x)
                               (&make-closure (&cons env2 x)
                                              (lambda (env1 y)
                                                (&apply
                                                 (&car env1)
                                                 (&cdr env1)
                                                 y))))))
     (gensym-reset!)
     (assert (closure-convert '(lambda (n cont)
                                 (let ((c (lambda (v) (&yield-cont cont v))))
                                   (if n
                                       (&yield-cont c n)
                                       (&yield-cont c n))))
                              '())
             '(&make-closure '()
                             (lambda (env2 n cont)
                               (let ((c (&make-closure cont
                                                       (lambda (env1 v)
                                                         (&yield-cont env1 v)))))
                                 (if n
                                     (&yield-cont c n)
                                     (&yield-cont c n)))))))

 (it "Converting define works."
     (assert (closure-convert '(define k v) '()) '(define k v))
     (gensym-reset!)
     (assert (closure-convert '(define f (lambda (x) x)) '())
             '(define f (&make-closure '()
                                       (lambda (env1 x) x))))
     (gensym-reset!)
     (assert (closure-convert '(define f (lambda (x) f)) '())
             '(define f (&make-closure f
                                       (lambda (env1 x) env1)))))

 (it "Converting do works."
     (assert (closure-convert '(do a b c) '()) '(do a b c))
     (gensym-reset!)
     (assert (closure-convert '(do a (lambda (x) b) c) '())
             '(do a
                  (&make-closure b
                                 (lambda (env1 x) env1))
                c)))

 (it "Converting if works."
     (assert (closure-convert '(if a b c) '()) '(if a b c))
     (gensym-reset!)
     (assert (closure-convert '(if a (lambda (x) b) c) '())
             '(if a
                  (&make-closure b
                                 (lambda (env1 x) env1))
                  c)))

 (it "Converting let works."
     (assert (closure-convert '(let ((a b)) a) '()) '(let ((a b)) a))
     (gensym-reset!)
     (assert (closure-convert '(let ((a 23)) (lambda (x) a)) '())
             '(let ((a 23))
                (&make-closure a
                               (lambda (env1 x)
                                 env1))))
     (gensym-reset!)
     (assert (closure-convert '(let ((a (lambda (x) x))) (a 23)) '())
             '(let ((a (&make-closure '()
                                      (lambda (env1 x)
                                        x))))
                (&apply a 23))))

 (it "Converting letrec works."
     (assert (closure-convert '(letrec ((a b)) a) '()) '(letrec ((a b)) a))
     (gensym-reset!)
     (assert (closure-convert '(letrec ((a 23)) (lambda (x) a)) '())
             '(letrec ((a 23))
                (&make-closure a
                               (lambda (env1 x)
                                 env1))))
     (gensym-reset!)
     (assert (closure-convert '(letrec ((a (lambda (x) x))) (a 23)) '())
             '(letrec ((a (&make-closure '()
                                         (lambda (env1 x)
                                           x))))
                (&apply a 23))))

 (it "Converting fix works."
     (gensym-reset!)
     (assert (closure-convert '(fix ((foo (lambda () (foo))))
                                    (foo 23))
                              '())
             '(let ((env2 '()))
                (let ((foo (&make-closure env2
                                          (lambda (env1)
                                            (&apply env1)))))
                  (do (&set-closure-env! foo foo)
                      (&apply foo 23)))))
     (gensym-reset!)
     (assert (closure-convert '(fix ((foo (lambda () (bar)))
                                     (bar (lambda () (foo))))
                                    (foo))
                              '())
             '(let ((env3 '())
                    (env4 '()))
                (let ((foo (&make-closure env3
                                          (lambda (env1)
                                            (&apply env1))))
                      (bar (&make-closure env4
                                          (lambda (env2)
                                            (&apply env2)))))
                  (do (&set-closure-env! bar foo)
                      (&set-closure-env! foo bar)
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
              '())
             '(&make-closure
               fact
               (lambda (env2 n cont4)
                 (&apply
                  env2
                  env2
                  (&make-closure
                   '()
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
              '())
             '(let ((foo (&make-closure
                          '()
                          (lambda (env2 foo bar)
                            (&make-closure
                             (&cons bar foo)
                             (lambda (env1)
                               (let ((bar (&apply (&car env1) (&cdr env1) (&car env1))))
                                 bar))))))
                    (bar (&make-closure
                          '()
                          (lambda (env4 foo bar)
                            (&make-closure
                             (&cons bar foo)
                             (lambda (env3)
                               (let ((foo (&apply (&cdr env3) (&cdr env3) (&car env3))))
                                 foo)))))))
                (let ((foo (&apply foo foo bar))
                      (bar (&apply bar foo bar)))
                  (&apply list (&apply foo) (&apply bar)))))))
