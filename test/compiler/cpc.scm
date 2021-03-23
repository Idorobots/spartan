;; Continuation Passing Style Conversion

(describe
 "CPC conversion"
 (it "Simple CPC works."
     (assert (cpc 'foo id) 'foo)
     (assert (cpc '&structure-ref id) '&structure-ref)
     (assert (cpc 23 id) 23)
     (assert (cpc '() id) '())
     (assert (cpc "hurr" id) "hurr")
     (assert (cpc ''23 id) ''23))

 (it "CPCing lambda works."
     (gensym-reset!)
     (assert (cpc-lambda '(lambda (x) x) id)
             '(lambda (x cont1)
                (&yield-cont cont1 x)))
     (gensym-reset!)
     (assert (cpc-lambda '(lambda (x) (do 1 2 x)) id)
             '(lambda (x cont1)
                (do 1 2 (&yield-cont cont1 x))))
     (gensym-reset!)
     (assert (cpc-lambda '(lambda (x) (x)) id)
             '(lambda (x cont1)
                (x (lambda (value2)
                     (&yield-cont cont1 value2))))))

 (it "CPCing do works."
     (gensym-reset!)
     (assert (cpc-do '(do 1 2 3) id) '(do 1 2 3))
     (gensym-reset!)
     (assert (cpc-do '(do (foo 2) (bar 3)) id)
             '(foo 2 (lambda (value1)
                       (bar 3 (lambda (value2)
                                (do value1 value2)))))))

 (it "CPCing if works."
     (gensym-reset!)
     (assert (cpc-if '(if a b c) id)
             '(let ((cont1 (lambda (value2)
                             value2)))
                (if a
                    (&yield-cont cont1 b)
                    (&yield-cont cont1 c))))
     (gensym-reset!)
     (assert (cpc-if '(if (a b) c d) id)
             '(a
               b
               (lambda (value3)
                 (let ((cont1 (lambda (value2)
                                value2)))
                   (if value3
                       (&yield-cont cont1 c)
                       (&yield-cont cont1 d))))))
     (gensym-reset!)
     (assert (cpc-if '(if (a b) (c d) (e f)) id)
             '(a
               b
               (lambda (value3)
                 (let ((cont1 (lambda (value2)
                                value2)))
                   (if value3
                       (c d (lambda (value4)
                              (&yield-cont cont1 value4)))
                       (e f (lambda (value5)
                              (&yield-cont cont1 value5)))))))))

 (it "CPCing let works."
     (gensym-reset!)
     (assert (cpc-let '(let ((a 23))
                         a)
                      id)
             '(let ((a 23))
                a))
     (gensym-reset!)
     (assert (cpc-let '(let ((a 23))
                         (do 23 a))
                      id)
             '(let ((a 23))
                (do 23 a))))

 (it "CPCing fix works."
     (gensym-reset!)
     (assert (cpc-fix '(fix ((foo (lambda () (foo))))
                            (foo))
                      id)
             '(fix
               ((foo (lambda (cont1) (foo (lambda (value2) (&yield-cont cont1 value2))))))
               (foo (lambda (value3) value3))))
     (gensym-reset!)
     (assert (cpc-fix '(fix ((foo (lambda () (bar)))
                             (bar (lambda () (foo))))
                            (foo))
                      id)
             '(fix
               ((foo (lambda (cont1) (bar (lambda (value2) (&yield-cont cont1 value2)))))
                (bar (lambda (cont3) (foo (lambda (value4) (&yield-cont cont3 value4))))))
               (foo (lambda (value5) value5)))))

 (it "CPCing application works."
     (gensym-reset!)
     (assert (cpc-app '(a) id)
             '(a (lambda (value1)
                   value1)))
     (gensym-reset!)
     (assert (cpc-app '(a b) id)
             '(a b (lambda (value1)
                     value1)))
     (gensym-reset!)
     (assert (cpc-app '(a (b c)) id)
             '(b c (lambda (value2)
                     (a value2 (lambda (value1)
                                 value1)))))))
