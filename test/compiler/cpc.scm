;; Continuation Passing Style Conversion

(describe
 "CPC conversion"
 (it "works for the simple cases"
     (check ((node gen-valid-symbol-node))
            (assert (cpc node id)
                    node))
     (check ((node gen-value-node))
            (assert (cpc node id)
                    node)))

 (it "converts if correctly"
     (check ((cond (gen-one-of gen-valid-symbol-node gen-value-node))
             (then (gen-one-of gen-valid-symbol-node gen-value-node))
             (else (gen-one-of gen-valid-symbol-node gen-value-node))
             (node (gen-if-node cond then else)))
            (gensym-reset!)
            (let ((result (cpc node id)))
              (assert-ast result
                          (let ((binding 'cont1 (lambda ('value2) 'value2)))
                            (if ,converted-cond
                                (primop-app '&yield-cont 'cont1 ,converted-then)
                                (primop-app '&yield-cont 'cont1 ,converted-else)))
                          (assert converted-cond cond)
                          (assert converted-then then)
                          (assert converted-else else))
              (assert (get-location result)
                      (get-location node))))))

(describe
 "old CPC conversion"
 (it "Simple CPC works."
     (assert (cp-convert 'foo id) 'foo)
     (assert (cp-convert '&structure-ref id) '&structure-ref)
     (assert (cp-convert 23 id) 23)
     (assert (cp-convert '() id) '())
     (assert (cp-convert "hurr" id) "hurr")
     (assert (cp-convert ''23 id) ''23))

 (it "CPCing lambda works."
     (gensym-reset!)
     (assert (cp-convert-lambda '(lambda (x) x) id)
             '(lambda (x cont1)
                (&yield-cont cont1 x)))
     (gensym-reset!)
     (assert (cp-convert-lambda '(lambda (x) (do 1 2 x)) id)
             '(lambda (x cont1)
                (do 1 2 (&yield-cont cont1 x))))
     (gensym-reset!)
     (assert (cp-convert-lambda '(lambda (x) (x)) id)
             '(lambda (x cont1)
                (x (lambda (value2)
                     (&yield-cont cont1 value2))))))

 (it "CPCing do works."
     (gensym-reset!)
     (assert (cp-convert-do '(do 1 2 3) id) '(do 1 2 3))
     (gensym-reset!)
     (assert (cp-convert-do '(do (foo 2) (bar 3)) id)
             '(foo 2 (lambda (value1)
                       (bar 3 (lambda (value2)
                                (do value1 value2)))))))

 (it "CPCing if works."
     (gensym-reset!)
     (assert (cp-convert-if '(if a b c) id)
             '(let ((cont1 (lambda (value2)
                             value2)))
                (if a
                    (&yield-cont cont1 b)
                    (&yield-cont cont1 c))))
     (gensym-reset!)
     (assert (cp-convert-if '(if (a b) c d) id)
             '(a
               b
               (lambda (value3)
                 (let ((cont1 (lambda (value2)
                                value2)))
                   (if value3
                       (&yield-cont cont1 c)
                       (&yield-cont cont1 d))))))
     (gensym-reset!)
     (assert (cp-convert-if '(if (a b) (c d) (e f)) id)
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
     (assert (cp-convert-let '(let ((a 23))
                                a)
                             id)
             '(let ((a 23))
                a))
     (gensym-reset!)
     (assert (cp-convert-let '(let ((a 23))
                                (do 23 a))
                             id)
             '(let ((a 23))
                (do 23 a))))

 (it "CPCing fix works."
     (gensym-reset!)
     (assert (cp-convert-fix '(fix ((foo (lambda () (foo))))
                                   (foo))
                             id)
             '(fix
               ((foo (lambda (cont1) (foo (lambda (value2) (&yield-cont cont1 value2))))))
               (foo (lambda (value3) value3))))
     (gensym-reset!)
     (assert (cp-convert-fix '(fix ((foo (lambda () (bar)))
                                    (bar (lambda () (foo))))
                                   (foo))
                             id)
             '(fix
               ((foo (lambda (cont1) (bar (lambda (value2) (&yield-cont cont1 value2)))))
                (bar (lambda (cont3) (foo (lambda (value4) (&yield-cont cont3 value4))))))
               (foo (lambda (value5) value5)))))

 (it "CPCing application works."
     (gensym-reset!)
     (assert (cp-convert-app '(a) id)
             '(a (lambda (value1)
                   value1)))
     (gensym-reset!)
     (assert (cp-convert-app '(a b) id)
             '(a b (lambda (value1)
                     value1)))
     (gensym-reset!)
     (assert (cp-convert-app '(a (b c)) id)
             '(b c (lambda (value2)
                     (a value2 (lambda (value1)
                                 value1)))))))
