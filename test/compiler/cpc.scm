;; Continuation Passing Style Conversion

(define gen-simple-cpc-node
  (gen-one-of gen-valid-symbol-node gen-value-node))

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
     (check ((cond gen-simple-cpc-node)
             (then gen-simple-cpc-node)
             (else gen-simple-cpc-node)
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
                      (get-location node)))))

 (it "converts do correctly"
     (check ((one gen-simple-cpc-node)
             (two gen-simple-cpc-node)
             (three gen-simple-cpc-node)
             (node (gen-specific-do-node one two three)))
            (gensym-reset!)
            (assert (cpc node id) three))
     (check ((cond gen-simple-cpc-node)
             (then gen-simple-cpc-node)
             (else gen-simple-cpc-node)
             (one (gen-if-node cond then else))
             (two gen-simple-cpc-node)
             (three gen-simple-cpc-node)
             (node (gen-specific-do-node one two three)))
            (gensym-reset!)
            (let ((result (cpc node id)))
              (assert-ast result
                          (let ((binding 'cont1 (lambda ('value2) ,value)))
                            (if _
                                _
                                _))
                          (assert value three))
              (assert (get-location result)
                      (get-location one)))))

 (it "converts lambda correctly"
     (check ((arg gen-valid-symbol-node)
             (body gen-simple-cpc-node)
             (node (gen-lambda-node (list arg) body)))
            (gensym-reset!)
            (let ((result (cpc node id)))
              (assert-ast result
                          (lambda (,converted-arg (symbol 'cont1))
                            (primop-app '&yield-cont 'cont1 ,converted-body))
                          (assert converted-arg arg)
                          (assert converted-body body))
              (assert (get-location result)
                      (get-location node))))
     (check ((arg gen-valid-symbol-node)
             (then gen-simple-cpc-node)
             (else gen-simple-cpc-node)
             (body (gen-if-node arg then else))
             (node (gen-lambda-node (list arg) body)))
            (gensym-reset!)
            (let ((result (cpc node id)))
              (assert-ast result
                          (lambda (,converted-arg (symbol 'cont1))
                            (let ((binding 'cont2 (lambda ('value3)
                                                    (primop-app '&yield-cont 'cont1 'value3))))
                              (if ,cond
                                (primop-app '&yield-cont 'cont2 ,converted-then)
                                (primop-app '&yield-cont 'cont2 ,converted-else))))
                          (assert converted-arg arg)
                          (assert converted-then then)
                          (assert converted-else else))
              (assert (get-location result)
                      (get-location node)))))

 (it "converts primop-app correctly"
     (check ((op gen-valid-symbol-node)
             (args (gen-list (gen-integer 0 5) gen-simple-cpc-node))
             (node (apply gen-primop-app-node op args)))
            (gensym-reset!)
            (let ((result (cpc node id)))
              (assert-ast result
                          (primop-app ,converted-op . ,converted-args)
                          (assert converted-op op)
                          (assert converted-args args))
              (assert (get-location result)
                      (get-location node))))
     (check ((op1 gen-valid-symbol-node)
             (arg1 gen-simple-cpc-node)
             (app1 (gen-app-node op1 arg1))
             (op2 gen-valid-symbol-node)
             (app2 (gen-primop-app-node op2 app1)))
            (gensym-reset!)
            (let ((result (cpc app2 id)))
              (assert-ast result
                          (app ,converted-op1
                               ,converted-arg1
                               (lambda ('value1)
                                 (primop-app ,converted-op2 'value1)))
                          (assert converted-op1 op1)
                          (assert converted-op2 op2)
                          (assert converted-arg1 arg1))
              (assert (get-location result)
                      (get-location app1)))))

 (it "converts app correctly"
     (check ((op gen-simple-cpc-node)
             (arg gen-simple-cpc-node)
             (node (gen-app-node op arg)))
            (gensym-reset!)
            (let ((result (cpc node id)))
              (assert-ast result
                          (app ,converted-op ,converted-arg (lambda ('value1) 'value1))
                          (assert converted-op op)
                          (assert converted-arg arg))
              (assert (get-location result)
                      (get-location node))))
     (check ((op1 gen-simple-cpc-node)
             (arg1 gen-simple-cpc-node)
             (app1 (gen-app-node op1 arg1))
             (arg2 gen-simple-cpc-node)
             (app2 (gen-app-node app1 arg2)))
            (gensym-reset!)
            (let ((result (cpc app2 id)))
              (assert-ast result
                          (app ,converted-op1
                               ,converted-arg1
                               (lambda ('value2)
                                 (app 'value2 ,converted-arg2 (lambda ('value1) 'value1))))
                          (assert converted-op1 op1)
                          (assert converted-arg1 arg1)
                          (assert converted-arg2 arg2))
              (assert (get-location result)
                      (get-location app1))))
     (check ((op1 gen-simple-cpc-node)
             (arg1 gen-simple-cpc-node)
             (app1 (gen-app-node op1 arg1))
             (arg2 gen-simple-cpc-node)
             (app2 (gen-app-node app1 arg2))
             (op3 gen-simple-cpc-node)
             (app3 (gen-app-node op3 app2)))
            (gensym-reset!)
            (let ((result (cpc app3 id)))
              (assert-ast result
                          (app ,converted-op1
                               ,converted-arg1
                               (lambda ('value3)
                                 (app 'value3
                                      ,converted-arg2
                                      (lambda ('value2)
                                        (app ,converted-op3 'value2 (lambda ('value1) 'value1))))))
                          (assert converted-op1 op1)
                          (assert converted-op3 op3)
                          (assert converted-arg1 arg1)
                          (assert converted-arg2 arg2))
              (assert (get-location result)
                      (get-location app1))))
     (check ((op1 gen-simple-cpc-node)
             (arg1 gen-simple-cpc-node)
             (app1 (gen-app-node op1 arg1))
             (op2 gen-simple-cpc-node)
             (arg2 gen-simple-cpc-node)
             (app2 (gen-app-node op2 arg2))
             (app3 (gen-app-node app1 app2)))
            (gensym-reset!)
            (let ((result (cpc app3 id)))
              (assert-ast result
                          (app ,converted-op1
                               ,converted-arg1
                               (lambda ('value2)
                                 (app ,converted-op2
                                      ,converted-arg2
                                      (lambda ('value3)
                                        (app 'value2 'value3 (lambda ('value1) 'value1))))))
                          (assert converted-op1 op1)
                          (assert converted-op2 op2)
                          (assert converted-arg1 arg1)
                          (assert converted-arg2 arg2))
              (assert (get-location result)
                      (get-location app1))))))

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
