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

 (it "CPCing define works."
     (gensym-reset!)
     (assert (cpc-define '(define x 23) id) '(define x 23))
     (gensym-reset!)
     (assert (cpc-define '(define x (foo 23)) id)
             '(define x (foo 23 (lambda (value1) value1)))))

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
                                 value1))))))

 (it "CPCing letcc works."
     (gensym-reset!)
     (assert (cpc-letcc '(letcc k k)
                        id)
             '(let ((cont3 (lambda (value1) value1)))
                (let ((k (lambda (value2 ignored4) (&yield-cont cont3 value2))))
                  (&yield-cont cont3 k))))
     (gensym-reset!)
     (assert (cpc-letcc '(letcc k (k 23))
                        id)
             '(let ((cont3 (lambda (value1) value1)))
                (let ((k (lambda (value2 ignored4) (&yield-cont cont3 value2))))
                  (k 23 (lambda (value5) (&yield-cont cont3 value5))))))
     (gensym-reset!)
     (assert (cpc '(+ 5 (letcc k (k 23)))
                  id)
             '(let ((cont4 (lambda (value2) (+ 5 value2 (lambda (value1) value1)))))
                (let ((k (lambda (value3 ignored5) (&yield-cont cont4 value3))))
                  (k 23 (lambda (value6) (&yield-cont cont4 value6)))))))

 (it "CPCing shift/reset works."
     (gensym-reset!)
     (assert (cpc-reset '(reset 23) id)
             '(let ((cont1 (lambda (value2) value2)))
                (do (&push-delimited-continuation! cont1)
                    ((&pop-delimited-continuation!) 23))))
     (gensym-reset!)
     (assert (cpc-reset '(reset (+ 23 5)) id)
             '(let ((cont1 (lambda (value2) value2)))
                (do (&push-delimited-continuation! cont1)
                    (+ 23 5 (lambda (value3) ((&pop-delimited-continuation!) value3))))))
     (gensym-reset!)
     (assert (cpc-shift '(shift k 23) id)
             '(let ((cont1 (lambda (value3) value3)))
                (let ((k (lambda (value4 cont2)
                           (do (&push-delimited-continuation! cont2)
                               (&yield-cont cont1 value4)))))
                  ((&pop-delimited-continuation!) 23))))
     (gensym-reset!)
     (assert (cpc-shift '(shift k (k 23)) id)
             '(let ((cont1 (lambda (value3) value3)))
                (let ((k (lambda (value4 cont2)
                           (do (&push-delimited-continuation! cont2)
                               (&yield-cont cont1 value4)))))
                  (k 23 (lambda (value5)
                          ((&pop-delimited-continuation!) value5))))))
     (gensym-reset!)
     (assert (cpc-shift '(shift k (+ 5 (k 23))) id)
             '(let ((cont1 (lambda (value3) value3)))
                (let ((k (lambda (value4 cont2)
                           (do (&push-delimited-continuation! cont2)
                               (&yield-cont cont1 value4)))))
                  (k 23 (lambda (value6)
                          (+ 5 value6 (lambda (value5)
                                        ((&pop-delimited-continuation!) value5))))))))
     (gensym-reset!)
     (assert (cpc '(reset (* 2 (shift k (k 4)))) id)
             '(let ((cont1 (lambda (value2) value2)))
                (do (&push-delimited-continuation! cont1)
                    (let ((cont4 (lambda (value6)
                                   (* 2 value6 (lambda (value3)
                                                 ((&pop-delimited-continuation!) value3))))))
                      (let ((k (lambda (value7 cont5)
                                 (do (&push-delimited-continuation! cont5)
                                     (&yield-cont cont4 value7)))))
                        (k 4 (lambda (value8)
                               ((&pop-delimited-continuation!) value8)))))))))

 (it "CPCing raise works."
     (gensym-reset!)
     (assert (cpc-raise '(raise 23) id)
             '(let ((handler3 (&error-handler)))
                (handler3 23
                          (lambda (value1 ignored2)
                            (do (&set-error-handler! handler3)
                                value1)))))
     (gensym-reset!)
     (assert (cpc-raise '(raise (* 2 2)) id)
             '(let ((handler3 (&error-handler)))
                (* 2
                   2
                   (lambda (value4)
                     (handler3 value4
                               (lambda (value1 ignored2)
                                 (do (&set-error-handler! handler3)
                                     value1)))))))
     (gensym-reset!)
     (assert (cpc '(* 2 (raise 2)) id)
             '(let ((handler4 (&error-handler)))
                (handler4 2
                          (lambda (value2 ignored3)
                            (do (&set-error-handler! handler4)
                                (* 2
                                   value2
                                   (lambda (value1)
                                     value1))))))))

 (it "CPCing handle works."
     (gensym-reset!)
     (assert (cpc-handle '(handle expr handler) id)
             '(let ((handler2 (&error-handler))
                    (cont1 (lambda (value3) value3)))
                (do (&set-error-handler!
                     (lambda (error5 restart4)
                       (do (&set-error-handler! handler2)
                           (handler error5 restart4 cont1))))
                    (do
                        (&set-error-handler! handler2)
                        (&yield-cont cont1 expr)))))
     (gensym-reset!)
     (assert (cpc-handle '(handle (* 2 2) handler) id)
             '(let ((handler2 (&error-handler))
                    (cont1 (lambda (value3) value3)))
                (do (&set-error-handler!
                     (lambda (error5 restart4)
                       (do (&set-error-handler! handler2)
                           (handler error5 restart4 cont1))))
                    (*
                     2
                     2
                     (lambda (value6)
                       (do (&set-error-handler! handler2)
                           (&yield-cont cont1 value6)))))))
     (gensym-reset!)
     (assert (cpc '(* 2 (handle expr handler)) id)
             '(let ((handler3 (&error-handler))
                    (cont2
                     (lambda (value4) (* 2 value4 (lambda (value1) value1)))))
                (do (&set-error-handler!
                     (lambda (error6 restart5)
                       (do (&set-error-handler! handler3)
                           (handler error6 restart5 cont2))))
                    (do
                        (&set-error-handler! handler3)
                        (&yield-cont cont2 expr)))))
     (gensym-reset!)
     (assert (cpc '(* 2 (handle expr handler)) id)
             '(let ((handler3 (&error-handler))
                    (cont2
                     (lambda (value4) (* 2 value4 (lambda (value1) value1)))))
                (do (&set-error-handler!
                     (lambda (error6 restart5)
                       (do (&set-error-handler! handler3)
                           (handler error6 restart5 cont2))))
                    (do
                        (&set-error-handler! handler3)
                        (&yield-cont cont2 expr)))))
     (gensym-reset!)
     (assert (cpc-handle '(handle expr (get handler)) id)
             '(let ((handler2 (&error-handler))
                    (cont1 (lambda (value3) value3)))
                (get
                 handler
                 (lambda (value6)
                   (do (&set-error-handler!
                        (lambda (error5 restart4)
                          (do (&set-error-handler! handler2)
                              (value6 error5 restart4 cont1))))
                       (do
                           (&set-error-handler! handler2)
                           (&yield-cont cont1 expr)))))))))
