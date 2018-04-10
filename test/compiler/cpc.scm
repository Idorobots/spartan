;; Continuation Passing Style Conversion

;; Simple CPC works.
(assert (cpc 'foo id) 'foo)
(assert (cpc '&structure-ref id) '&structure-ref)
(assert (cpc 23 id) 23)
(assert (cpc '() id) '())
(assert (cpc "hurr" id) "hurr")
(assert (cpc ''23 id) ''23)

;; CPCing lambda works.
(gensym-reset!)
(assert (cpc-lambda '(lambda (x) x) id)
        '(lambda (x cont1)
           (&yield-cont cont1 x)))

(gensym-reset!)
(assert (cpc-lambda '(lambda (x) 1 2 x) id)
        '(lambda (x cont1)
           (do 1 2 (&yield-cont cont1 x))))

(gensym-reset!)
(assert (cpc-lambda '(lambda (x) (x)) id)
        '(lambda (x cont1)
           (x (lambda (value2)
                  (&yield-cont cont1 value2)))))

;; CPCing define works.
(gensym-reset!)
(assert (cpc-define '(define x 23) id) '(define x 23))

(gensym-reset!)
(assert (cpc-define '(define x (foo 23)) id)
        '(define x (foo 23 (lambda (value1) value1))))

;; CPCing do works.
(gensym-reset!)
(assert (cpc-do '(do 1 2 3) id) '(do 1 2 3))

(gensym-reset!)
(assert (cpc-do '(do (foo 2) (bar 3)) id)
        '(foo 2 (lambda (value1)
                    (bar 3 (lambda (value2)
                               (do value1 value2))))))

;; CPCing if works.
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
                             (&yield-cont cont1 value5))))))))

;; TODO CPCing let works.

;; CPCing letrec works.
(gensym-reset!)
(assert (cpc-let make-letrec
                 '(letrec ((hcf (lambda (x) (hcf x))))
                    hcf)
                 id)
        '(letrec ((hcf '()))
           (do (set!
                hcf
                (lambda (x cont1)
                  (hcf x (lambda (value2) (&yield-cont cont1 value2)))))
               hcf)))

(gensym-reset!)
(assert (cpc-let make-letrec
                 '(letrec ((fact (lambda (n)
                                   (if (< n 2)
                                       n
                                       (* n (fact (- n 1)))))))
                    (fact 10))
                 id)
        '(letrec ((fact '()))
           (do (set!
                fact
                (lambda (n cont1)
                  (<
                   n
                   2
                   (lambda (value4)
                     (let ((cont2
                            (lambda (value3) (&yield-cont cont1 value3))))
                       (if value4
                           (&yield-cont cont2 n)
                           (-
                            n
                            1
                            (lambda (value7)
                              (fact
                               value7
                               (lambda (value6)
                                 (*
                                  n
                                  value6
                                  (lambda (value5)
                                    (&yield-cont cont2 value5)))))))))))))
               (fact 10 (lambda (value8) value8)))))

;; NOTE I don't even...
(gensym-reset!)
(assert (cpc-let make-letrec
                 '(letrec ((even? (lambda (x) (if (= 0 x) 't (odd? (- x 1)))))
                           (odd? (lambda (x) (if (= 0 x) 'n (even? (- x 1))))))
                    (even? 7))
                 id)
        '(letrec ((even? '()) (odd? '()))
           (do (set!
                even?
                (lambda (x cont1)
                  (=
                   0
                   x
                   (lambda (value4)
                     (let ((cont2
                            (lambda (value3) (&yield-cont cont1 value3))))
                       (if value4
                           (&yield-cont cont2 't)
                           (-
                            x
                            1
                            (lambda (value6)
                              (odd?
                               value6
                               (lambda (value5)
                                 (&yield-cont cont2 value5)))))))))))
               (set!
                odd?
                (lambda (x cont7)
                  (=
                   0
                   x
                   (lambda (value10)
                     (let ((cont8
                            (lambda (value9) (&yield-cont cont7 value9))))
                       (if value10
                           (&yield-cont cont8 'n)
                           (-
                            x
                            1
                            (lambda (value12)
                              (even?
                               value12
                               (lambda (value11)
                                 (&yield-cont cont8 value11)))))))))))
             (even? 7 (lambda (value13) value13)))))

;; CPCing application works.
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
                            value1)))))

;; TODO CPCing letcc works.
;; TODO CPCing shift/reset works.

;; CPCing raise works.
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
                                value1)))))))

;; CPCing handle works.
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
                      (&yield-cont cont1 expr)))))))
