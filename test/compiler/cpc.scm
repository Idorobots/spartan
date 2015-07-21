;; Continuation Passing Style Conversion

;; Simple CPC works.
(assert (cpc-simple 'foo) (symbol->safe 'foo))
(assert (cpc 'foo id) (symbol->safe 'foo))

(assert (cpc-simple 23) 23)
(assert (cpc 23 id) 23)

(assert (cpc-simple '()) '())
(assert (cpc '() id) '())

(assert (cpc-simple "hurr") "hurr")
(assert (cpc "hurr" id) "hurr")

(assert (cpc-simple '(quote 23)) ''23)
(assert (cpc ''23 id) ''23)

;; CPCing lambda works.
(gensym-reset!)
(assert (cpc-lambda '(lambda (x) x))
        '(lambda (__x __cont1)
           (&yield-cont __cont1 __x)))

(gensym-reset!)
(assert (cpc-lambda '(lambda (x) 1 2 x))
        '(lambda (__x __cont1)
           (do 1 2 (&yield-cont __cont1 __x))))

(gensym-reset!)
(assert (cpc-lambda '(lambda (x) (x)))
        '(lambda (__x __cont1)
           (__x (lambda (__value2)
                  (&yield-cont __cont1 __value2)))))

;; CPCing define works.
(gensym-reset!)
(assert (cpc-define '(define x 23) id) '(define __x 23))

(gensym-reset!)
(assert (cpc-define '(define x (foo 23)) id)
        '(define __x (__foo 23 (lambda (__value1) __value1))))

;; CPCing do works.
(gensym-reset!)
(assert (cpc-do '(do 1 2 3) id) '(do 1 2 3))

(gensym-reset!)
(assert (cpc-do '(do (foo 2) (bar 3)) id)
        '(__foo 2 (lambda (__value1)
                    (__bar 3 (lambda (__value2)
                               (do __value1 __value2))))))

;; CPCing if works.
(gensym-reset!)
(assert (cpc-if '(if a b c) id)
        '(let ((__cont1 (lambda (__value2)
                          __value2)))
           (if __a
               (&yield-cont __cont1 __b)
               (&yield-cont __cont1 __c))))

(gensym-reset!)
(assert (cpc-if '(if (a b) c d) id)
        '(__a
          __b
          (lambda (__value3)
            (let ((__cont1 (lambda (__value2)
                             __value2)))
              (if __value3
                  (&yield-cont __cont1 __c)
                  (&yield-cont __cont1 __d))))))

(gensym-reset!)
(assert (cpc-if '(if (a b) (c d) (e f)) id)
        '(__a
          __b
          (lambda (__value3)
            (let ((__cont1 (lambda (__value2)
                             __value2)))
              (if __value3
                  (__c __d (lambda (__value4)
                             (&yield-cont __cont1 __value4)))
                  (__e __f (lambda (__value5)
                             (&yield-cont __cont1 __value5))))))))

;; CPCing letrec works.
(gensym-reset!)
(assert (cpc-letrec '(letrec ((hcf (lambda (x) (hcf x))))
                       hcf)
                    id)
        '(let ((__hcf '()))
           (do (set!
                __hcf
                (lambda (__x __cont1)
                  (__hcf __x (lambda (__value2) (&yield-cont __cont1 __value2)))))
               __hcf)))

(gensym-reset!)
(assert (cpc-letrec '(letrec ((fact (lambda (n)
                                      (if (< n 2)
                                          n
                                          (* n (fact (- n 1)))))))
                       (fact 10))
                    id)
        '(let ((__fact '()))
           (do (set!
                __fact
                (lambda (__n __cont1)
                  (__LESS
                   __n
                   2
                   (lambda (__value4)
                     (let ((__cont2
                            (lambda (__value3) (&yield-cont __cont1 __value3))))
                       (if __value4
                           (&yield-cont __cont2 __n)
                           (___
                            __n
                            1
                            (lambda (__value7)
                              (__fact
                               __value7
                               (lambda (__value6)
                                 (__MULT
                                  __n
                                  __value6
                                  (lambda (__value5)
                                    (&yield-cont __cont2 __value5)))))))))))))
               (__fact 10 (lambda (__value8) __value8)))))

;; NOTE I don't even...
(gensym-reset!)
(assert (cpc-letrec '(letrec ((even? (lambda (x) (if (= 0 x) 't (odd? (- x 1)))))
                              (odd? (lambda (x) (if (= 0 x) 'n (even? (- x 1))))))
                       (even? 7))
                    id)
        '(let ((__evenQUEST '()) (__oddQUEST '()))
           (do (set!
                __evenQUEST
                (lambda (__x __cont1)
                  (__EQUAL
                   0
                   __x
                   (lambda (__value4)
                     (let ((__cont2
                            (lambda (__value3) (&yield-cont __cont1 __value3))))
                       (if __value4
                           (&yield-cont __cont2 't)
                           (___
                            __x
                            1
                            (lambda (__value6)
                              (__oddQUEST
                               __value6
                               (lambda (__value5)
                                 (&yield-cont __cont2 __value5)))))))))))
               (set!
                __oddQUEST
                (lambda (__x __cont7)
                  (__EQUAL
                   0
                   __x
                   (lambda (__value10)
                     (let ((__cont8
                            (lambda (__value9) (&yield-cont __cont7 __value9))))
                       (if __value10
                           (&yield-cont __cont8 'n)
                           (___
                            __x
                            1
                            (lambda (__value12)
                              (__evenQUEST
                               __value12
                               (lambda (__value11)
                                 (&yield-cont __cont8 __value11)))))))))))
             (__evenQUEST 7 (lambda (__value13) __value13)))))

;; CPCing application works.
(gensym-reset!)
(assert (cpc-app '(a) id)
        '(__a (lambda (__value1)
                __value1)))

(gensym-reset!)
(assert (cpc-app '(a b) id)
        '(__a __b (lambda (__value1)
                    __value1)))

(gensym-reset!)
(assert (cpc-app '(a (b c)) id)
        '(__b __c (lambda (__value2)
                    (__a __value2 (lambda (__value1)
                                    __value1)))))

;; TODO CPCing letcc works.
;; TODO CPCing shift/reset works.
