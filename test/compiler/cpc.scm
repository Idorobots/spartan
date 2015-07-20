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

;; CPCing lambda works
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

;; CPCing define works
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

;; CPCing if works
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
