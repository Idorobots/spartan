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
        '(lambda (__x __ct1)
           (&yield-cont __ct1 __x)))

(gensym-reset!)
(assert (cpc-lambda '(lambda (x) 1 2 x))
        '(lambda (__x __ct1)
           (do 1 2 (&yield-cont __ct1 __x))))

(gensym-reset!)
(assert (cpc-lambda '(lambda (x) (x)))
        '(lambda (__x __ct1)
           (__x (lambda (__value2)
                  (&yield-cont __ct1 __value2)))))

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
