;; Parser tests.

(assert (parse "foo") 'foo)
(assert (parse "(define (foo x) 23)") '(define (foo x) 23))
(assert (parse "(define (foo x) ;; Coments should be removed!
                   #t)")
        '(define (foo x) #t))
