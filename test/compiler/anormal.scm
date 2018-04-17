;; A-normal form conversion tests.

;; Simple expressions are unchanged.
(assert (normalize '23 id) '23)
(assert (normalize 'foo id) 'foo)
(assert (normalize '"bar" id) '"bar")
(assert (normalize ''23 id) ''23)

;; Normalizing application works.
(gensym-reset!)
(assert (normalize '(foo bar baz) id)
        '(let ((temp1 foo))
           (let ((temp2 bar))
             (let ((temp3 baz))
               (temp1 temp2 temp3)))))

(gensym-reset!)
(assert (normalize '(foo bar (bar baz)) id)
        '(let ((temp1 foo))
           (let ((temp2 bar))
             (let ((temp3 bar))
               (let ((temp4 baz))
                 (let ((temp5 (temp3 temp4)))
                   (temp1 temp2 temp5)))))))

;; Normalizing lambda works.
(gensym-reset!)
(assert (normalize '(lambda (x) (x 23)) id)
        '(lambda (x)
           (let ((temp1 x))
             (let ((temp2 23))
               (temp1 temp2)))))

;; Normalizing if works.
(gensym-reset!)
(assert (normalize '(if a b c) id)
        '(let ((temp1 a))
           (if temp1
               b
               c)))

(gensym-reset!)
(assert (normalize '(if a (b c) d) id)
        '(let ((temp1 a))
           (if temp1
               (let ((temp2 b))
                 (let ((temp3 c))
                   (temp2 temp3)))
               d)))

;; Normalizing do works.
(gensym-reset!)
(assert (normalize '(do a b c) id)
        '(let ((temp1 a))
           (let ((temp2 b))
             (let ((temp3 c))
               temp3))))

;; Normalizing define works.
(gensym-reset!)
(assert (normalize '(define n (foo bar)) id)
        '(define n
           (let ((temp1 foo))
             (let ((temp2 bar))
               (temp1 temp2)))))

(gensym-reset!)
(assert (normalize '(define n (lambda (foo) (foo bar))) id)
        '(define n
           (lambda (foo)
             (let ((temp1 foo))
               (let ((temp2 bar))
                 (temp1 temp2))))))

;; Normalizing let works.
(gensym-reset!)
(assert (normalize '(let ((foo (bar 23)))
                      (foo 5))
                   id)
        '(let ((foo (let ((temp1 bar))
                      (let ((temp2 23))
                        (temp1 temp2)))))
           (let ((temp3 foo))
             (let ((temp4 5))
               (temp3 temp4)))))

(gensym-reset!)
(assert (normalize '(let ((foo (bar 23))
                          (bar (foo 5)))
                      13)
                   id)
        '(let ((foo (let ((temp1 bar))
                      (let ((temp2 23))
                        (temp1 temp2))))
               (bar (let ((temp3 foo))
                      (let ((temp4 5))
                        (temp3 temp4)))))
           13))

;; Normalizing letrec works.
(gensym-reset!)
(assert (normalize '(letrec ((foo (bar 23))
                             (bar (foo 5)))
                      13)
                   id)
        '(letrec ((foo (let ((temp1 bar))
                         (let ((temp2 23))
                           (temp1 temp2))))
                  (bar (let ((temp3 foo))
                         (let ((temp4 5))
                           (temp3 temp4)))))
           13))

;; Normalizing letcc works.
(gensym-reset!)
(assert (normalize '(letcc k (k 23)) id)
        '(letcc k
                (let ((temp1 k))
                  (let ((temp2 23))
                    (temp1 temp2)))))

;; Normalizing reset/shift works.
(gensym-reset!)
(assert (normalize '(reset (foo bar)) id)
        '(reset (let ((temp1 foo))
                  (let ((temp2 bar))
                    (temp1 temp2)))))

(gensym-reset!)
(assert (normalize '(shift k (k 23)) id)
        '(shift k
                (let ((temp1 k))
                  (let ((temp2 23))
                    (temp1 temp2)))))

;; Normalizing handle/raise works.
(gensym-reset!)
(assert (normalize '(handle foo bar) id)
        '(let ((temp1 bar))
           (handle foo temp1)))

(gensym-reset!)
(assert (normalize '(handle (raise (foo 23)) handler) id)
        '(let ((temp1 handler))
           (handle
            (let ((temp2 foo))
              (let ((temp3 23))
                (let ((temp4 (temp2 temp3)))
                  (raise temp4))))
            temp1)))
