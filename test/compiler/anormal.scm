;; A-normal form conversion tests.

;; atomic? predicate works.
(assert (atomic? '23))
(assert (atomic? 'foo))
(assert (atomic? '"bar"))
(assert (atomic? '(quote (he he))))
(assert (atomic? '()))

;; Simple expressions are unchanged.
(assert (normalize '23 id) '23)
(assert (normalize 'foo id) 'foo)
(assert (normalize '"bar" id) '"bar")
(assert (normalize ''23 id) ''23)

;; Normalizing application works.
(gensym-reset!)
(assert (normalize '(foo bar baz) id)
        '(foo bar baz))

(gensym-reset!)
(assert (normalize '(foo bar (bar baz)) id)
        '(let ((temp1 (bar baz)))
           (foo bar temp1)))

;; Normalizing lambda works.
(gensym-reset!)
(assert (normalize '(lambda (x) x) id)
        '(lambda (x) x))

(gensym-reset!)
(assert (normalize '(lambda (x) (x 23)) id)
        '(lambda (x)
           (x 23)))

(gensym-reset!)
(assert (normalize '(lambda (x) ((x 23))) id)
        '(lambda (x)
           (let ((temp1 (x 23)))
             (temp1))))

;; Normalizing if works.
(gensym-reset!)
(assert (normalize '(if a b c) id)
        '(if a b c))

(gensym-reset!)
(assert (normalize '(if a (b c) d) id)
        '(if a (b c) d))

(gensym-reset!)
(assert (normalize '(if a ((b c) d) e) id)
        '(if a
             (let ((temp1 (b c)))
               (temp1 d))
             e))

;; Normalizing do works.
(gensym-reset!)
(assert (normalize '(do a b c) id)
        'c)

(gensym-reset!)
(assert (normalize '(do (a) (b) (c)) id)
        '(let ((temp1 (a)))
           (let ((temp2 (b)))
             (let ((temp3 (c)))
               temp3))))

;; Normalizing define works.
(gensym-reset!)
(assert (normalize '(define n (foo bar)) id)
        '(define n (foo bar)))

(gensym-reset!)
(assert (normalize '(define n ((foo bar) baz)) id)
        '(define n (let ((temp1 (foo bar)))
                     (temp1 baz))))

(gensym-reset!)
(assert (normalize '(define n (lambda (foo) (foo bar))) id)
        '(define n
           (lambda (foo)
             (foo bar))))

(gensym-reset!)
(assert (normalize '(define n (lambda (foo) ((foo bar) baz))) id)
        '(define n
           (lambda (foo)
             (let ((temp1 (foo bar)))
               (temp1 baz)))))

(gensym-reset!)
(assert (normalize '((lambda (x) x)
                     (define x 23))
                   id)
        '(let ((temp1 (lambda (x) x)))
           (let ((temp2 (define x 23)))
             (temp1 temp2))))

;; Normalizing let works.
(gensym-reset!)
(assert (normalize '(let ((foo (bar 23)))
                      (foo 5))
                   id)
        '(let ((foo (bar 23)))
           (foo 5)))

(gensym-reset!)
(assert (normalize '(let ((foo (bar (+ 23 5))))
                      (foo 5))
                   id)
        '(let ((foo (let ((temp1 (+ 23 5)))
                      (bar temp1))))
           (foo 5)))

(gensym-reset!)
(assert (normalize '(let ((foo (bar 23))
                          (bar (foo 5)))
                      13)
                   id)
        '(let ((foo (bar 23))
               (bar (foo 5)))
           13))

(gensym-reset!)
(assert (normalize '(let ((foo (bar 23))
                          (bar (bar (foo 5))))
                      13)
                   id)
        '(let ((foo (bar 23))
               (bar (let ((temp1 (foo 5)))
                      (bar temp1))))
           13))

(gensym-reset!)
(assert (normalize '((lambda (x) x)
                     (let ((x 23))
                       x))
                   id)
        '(let ((temp1 (lambda (x) x)))
           (let ((temp2 (let ((x 23))
                          x)))
             (temp1 temp2))))

;; Normalizing letrec works.
(gensym-reset!)
(assert (normalize '(letrec ((foo (bar 23))
                             (bar (foo 5)))
                      13)
                   id)
        '(letrec ((foo (bar 23))
                  (bar (foo 5)))
           13))

(gensym-reset!)
(assert (normalize '(letrec ((foo (bar 23))
                             (bar (bar (foo 5))))
                      13)
                   id)
        '(letrec ((foo (bar 23))
                  (bar (let ((temp1 (foo 5)))
                         (bar temp1))))
           13))

(gensym-reset!)
(assert (normalize '((lambda (x) x)
                     (letrec ((x 23))
                       x))
                   id)
        '(let ((temp1 (lambda (x) x)))
           (let ((temp2 (letrec ((x 23))
                          x)))
             (temp1 temp2))))

;; Normalizing letcc works.
(gensym-reset!)
(assert (normalize '(letcc k (k 23)) id)
        '(letcc k
                (k 23)))

(gensym-reset!)
(assert (normalize '(letcc k (k (k 23))) id)
        '(letcc k
                (let ((temp1 (k 23)))
                  (k temp1))))

;; Normalizing reset/shift works.
(gensym-reset!)
(assert (normalize '(reset (foo bar)) id)
        '(reset (foo bar)))

(gensym-reset!)
(assert (normalize '(reset ((foo bar) baz)) id)
        '(reset (let ((temp1 (foo bar)))
                  (temp1 baz))))

(gensym-reset!)
(assert (normalize '(shift k (k 23)) id)
        '(shift k (k 23)))

(gensym-reset!)
(assert (normalize '(shift k (k (k 23))) id)
        '(shift k (let ((temp1 (k 23)))
                    (k temp1))))

;; Normalizing handle/raise works.
(gensym-reset!)
(assert (normalize '(handle foo bar) id)
        '(handle foo bar))

(gensym-reset!)
(assert (normalize '(handle (foo bar) bar) id)
        '(handle (foo bar) bar))

(gensym-reset!)
(assert (normalize '(handle ((foo bar) baz) bar) id)
        '(handle (let ((temp1 (foo bar)))
                   (temp1 baz))
                 bar))

(gensym-reset!)
(assert (normalize '(handle bar (foo bar)) id)
        '(let ((temp1 (foo bar)))
           (handle bar temp1)))

(gensym-reset!)
(assert (normalize '(handle bar ((foo bar) baz)) id)
        '(let ((temp1 (foo bar)))
           (let ((temp2 (temp1 baz)))
             (handle bar temp2))))

(gensym-reset!)
(assert (normalize '(handle (raise (foo 23)) handler) id)
        '(handle
            (let ((temp1 (foo 23)))
                  (raise temp1))
            handler))
