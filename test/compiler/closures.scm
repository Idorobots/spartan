;; Closure conversion

;; Set operations work.
(assert (set) '())
(assert (set 'a 'b 'c) '(a b c))
(assert (set 'c 'a 'b) '(a b c))
(assert (set-difference (set 'a 'b 'c) (set 'a 'b 'c)) '())
(assert (set-difference (set 'a 'b 'c) (set 'a)) '(b c))
(assert (set-difference (set 'a 'b 'c) (set 'd)) '(a b c))
(assert (set-difference (set 'a 'b 'c) (set 'c 'd)) '(a b))
(assert (set-union (set 'a 'b 'c) (set 'a 'b 'c)) '(a b c))
(assert (set-union (set 'a 'b 'c) (set 'd)) '(a b c d))
(assert (set-union (set 'b 'c 'd) (set 'a)) '(a b c d))
(assert (set-union (set 'b 'c) (set 'a 'd)) '(a b c d))

;; Free variables computation works.
(assert (free-vars '()) '())
(assert (free-vars '23) '())
(assert (free-vars '"foo") '())
(assert (free-vars '(a b c)) '(a b c))
(assert (free-vars '(quote foo bar)) '())
(assert (free-vars '(list (quote foo bar) baz)) '(baz list))
(assert (free-vars '(define foo (list foo bar baz))) '(bar baz foo list))
(assert (free-vars '(do a b c)) '(a b c))
(assert (free-vars '(if a b c)) '(a b c))
(assert (free-vars '(letcc cont (foo bar cont))) '(bar foo))
(assert (free-vars '(letrec ((a 23) (bar a)) (* 2 bar))) '(*))
(assert (free-vars '(letrec ((a 23) (bar foo)) (* a bar))) '(* foo))
(assert (free-vars '(shift k (reset (* foo k)))) '(* foo))
(assert (free-vars '(handle (raise foo) bar)) '(bar foo))
(assert (free-vars '(lambda (x y) (+ x y))) '(+))
(assert (free-vars '(lambda (x) x)) '())

;; Substitution works.
(assert (substitute '((foo . bar)) 'faz) 'faz)
(assert (substitute '((foo . bar)) 'foo) 'bar)
(assert (substitute '((foo . bar)) '(foo bar)) '(bar bar))
(assert (substitute '((foo . bar) (bar . foo)) '(foo bar)) '(bar foo))
(assert (substitute '((foo . bar)) '(foo foo)) '(bar bar))
(assert (substitute '((foo . bar)) '(do foo (lambda (foo) foo))) '(do bar (lambda (bar) bar)))
(assert (substitute '((foo . bar)) '(do foo (lambda (bar) foo))) '(do bar (lambda (bar) bar)))

;; Simple cases work.
(assert (closure-convert 'foo '()) 'foo)
(assert (closure-convert 23 '()) 23)
(assert (closure-convert '() '()) '())
(assert (closure-convert "hurr" '()) "hurr")
(assert (closure-convert '(quote foo) '()) '(quote foo))

;; Converting application works.
(assert (closure-convert '(a b c) '()) '(&apply a b c))

(gensym-reset!)
(assert (closure-convert '((lambda (a) a) a) '())
        '(&apply (&make-closure (&make-env)
                                (lambda (__env1 a) a))
                 a))

;; Convertin lambda works.
(gensym-reset!)
(assert (closure-convert '(lambda () 23) '())
        '(&make-closure (&make-env)
                        (lambda (__env1) 23)))

(gensym-reset!)
(assert (closure-convert '(lambda (foo) foo) '())
        '(&make-closure (&make-env)
                        (lambda (__env1 foo) foo)))

(gensym-reset!)
(assert (closure-convert '(lambda (foo) (+ foo 23)) '(&apply))
        '(&make-closure (&make-env +)
                        (lambda (__env1 foo) (&apply (&env-ref __env1 0) foo 23))))

(gensym-reset!)
(assert (closure-convert '(lambda (foo) (+ foo 23)) '(+ &apply))
        '(&make-closure (&make-env)
                        (lambda (__env1 foo) (+ foo 23))))

(gensym-reset!)
(assert (closure-convert '(lambda (foo) (foo bar baz)) '(&apply))
        '(&make-closure (&make-env bar baz)
                        (lambda (__env1 foo)
                          (&apply foo (&env-ref __env1 0) (&env-ref __env1 1)))))

(gensym-reset!)
(assert (closure-convert '(lambda (x) (lambda (y) (+ x y))) '(&apply &env-ref &make-closure &make-env))
        '(&make-closure (&make-env +)
                        (lambda (__env2 x)
                          (&make-closure (&make-env (&env-ref __env2 0) x)
                                         (lambda (__env1 y)
                                           (&apply
                                            (&env-ref __env1 0)
                                            (&env-ref __env1 1)
                                            y))))))

;; Converting define works.
(assert (closure-convert '(define k v) '()) '(define k v))

(gensym-reset!)
(assert (closure-convert '(define f (lambda (x) x)) '())
        '(define f (&make-closure (&make-env)
                                  (lambda (__env1 x) x))))

(gensym-reset!)
(assert (closure-convert '(define f (lambda (x) f)) '())
        '(define f (&make-closure (&make-env f)
                                  (lambda (__env1 x) (&env-ref __env1 0)))))

;; Converting do works.
(assert (closure-convert '(do a b c) '()) '(do a b c))

(gensym-reset!)
(assert (closure-convert '(do a (lambda (x) b) c) '())
        '(do a
             (&make-closure (&make-env b)
                            (lambda (__env1 x) (&env-ref __env1 0)))
           c))

;; Converting if works.
(assert (closure-convert '(if a b c) '()) '(if a b c))

(gensym-reset!)
(assert (closure-convert '(if a (lambda (x) b) c) '())
        '(if a
             (&make-closure (&make-env b)
                            (lambda (__env1 x) (&env-ref __env1 0)))
             c))

;; Converting let works.
(assert (closure-convert '(let ((a b)) a) '()) '(let ((a b)) a))

(gensym-reset!)
(assert (closure-convert '(let ((a 23)) (lambda (x) a)) '(&apply))
        '(let ((a 23))
           (&make-closure (&make-env a)
                          (lambda (__env1 x)
                            (&env-ref __env1 0)))))

(gensym-reset!)
(assert (closure-convert '(let ((a (lambda (x) x))) (a 23)) '(&apply))
        '(let ((a (&make-closure (&make-env)
                                 (lambda (__env1 x)
                                   x))))
           (&apply a 23)))

;; Converting letrec works.
(assert (closure-convert '(letrec ((a b)) a) '()) '(letrec ((a b)) a))

(gensym-reset!)
(assert (closure-convert '(letrec ((a 23)) (lambda (x) a)) '(&apply))
        '(letrec ((a 23))
           (&make-closure (&make-env a)
                          (lambda (__env1 x)
                            (&env-ref __env1 0)))))

(gensym-reset!)
(assert (closure-convert '(letrec ((a (lambda (x) x))) (a 23)) '(&apply))
        '(letrec ((a (&make-closure (&make-env)
                                 (lambda (__env1 x)
                                   x))))
           (&apply a 23)))

;; Converting letcc works.
(assert (closure-convert '(letcc k k) '()) '(letcc k k))

(gensym-reset!)
(assert (closure-convert '(letcc k (lambda (x) k)) '(&apply))
        '(letcc k
           (&make-closure (&make-env k)
                          (lambda (__env1 x)
                            (&env-ref __env1 0)))))


;; Converting shift/reset works.
(assert (closure-convert '(shift k (reset k)) '()) '(shift k (reset k)))

(gensym-reset!)
(assert (closure-convert '(shift k (lambda (x) (k x))) '(&apply))
        '(shift k
                (&make-closure (&make-env k)
                               (lambda (__env1 x)
                                 (&apply
                                  (&env-ref __env1 0)
                                  x)))))

(gensym-reset!)
(assert (closure-convert '(reset (lambda (x) x)) '(&apply))
        '(reset
          (&make-closure (&make-env)
                         (lambda (__env1 x) x))))

(gensym-reset!)
(assert (closure-convert '(shift k (reset (lambda (x) (k x)))) '(&apply))
        '(shift k
                (reset (&make-closure (&make-env k)
                                      (lambda (__env1 x)
                                        (&apply
                                         (&env-ref __env1 0)
                                         x))))))

;; Converting raise works.
(assert (closure-convert '(raise e) '()) '(raise e))

(gensym-reset!)
(assert (closure-convert '(raise (lambda (x) x)) '())
        '(raise (&make-closure (&make-env)
                               (lambda (__env1 x) x))))

;; Converting handle works.
(assert (closure-convert '(handle e h) '()) '(handle e h))

(gensym-reset!)
(assert (closure-convert '(handle (lambda (x) x) h) '())
        '(handle (&make-closure (&make-env)
                                (lambda (__env1 x) x))
                 h))

(gensym-reset!)
(assert (closure-convert '(handle x (lambda (e) e)) '())
        '(handle x
                 (&make-closure (&make-env)
                                (lambda (__env1 e) e))))
