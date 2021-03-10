;; AST tests

(describe
 "AST primitives"
 (it "application? can be used in front of simple expression predicates."
     (assert (let ((expr 'foo))
               (cond ((string? expr) 'string)
                     ((application? expr) 'app)
                     ((symbol? expr) 'symbol)
                     ((lambda? expr) 'lambda)))
             'symbol)
     (assert (let ((expr '(foo bar)))
               (cond ((string? expr) 'string)
                     ((application? expr) 'app)
                     ((symbol? expr) 'symbol)
                     ((lambda? expr) 'lambda)))
             'app)
     (assert (let ((expr '(lambda (foo) bar)))
               (cond ((string? expr) 'string)
                     ((application? expr) 'app)
                     ((symbol? expr) 'symbol)
                     ((lambda? expr) 'lambda)))
             'lambda))

 (it "application? doesn't trigger for syntax forms."
     (let ((exprs (list '(lambda (x) x)
                        '(if a b c)
                        '(quote a)
                        '(define foo bar)
                        '(do a b c)
                        '(let ((a b)) c)
                        '(letrec ((a b)) c)
                        '(set! foo bar)
                        '(module (x) x)
                        '(structure a b c))))
       (assert (map (lambda (expr)
                      (cond ((application? expr) 'app)
                            ('else 'something-else)))
                    exprs)
               (map (constantly 'something-else)
                    exprs)))))

(define forms
  '(foo
    23
    ()
    "hurr"
    (quote foo)
    (a b c)
    (&yield-cont cont val)
    ((lambda (a) a) a)
    (lambda () 23)
    (lambda (foo) foo)
    (lambda (foo) (+ foo 23))
    (lambda (foo) (+ foo 23))
    (lambda (foo) (foo bar baz))
    (lambda (x) (lambda (y) (+ x y)))
    (lambda (n cont)
      (let ((c (lambda (v) (&yield-cont cont v))))
        (if n
            (&yield-cont c n)
            (&yield-cont c n))))
    (define k v)
    (define f (lambda (x) x))
    (define f (lambda (x) f))
    (do a b c)
    (do a (lambda (x) b) c)
    (if a b c)
    (if a (lambda (x) b) c)
    (let ((a b)) a)
    (let ((a 23)) (lambda (x) a))
    (let ((a (lambda (x) x))) (a 23))
    (letrec ((a b)) a)
    (letrec ((a 23)) (lambda (x) a))
    (letrec ((a (lambda (x) x))) (a 23))))

(describe
 "walk"
 (it "Walk transforms the AST correctly."
     (map (lambda (expr)
            (assert (walk id id expr)
                    expr))
          forms)
     (map (lambda (expr expected)
            (assert (walk id
                          (lambda (e)
                            (if (symbol? e)
                                's
                                e))
                          expr)
                    expected))
          forms
          '(s
            23
            ()
            "hurr"
            (quote foo)
            (s s s)
            (s s s)
            ((lambda (s) s) s)
            (lambda () 23)
            (lambda (s) s)
            (lambda (s) (s s 23))
            (lambda (s) (s s 23))
            (lambda (s) (s s s))
            (lambda (s) (lambda (s) (s s s)))
            (lambda (s s)
              (let ((s (lambda (s) (s s s))))
                (if s
                    (s s s)
                    (s s s))))
            (define s s)
            (define s (lambda (s) s))
            (define s (lambda (s) s))
            (do s s s)
            (do s (lambda (s) s) s)
            (if s s s)
            (if s (lambda (s) s) s)
            (let ((s s)) s)
            (let ((s 23)) (lambda (s) s))
            (let ((s (lambda (s) s))) (s 23))
            (letrec ((s s)) s)
            (letrec ((s 23)) (lambda (s) s))
            (letrec ((s (lambda (s) s))) (s 23)))))

 (it "walk preprocessing works correctly"
     (assert (walk (lambda (expr)
                     (cond ((number? expr) 23)
                           ((lambda? expr) (make-lambda (lambda-args expr)
                                                        42))
                           ('else expr)))
                   id
                   '(lambda (x) 5))
             '(lambda (x) 23)))

 (it "postprocessing works correctly"
     (assert (walk id
                   (lambda (expr)
                     (cond ((number? expr) 23)
                           ((lambda? expr) (make-lambda (lambda-args expr)
                                                        42))
                           ('else expr)))
                   '(lambda (x) 5))
             '(lambda (x) 42))))
