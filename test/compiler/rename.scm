;; Renaming things for safety.

(describe
 "symbol->safe"
 (it "correctly escapes symbols"
     (assert (symbol->safe 'foo) '__foo)
     (assert (symbol->safe 'foo23) '__foo23)
     (assert (symbol->safe 'foo!) '__fooBANG)
     (assert (symbol->safe 'symbol->safe) '__symbol_GREATERsafe)))


(describe
 "mangle"
 (it "Renaming simple expressions works."
     (assert (mangle '23 '()) '23)
     (assert (mangle '"foo bar" '()) '"foo bar")
     (assert (mangle ''(heh quote) '()) ''(heh quote))
     (assert (mangle 'foo '()) '__foo)
     (assert (mangle 'foo23 '()) '__foo23)
     (assert (mangle 'foo! '()) '__fooBANG)
     (assert (mangle 'symbol->safe '()) '__symbol_GREATERsafe)
     (assert (mangle 'symbol->safe '(symbol->safe)) 'symbol->safe))

 (it "Renaming define works."
     (assert (mangle '(define foo bar) '())
             '(define __foo __bar))
     (assert (mangle '(define id (lambda (x) x)) '())
             '(define __id (lambda (__x) __x))))

 (it "Renaming let works."
     (assert (mangle '(let ((foo bar))
                        baz)
                     '())
             '(let ((__foo __bar))
                __baz)))

 (it "Renaming lambda works."
     (assert (mangle '(lambda (foo) bar) '())
             '(lambda (__foo) __bar))
     (assert (mangle '(lambda (foo) ((lambda (x) foo) bar)) '())
             '(lambda (__foo) ((lambda (__x) __foo) __bar))))

 (it "Renaming do works."
     (assert (mangle '(do a b c) '())
             '(do __a __b __c))
     (assert (mangle '(do a (do b c) d) '())
             '(do __a (do __b __c) __d)))

 (it "Renaming if works."
     (assert (mangle '(if cond then else) '())
             '(if __cond __then __else))
     (assert (mangle '(if (equal? 23 23) (cons a b) c) '())
             '(if (__equalQUEST 23 23) (__cons __a __b) __c)))

 (it "Renaming letrec works."
     (assert (mangle '(letrec ((a 23)
                               (b a))
                        b)
                     '())
             '(letrec ((__a 23)
                       (__b __a))
                __b)))

 (it "Renaming application works."
     (assert (mangle '(foo bar baz) '())
             '(__foo __bar __baz))
     (assert (mangle '(&yield-cont bar baz) '(&yield-cont))
             '(&yield-cont __bar __baz)))

 (it "Renaming set! works."
     (assert (mangle '(set! foo bar) '())
             '(set! __foo __bar)))

 (it "Renaming letcc works."
     (assert (mangle '(letcc k k) '())
             '(letcc __k __k))
     (assert (mangle '(letcc k (k bar)) '())
             '(letcc __k (__k __bar))))

 (it "Renaming shift/reset works."
     (assert (mangle '(shift k k) '())
             '(shift __k __k))
     (assert (mangle '(reset foo) '())
             '(reset __foo))
     (assert (mangle '(shift k (k (reset (k 23)))) '())
             '(shift __k (__k (reset (__k 23))))))

 (it "Renaming handle/raise works."
     (assert (mangle '(raise error) '())
             '(raise __error))
     (assert (mangle '(handle expr handler) '())
             '(handle __expr __handler))
     (assert (mangle '(handle (raise error)
                              (lambda (e r) (r e)))
                     '())
             '(handle (raise __error)
                      (lambda (__e __r)
                        (__r __e))))))
