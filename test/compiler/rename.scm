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
     (assert (mangle '23) '23)
     (assert (mangle '"foo bar") '"foo bar")
     (assert (mangle ''(heh quote)) ''(heh quote))
     (assert (mangle 'foo) '__foo)
     (assert (mangle 'foo23) '__foo23)
     (assert (mangle 'foo!) '__fooBANG)
     (assert (mangle 'symbol->safe) '__symbol_GREATERsafe)
     (assert (mangle '&symbol->safe) '&symbol->safe))

 (it "Renaming let works."
     (assert (mangle '(let ((foo bar))
                        baz))
             '(let ((__foo __bar))
                __baz)))

 (it "Renaming lambda works."
     (assert (mangle '(lambda (foo) bar))
             '(lambda (__foo) __bar))
     (assert (mangle '(lambda (foo) ((lambda (x) foo) bar))
                     )
             '(lambda (__foo) ((lambda (__x) __foo) __bar))))

 (it "Renaming do works."
     (assert (mangle '(do a b c))
             '(do __a __b __c))
     (assert (mangle '(do a (do b c) d))
             '(do __a (do __b __c) __d)))

 (it "Renaming if works."
     (assert (mangle '(if cond then else))
             '(if __cond __then __else))
     (assert (mangle '(if (equal? 23 23) (cons a b) c))
             '(if (__equalQUEST 23 23) (__cons __a __b) __c)))

 (it "Renaming application works."
     (assert (mangle '(foo bar baz))
             '(__foo __bar __baz))
     (assert (mangle '(&yield-cont bar baz))
             '(&yield-cont __bar __baz))))
