;; Substitution works.

(describe
 "substitute"
 (it "substitutes simple cases"
     (check ((var 'foo)
             (original (gen-symbol-node var))
             (subbed (gen-symbol-node 'bar))
             (other gen-valid-symbol-node)
             (app1 (gen-app-node original other))
             (app2 (gen-app-node original original))
             (app3 (gen-app-node other other))
             (if1 (gen-if-node original other other))
             (if2 (gen-if-node original other original))
             (if3 (gen-if-node other original original))
             (if4 (gen-if-node other other other))
             (do1 (gen-specific-do-node original original))
             (do2 (gen-specific-do-node other original))
             (do3 (gen-specific-do-node other other)))
            (let ((subs (list (cons var (constantly subbed)))))
              (assert (substitute subs other) other)
              (assert (substitute subs app3) app3)
              (assert (substitute subs if4) if4)
              (assert (substitute subs do3) do3)
              (assert (substitute subs original) subbed)
              (let ((result (substitute subs app1)))
                (assert-ast result
                            (app ,subbed-orig ,subbed-other)
                            (assert subbed-orig subbed)
                            (assert subbed-other other)))
              (let ((result (substitute subs app2)))
                (assert-ast result
                            (app ,subbed-orig1 ,subbed-orig2)
                            (assert subbed-orig1 subbed)
                            (assert subbed-orig2 subbed)))
              (let ((result (substitute subs if1)))
                (assert-ast result
                            (if ,subbed-orig ,subbed-other1 ,subbed-other2)
                            (assert subbed-orig subbed)
                            (assert subbed-other1 other)
                            (assert subbed-other2 other)))
              (let ((result (substitute subs if2)))
                (assert-ast result
                            (if ,subbed-orig1 ,subbed-other ,subbed-orig2)
                            (assert subbed-orig1 subbed)
                            (assert subbed-orig2 subbed)
                            (assert subbed-other other)))
              (let ((result (substitute subs if3)))
                (assert-ast result
                            (if ,subbed-other ,subbed-orig1 ,subbed-orig2)
                            (assert subbed-other other)
                            (assert subbed-orig1 subbed)
                            (assert subbed-orig2 subbed)))
              (let ((result (substitute subs do1)))
                (assert-ast result
                            (do ,subbed-orig1 ,subbed-orig2)
                            (assert subbed-orig1 subbed)
                            (assert subbed-orig2 subbed)))
              (let ((result (substitute subs do2)))
                (assert-ast result
                            (do ,subbed-other ,subbed-orig)
                            (assert subbed-other other)
                            (assert subbed-orig subbed)))))))

(define subs '((foo . bar)))

(describe
 "old-substitute"
 (it "simple cases work"
     (assert (old-substitute subs 'faz) 'faz)
     (assert (old-substitute subs 'foo) 'bar)
     (assert (old-substitute subs '(foo bar)) '(bar bar))
     (assert (old-substitute subs '(foo foo)) '(bar bar))
     (assert (old-substitute (cons '(bar . foo) subs) '(foo bar)) '(bar foo))
     (assert (old-substitute (cons '(bar . foo) subs) '(a b c)) '(a b c)))
 (it "syntax forms are handled correctly"
     (assert (old-substitute subs '(if foo foo (not foo))) '(if bar bar (not bar)))
     (assert (old-substitute subs '(do foo foo (also foo))) '(do bar bar (also bar)))
     (assert (old-substitute subs '(do foo foo (also foo))) '(do bar bar (also bar))))
 (it "bindings are handled correctly"
     (assert (old-substitute subs '(letrec ((bar foo) (foo bar)) foo)) '(letrec ((bar foo) (foo bar)) foo))
     (assert (old-substitute subs '(let ((bar foo) (foo bar)) foo)) '(let ((bar bar) (foo bar)) foo))
     (assert (old-substitute subs '(do foo (lambda (foo) foo))) '(do bar (lambda (foo) foo)))
     (assert (old-substitute subs '(do foo (lambda (bar) foo))) '(do bar (lambda (bar) bar)))
     (assert (old-substitute (cons '(bar . baz) subs) '(lambda (bar) (* foo bar))) '(lambda (bar) (* bar bar)))))
