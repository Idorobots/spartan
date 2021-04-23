;; Substitution works.

(describe
 "substitute"
 (it "substitutes simple cases"
     (check ((var gen-valid-symbol)
             (original (gen-symbol-node var))
             (subbed gen-valid-symbol-node)
             (other gen-valid-symbol-node)
             (app1 (gen-app-node original other))
             (app2 (gen-app-node original original))
             (app3 gen-valid-app-node)
             (if1 (gen-if-node original other other))
             (if2 (gen-if-node original other original))
             (if3 (gen-if-node other original original))
             (if4 gen-valid-if-node)
             (do1 (gen-specific-do-node original original))
             (do2 (gen-specific-do-node other original))
             (do3 gen-valid-do-node))
            (let ((subs (list (cons var (constantly subbed)))))
              (assert (substitute-symbols subs other) other)
              (assert (substitute-symbols subs app3) app3)
              (assert (substitute-symbols subs if4) if4)
              (assert (substitute-symbols subs do3) do3)
              (assert (substitute-symbols subs original) subbed)
              (assert-ast (substitute-symbols subs app1)
                          (app ,subbed-orig ,subbed-other)
                          (assert subbed-orig subbed)
                          (assert subbed-other other))
              (assert-ast (substitute-symbols subs app2)
                          (app ,subbed-orig1 ,subbed-orig2)
                          (assert subbed-orig1 subbed)
                          (assert subbed-orig2 subbed))
              (assert-ast (substitute-symbols subs if1)
                          (if ,subbed-orig ,subbed-other1 ,subbed-other2)
                          (assert subbed-orig subbed)
                          (assert subbed-other1 other)
                          (assert subbed-other2 other))
              (assert-ast (substitute-symbols subs if2)
                          (if ,subbed-orig1 ,subbed-other ,subbed-orig2)
                          (assert subbed-orig1 subbed)
                          (assert subbed-orig2 subbed)
                          (assert subbed-other other))
              (assert-ast (substitute-symbols subs if3)
                          (if ,subbed-other ,subbed-orig1 ,subbed-orig2)
                          (assert subbed-other other)
                          (assert subbed-orig1 subbed)
                          (assert subbed-orig2 subbed))
              (assert-ast (substitute-symbols subs do1)
                          (do ,subbed-orig1 ,subbed-orig2)
                          (assert subbed-orig1 subbed)
                          (assert subbed-orig2 subbed))
              (assert-ast (substitute-symbols subs do2)
                          (do ,subbed-other ,subbed-orig)
                          (assert subbed-other other)
                          (assert subbed-orig subbed)))))

 (it "substitutes lambdas correctly"
     (check ((var1 gen-valid-symbol)
             (original (gen-symbol-node var1))
             (var2 gen-valid-symbol)
             (subbed (gen-symbol-node var2))
             (other gen-valid-symbol-node)
             (lambda1 (gen-with-bv (gen-lambda-node (list other) original) (set var2)))
             (lambda2 (gen-with-bv (gen-lambda-node (list original) original) (set var1)))
             (lambda3 gen-valid-lambda-node))
            (let ((subs (list (cons var1 (constantly subbed)))))
              (assert (substitute-symbols subs lambda3) lambda3)
              (assert (substitute-symbols subs lambda2) lambda2)
              (assert-ast (substitute-symbols subs lambda1)
                          (lambda (,subbed-other) ,subbed-orig)
                          (assert subbed-other other)
                          (assert subbed-orig subbed)))))

 (it "substitutes let correctly"
     (check ((var1 gen-valid-symbol)
             (original (gen-symbol-node var1))
             (subbed gen-valid-symbol-node)
             (var2 gen-valid-symbol)
             (other (gen-symbol-node var2))
             (b1 (gen-binding-node other original))
             (b2 (gen-binding-node original other))
             (b3 (gen-binding-node original original))
             (let1 (gen-with-bv (gen-let-node (list b1) original) (set var2)))
             (let2 (gen-with-bv (gen-let-node (list b2) original) (set var1)))
             (let3 (gen-with-bv (gen-let-node (list b1) other) (set var2)))
             (let4 (gen-with-bv (gen-let-node (list b2) other) (set var1)))
             (let5 (gen-with-bv (gen-let-node (list b3) original) (set var1)))
             (let6 gen-valid-let-node))
            (let ((subs (list (cons var1 (constantly subbed)))))
              (assert (substitute-symbols subs let6) let6)
              (assert (substitute-symbols subs let4) let4)
              (assert (substitute-symbols subs let2) let2)
              (assert-ast (substitute-symbols subs let5)
                          (let ((binding ,subbed-orig1 ,subbed-orig2)) ,subbed-orig3)
                          (assert subbed-orig1 original)
                          (assert subbed-orig2 subbed)
                          (assert subbed-orig3 original))
              (assert-ast (substitute-symbols subs let3)
                          (let ((binding ,subbed-other1 ,subbed-orig)) ,subbed-other2)
                          (assert subbed-other1 other)
                          (assert subbed-other2 other)
                          (assert subbed-orig subbed))
              (assert-ast (substitute-symbols subs let1)
                          (let ((binding ,subbed-other ,subbed-orig1)) ,subbed-orig2)
                          (assert subbed-other other)
                          (assert subbed-orig1 subbed)
                          (assert subbed-orig2 subbed)))))

 (it "substitutes letrec correctly"
     (check ((var1 gen-valid-symbol)
             (original (gen-symbol-node var1))
             (subbed gen-valid-symbol-node)
             (var2 gen-valid-symbol)
             (other (gen-symbol-node var2))
             (b1 (gen-binding-node other original))
             (b2 (gen-binding-node original other))
             (b3 (gen-binding-node original original))
             (letrec1 (gen-with-bv (gen-letrec-node (list b1) original) (set var2)))
             (letrec2 (gen-with-bv (gen-letrec-node (list b2) original) (set var1)))
             (letrec3 (gen-with-bv (gen-letrec-node (list b1) other) (set var2)))
             (letrec4 (gen-with-bv (gen-letrec-node (list b2) other) (set var1)))
             (letrec5 (gen-with-bv (gen-letrec-node (list b3) original) (set var1)))
             (letrec6 gen-valid-letrec-node))
            (let ((subs (list (cons var1 (constantly subbed)))))
              (assert (substitute-symbols subs letrec6) letrec6)
              (assert (substitute-symbols subs letrec5) letrec5)
              (assert (substitute-symbols subs letrec4) letrec4)
              (assert (substitute-symbols subs letrec2) letrec2)
              (assert-ast (substitute-symbols subs letrec3)
                          (letrec ((binding ,subbed-other1 ,subbed-orig)) ,subbed-other2)
                          (assert subbed-other1 other)
                          (assert subbed-other2 other)
                          (assert subbed-orig subbed))
              (assert-ast (substitute-symbols subs letrec1)
                          (letrec ((binding ,subbed-other ,subbed-orig1)) ,subbed-orig2)
                          (assert subbed-other other)
                          (assert subbed-orig1 subbed)
                          (assert subbed-orig2 subbed)))))

 (it "substitutes fix correctly"
     (check ((var1 gen-valid-symbol)
             (original (gen-symbol-node var1))
             (subbed gen-valid-symbol-node)
             (var2 gen-valid-symbol)
             (other (gen-symbol-node var2))
             (b1 (gen-binding-node other original))
             (b2 (gen-binding-node original other))
             (b3 (gen-binding-node original original))
             (fix1 (gen-with-bv (gen-fix-node (list b1) original) (set var2)))
             (fix2 (gen-with-bv (gen-fix-node (list b2) original) (set var1)))
             (fix3 (gen-with-bv (gen-fix-node (list b1) other) (set var2)))
             (fix4 (gen-with-bv (gen-fix-node (list b2) other) (set var1)))
             (fix5 (gen-with-bv (gen-fix-node (list b3) original) (set var1)))
             (fix6 gen-valid-fix-node))
            (let ((subs (list (cons var1 (constantly subbed)))))
              (assert (substitute-symbols subs fix6) fix6)
              (assert (substitute-symbols subs fix5) fix5)
              (assert (substitute-symbols subs fix4) fix4)
              (assert (substitute-symbols subs fix2) fix2)
              (assert-ast (substitute-symbols subs fix3)
                          (fix ((binding ,subbed-other1 ,subbed-orig)) ,subbed-other2)
                          (assert subbed-other1 other)
                          (assert subbed-other2 other)
                          (assert subbed-orig subbed))
              (assert-ast (substitute-symbols subs fix1)
                          (fix ((binding ,subbed-other ,subbed-orig1)) ,subbed-orig2)
                          (assert subbed-other other)
                          (assert subbed-orig1 subbed)
                          (assert subbed-orig2 subbed)))))

 (it "doesn't subsitute const values"
     (check ((var gen-valid-symbol)
             (sym (gen-symbol-node var))
             (node (gen-specific-const-node sym))
             (subbed gen-valid-symbol-node))
            (assert (substitute-symbols (list (cons var (constantly subbed)))
                                        node)
                    node))))
