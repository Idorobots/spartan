;; Lambda inlining tests.

(describe
 "lambda-inlining"
 (it "beta reduces lambda applications"
     (check ((formals (gen-arg-list (gen-integer 1 5)))
             (body gen-simple-node)
             (f (gen-lambda-node formals body))
             (args (gen-arg-list (length (ast-lambda-formals f))))
             (app (apply gen-app-node f args)))
            (gensym-reset!)
            (assert-ast (lambda-inlining '() app)
                        (let bindings
                          body1)
                        (assert body1 body)
                        (gensym-reset!)
                        (assert (map ast-binding-var bindings) (map temporary-name formals))
                        (assert (map ast-binding-val bindings) args))))

(it "doesn't beta reduce unreducable applications"
     (check ((formals (gen-arg-list (gen-integer 1 5)))
             (body gen-simple-node)
             (f (gen-lambda-node formals body))
             (args (gen-arg-list (+ (length (ast-lambda-formals f)) 1)))
             (app (apply gen-app-node f args)))
            (assert (lambda-inlining '() app) app)))

 (it "inlines small lambdas"
     (check ((var gen-valid-symbol)
             (sym (gen-symbol-node var))
             (formals (gen-arg-list (gen-integer 1 5)))
             (body gen-simple-node)
             (f (gen-lambda-node formals body))
             (args (gen-arg-list (length (ast-lambda-formals f))))
             (app (apply gen-app-node sym args)))
            (gensym-reset!)
            (assert-ast (lambda-inlining (list (cons var f)) app)
                        (let bindings
                          body1)
                        (assert body1 body)
                        (gensym-reset!)
                        (assert (map ast-binding-var bindings) (map temporary-name formals))
                        (assert (map ast-binding-val bindings) args)))
     (check ((var gen-valid-symbol)
             (sym (gen-symbol-node var))
             (body gen-simple-node)
             (f (gen-lambda-node '() body))
             (args (gen-arg-list (length (ast-lambda-formals f))))
             (app (apply gen-app-node sym args)))
            (assert (lambda-inlining (list (cons var f)) app)
                    body)))

 (it "determines lambdas suitable for inlining"
     (check ((var gen-valid-symbol)
             (sym (gen-symbol-node var))
             (formals (gen-arg-list (gen-integer 1 5)))
             (body gen-simple-node)
             (f (gen-lambda-node formals body))
             (b (gen-binding-node sym f))
             (args (gen-arg-list (length (ast-lambda-formals f))))
             (app (apply gen-app-node sym args))
             (node (gen-let-node (list b) app)))
            (gensym-reset!)
            (assert-ast (lambda-inlining '() node)
                        (let ((binding sym1 f1))
                          (let bindings
                            body1))
                        (assert sym1 sym)
                        (assert f1 f)
                        (assert body1 body)
                        (gensym-reset!)
                        (assert (map ast-binding-var bindings) (map temporary-name formals))
                        (assert (map ast-binding-val bindings) args))))

 (it "doesn't inline too large lambdas"
     (check ((var gen-valid-symbol)
             (sym (gen-symbol-node var))
             (formals (gen-arg-list (gen-integer 1 5)))
             ;; NOTE This needs to be larger than the max lambda size for inlining.
             (body (gen-do-node +max-inlineable-size+
                                gen-valid-app-node))
             (f (gen-lambda-node formals body))
             (b (gen-binding-node sym f))
             (args (gen-arg-list (length (ast-lambda-formals f))))
             (app (apply gen-app-node sym args))
             (node (gen-let-node (list b) app)))
            (assert (lambda-inlining '() node) node)))

 (it "handles self-recursive functions"
     (check ((var gen-valid-symbol)
             (sym (gen-symbol-node var))
             (formals (gen-arg-list (gen-integer 1 5)))
             (body (apply gen-app-node sym formals))
             (f (gen-lambda-node formals body))
             (b (gen-binding-node sym f))
             (args (gen-arg-list (length (ast-lambda-formals f))))
             (app (apply gen-app-node sym args))
             (node (gen-letrec-node (list b) app)))
            (gensym-reset!)
            (assert-ast (lambda-inlining '() node)
                        (letrec ((binding sym1
                                          (lambda formals1
                                            (let bindings1
                                              body1))))
                          (let bindings2
                            body2))
                        (assert sym1 sym)
                        (assert formals1 formals)
                        (gensym-reset!)
                        (let ((first (map temporary-name formals))
                              (second (map temporary-name formals)))
                          (assert (ast-app-op body1) (ast-app-op body))
                          (assert (ast-app-args body1) second)
                          (assert (ast-app-op body2) (ast-app-op body))
                          (assert (ast-app-args body2) first)
                          (assert (map ast-binding-var bindings1) second)
                          (assert (map ast-binding-val bindings1) formals)
                          (assert (map ast-binding-var bindings2) first)
                          (assert (map ast-binding-val bindings2) args)))))

 (it "respects free variable scope"
     (check ((var1 gen-valid-symbol)
             (sym1 (gen-symbol-node var1))
             (var2 gen-valid-symbol)
             (f (gen-with-fv gen-valid-lambda-node
                             (set var2)))
             (b1 (gen-binding-node sym1 f))
             (sym2 (gen-symbol-node var2))
             (val gen-const-node)
             (b2 (gen-binding-node sym2 val))
             (args (gen-arg-list (length (ast-lambda-formals f))))
             (app (apply gen-app-node sym1 args))
             (inner (gen-with-bv (gen-let-node (list b2) app)
                                 (set var2)))
             (node (gen-with-bv (gen-let-node (list b1) inner)
                                (set var1))))
            (assert (lambda-inlining '() node) node))))
