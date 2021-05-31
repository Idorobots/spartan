;; Closure conversion

(describe
 "make-env"
 (it "should create environment correctly"
     (check ((free-vars '())
             (loc gen-location))
            (let ((result (make-env loc free-vars '())))
              (assert-ast result
                          (const (list))
                          (assert (generated? result))
                          (assert (ast-node-location result) loc))))
     (check ((free-vars (gen-list 1 gen-valid-symbol))
             (loc gen-location))
            (let ((result (make-env loc free-vars '())))
              (assert (ast-symbol-value result) (car free-vars))
              (assert (ast-node-location result) loc)))
     (check ((free-vars (gen-list 2 gen-valid-symbol))
             (loc gen-location))
            (let ((result (make-env loc free-vars '())))
              (assert-ast result
                          (primop-app 'cons first second)
                          (assert (ast-symbol-value first) (car free-vars))
                          (assert (ast-symbol-value second) (cadr free-vars))
                          (assert (ast-node-location result) loc))))
     (check ((free-vars (gen-list (gen-integer 3 5) gen-valid-symbol))
             (loc gen-location))
            (let ((result (make-env loc free-vars '())))
              (assert-ast result
                          (primop-app '&make-env args ...)
                          (assert (map ast-symbol-value args) free-vars)
                          (assert (ast-node-location result) loc)))))

 (it "should re-use bound names to preserve proper source locations"
     (check ((closures (gen-list (gen-integer 3 5) gen-valid-symbol-node))
             (free-vars (map ast-symbol-value closures))
             (loc gen-location))
            (let ((result (make-env loc free-vars closures)))
              (assert-ast result
                          (primop-app '&make-env args ...)
                          (assert args closures)
                          (assert (ast-node-location result) loc))))))

(describe
 "make-env-subs"
 (it "should create environment substitutions correctly"
     (check ((env gen-valid-symbol-node)
             (free-vars '()))
            (assert (empty-subs? (make-env-subs env free-vars))))
     (check ((env gen-valid-symbol-node)
             (nodes (gen-arg-list 1))
             (node (apply gen-specific-do-node nodes))
             (free-vars (map ast-symbol-value nodes)))
            (let* ((subs (make-env-subs env free-vars))
                   (result (substitute-symbols subs node)))
              (assert-ast result
                          (do converted-var)
                          (assert converted-var
                                  (set-ast-node-location env (ast-node-location (car nodes)))))))
     (check ((env gen-valid-symbol-node)
             (nodes (gen-arg-list 2))
             (node (apply gen-specific-do-node nodes))
             (free-vars (map ast-symbol-value nodes)))
            (let* ((subs (make-env-subs env free-vars))
                   (result (substitute-symbols subs node)))
              (assert-ast result
                          (do (primop-app 'car converted-var1)
                              (primop-app 'cdr converted-var2))
                          (assert converted-var1 env)
                          (assert converted-var2 env))))
     (check ((env gen-valid-symbol-node)
             (nodes (gen-arg-list (gen-integer 3 5)))
             (node (apply gen-specific-do-node nodes))
             (free-vars (map ast-symbol-value nodes)))
            (let* ((subs (make-env-subs env free-vars))
                   (result (substitute-symbols subs node)))
              (assert-ast result
                          (do (primop-app '&env-ref converted-env1 (const (number '0)))
                              (primop-app '&env-ref converted-env2 (const (number '1)))
                              (primop-app '&env-ref converted-env3 (const (number '2)))
                              rest
                              ...)
                          (assert (length rest)
                                  (- (length free-vars) 3))
                          (assert converted-env1 env)
                          (assert converted-env2 env)
                          (assert converted-env3 env))))))

(describe
 "make-env-setters"
 (it "should correctly patch empty envs"
     (check ((env-var gen-valid-symbol-node)
             (loc gen-location))
            (let* ((env (make-env loc '() '()))
                   (result (make-env-setters env env-var '() '() '())))
              (assert (empty? result)))))

  (it "should correctly patch shared envs encoded as pairs and raw values"
     (check ((env-var gen-valid-symbol-node)
             (loc gen-location)
             (closures (gen-arg-list (gen-integer 1 2)))
             (bound (map ast-symbol-value closures))
             (free bound))
            (let* ((env (make-env loc free closures))
                   (result (make-env-setters env env-var free (apply set bound) closures)))
              (assert (length result) (length closures))
              (map (lambda (setter var)
                     (assert-ast setter
                                 (primop-app '&set-closure-env! converted-closure-var converted-env)
                                 (assert (ast-symbol-value converted-closure-var) var)
                                 (assert converted-env env)))
                   result
                   bound))))

 (it "should correctly patch large shared envs"
     (check ((env-var gen-valid-symbol-node)
             (loc gen-location)
             (closures (gen-arg-list (gen-integer 3 5)))
             (bound (map ast-symbol-value closures))
             (extra-free (gen-list (gen-integer 0 5) gen-valid-symbol))
             (free (append bound extra-free)))
            (let* ((env (make-env loc free closures))
                   (result (make-env-setters env env-var free (apply set bound) closures)))
              (assert (length result) (length closures))
              (map (lambda (setter var)
                     (assert-ast setter
                                 (primop-app '&set-env! converted-env-var (const converted-offset) converted-var)
                                 (assert converted-env-var env-var)
                                 (assert (ast-number-value converted-offset) (offset var free))
                                 (assert (ast-symbol-value converted-var) var)))
                   result
                   bound)))))

(describe
 "closure-convert"
 (it "should convert application correctly"
     (check ((app gen-valid-app-node))
            (let ((result (convert-closures app (set))))
              (assert-ast result
                          (primop-app '&apply op args ...)
                          (assert op (ast-app-op app))
                          (assert args (ast-app-args app)))
              (assert (ast-node-location result)
                      (ast-node-location app)))))

 (it "should convert lambdas correctly"
     (check ((node gen-valid-lambda-node))
            (gensym-reset!)
            (let ((result (convert-closures node (set))))
              (assert-ast result
                          (primop-app '&make-closure
                                      (const (list))
                                      (lambda ((symbol 'env1) formals ...)
                                        body))
                          (assert formals (ast-lambda-formals node))
                          (assert body (ast-lambda-body node)))
              (assert (ast-node-location result)
                      (ast-node-location node))))
     (check ((arg gen-valid-symbol-node)
             (body gen-valid-symbol-node)
             (var (ast-symbol-value body))
             (node (gen-with-fv (gen-lambda-node (list arg) body) (set var))))
            (gensym-reset!)
            (let ((result (convert-closures node (set))))
              (assert-ast result
                          (primop-app '&make-closure
                                      arg
                                      (lambda ((symbol 'env1) _)
                                        (symbol 'env1)))
                          (assert (ast-symbol-value arg) var))
              (assert (ast-node-location result)
                      (ast-node-location node))))
     (check ((nodes (gen-arg-list 2))
             (free-vars (apply set (map ast-symbol-value nodes)))
             (body (apply gen-specific-do-node nodes))
             (node (gen-with-fv (gen-lambda-node '() body) free-vars)))
            (gensym-reset!)
            (let ((result (convert-closures node (set))))
              (assert-ast result
                          (primop-app '&make-closure
                                      (primop-app 'cons args ...)
                                      (lambda ((symbol 'env1))
                                        ;; NOTE The order of cars and cdrs is unpredictable.
                                        (do (primop-app first (symbol 'env1))
                                            (primop-app second (symbol 'env1)))))
                          (assert (apply set (map ast-symbol-value args)) free-vars)
                          (if (equal? first 'car)
                              (begin (assert second 'cdr)
                                     (assert (map ast-symbol-value args)
                                             (map ast-symbol-value nodes)))
                              (begin (assert second 'car)
                                     (assert (map ast-symbol-value args)
                                             (map ast-symbol-value (reverse nodes))))))
              (assert (ast-node-location result)
                      (ast-node-location node))))
     (check ((nodes (gen-arg-list (gen-integer 3 5)))
             (free-vars (apply set (map ast-symbol-value nodes)))
             (body (apply gen-specific-do-node nodes))
             (node (gen-with-fv (gen-lambda-node '() body) free-vars)))
            (gensym-reset!)
            (let ((result (convert-closures node (set))))
              (assert-ast result
                          (primop-app '&make-closure
                                      (primop-app '&make-env args ...)
                                      (lambda ((symbol 'env1))
                                        ;; NOTE The order of the offsets is unpredictable as well.
                                        (do (primop-app '&env-ref (symbol 'env1) first)
                                            (primop-app '&env-ref (symbol 'env1) second)
                                            (primop-app '&env-ref (symbol 'env1) third)
                                            rest
                                            ...)))
                          (assert (apply set (map ast-symbol-value args)) free-vars)
                          (let ((first-val (ast-number-value (ast-const-value first)))
                                (second-val (ast-number-value (ast-const-value second)))
                                (third-val (ast-number-value (ast-const-value third))))
                            (assert (not (empty? (member first-val '(0 1 2 3 4)))))
                            (assert (not (empty? (member second-val '(0 1 2 3 4)))))
                            (assert (not (empty? (member third-val '(0 1 2 3 4)))))
                            (assert (ast-symbol-value (list-ref nodes 0))
                                    (ast-symbol-value (list-ref args first-val)))
                            (assert (ast-symbol-value (list-ref nodes 1))
                                    (ast-symbol-value (list-ref args second-val)))
                            (assert (ast-symbol-value (list-ref nodes 2))
                                    (ast-symbol-value (list-ref args third-val))))
                          (assert (length rest) (- (length nodes) 3)))
              (assert (ast-node-location result)
                      (ast-node-location node)))))

 (it "should take global values into account"
     (check ((arg gen-valid-symbol-node)
             (body gen-valid-symbol-node)
             (var (ast-symbol-value body))
             (node (gen-with-fv (gen-lambda-node (list arg) body) (set var))))
            (gensym-reset!)
            (let ((result (convert-closures node (set var))))
              (assert-ast result
                          (primop-app '&make-closure
                                      (const (list))
                                      (lambda ((symbol 'env1) _)
                                        converted-body))
                          (assert converted-body body))
              (assert (ast-node-location result)
                      (ast-node-location node)))))

 (it "should convert fix correctly"
     (check ((sym1 gen-valid-symbol)
             (var1 (gen-symbol-node sym1))
             (arg1 gen-valid-symbol-node)
             (lambda1 (gen-lambda-node (list arg1) var1))
             (b1 (gen-with-fv-bv (gen-binding-node var1 lambda1) (set sym1) (set sym1)))
             (body gen-simple-node)
             (node (gen-with-bv (gen-fix-node (list b1) body) (set sym1))))
            (gensym-reset!)
            (let ((result (convert-closures node (set))))
              (assert-ast result
                          (let ((binding (symbol 'env1) (const (list))))
                            (let ((binding converted-var1
                                           (primop-app '&make-closure
                                                       (symbol 'env1)
                                                       (lambda ((symbol 'env2) converted-arg)
                                                         (symbol 'env2)))))
                              (do (primop-app '&set-closure-env!
                                              converted-var2
                                              converted-var3)
                                  converted-body)))
                          (assert converted-var1 var1)
                          (assert converted-arg arg1)
                          (assert converted-var2 var1)
                          (assert converted-var3 var1)
                          (assert converted-body body))
              (assert (ast-node-location result)
                      (ast-node-location node))
              (assert (generated? result))))

     (check ((sym1 gen-valid-symbol)
             (var1 (gen-symbol-node sym1))
             (arg1 gen-valid-symbol-node)
             (lambda1 (gen-lambda-node (list arg1) var1))
             (b1 (gen-with-fv-bv (gen-binding-node var1 lambda1) (set sym1) (set sym1)))
             (sym2 gen-valid-symbol)
             (var2 (gen-symbol-node sym2))
             (arg2 gen-valid-symbol-node)
             (lambda2 (gen-lambda-node (list arg2) var2))
             (b2 (gen-with-fv-bv (gen-binding-node var2 lambda2) (set sym2) (set sym2)))
             (body gen-simple-node)
             (node (gen-with-bv (gen-fix-node (list b1 b2) body) (set sym1 sym2))))
            (gensym-reset!)
            (let ((result (convert-closures node (set))))
              (assert-ast result
                          (let ((binding (symbol 'env1) (primop-app 'cons
                                                           (const (list))
                                                           (const (list)))))
                            (let ((binding converted-var1
                                           (primop-app '&make-closure
                                                       (symbol 'env1)
                                                       (lambda ((symbol 'env2) converted-arg1)
                                                         (primop-app first (symbol 'env2)))))
                                  (binding converted-var2
                                           (primop-app '&make-closure
                                                       (symbol 'env1)
                                                       (lambda ((symbol 'env3) converted-arg2)
                                                         (primop-app second (symbol 'env3))))))
                              (do (primop-app '&set-closure-env!
                                              converted-var3
                                              (primop-app 'cons
                                                          converted-var4
                                                          converted-var5))
                                  (primop-app '&set-closure-env!
                                              converted-var6
                                              (primop-app 'cons
                                                          converted-var7
                                                          converted-var8))
                                converted-body)))
                          (assert converted-var1 var1)
                          (assert converted-var2 var2)
                          (assert converted-var3 var1)
                          (assert converted-var6 var2)
                          ;; NOTE The order of the consed values is unpredictable.
                          (if (equal? first 'car)
                              (begin (assert second 'cdr)
                                     (assert converted-var4 var1)
                                     (assert converted-var5 var2)
                                     (assert converted-var7 var1)
                                     (assert converted-var8 var2))
                              (begin (assert second 'car)
                                     (assert converted-var4 var2)
                                     (assert converted-var5 var1)
                                     (assert converted-var7 var2)
                                     (assert converted-var8 var1)))
                          (assert converted-arg1 arg1)
                          (assert converted-arg2 arg2)
                          (assert converted-body body))
              (assert (ast-node-location result)
                      (ast-node-location node))
              (assert (generated? result))))

     (check ((sym1 gen-valid-symbol)
             (var1 (gen-symbol-node sym1))
             (arg1 gen-valid-symbol-node)
             (lambda1 (gen-lambda-node (list arg1) var1))
             (b1 (gen-with-fv-bv (gen-binding-node var1 lambda1) (set sym1) (set sym1)))
             (sym2 gen-valid-symbol)
             (var2 (gen-symbol-node sym2))
             (arg2 gen-valid-symbol-node)
             (lambda2 (gen-lambda-node (list arg2) var2))
             (b2 (gen-with-fv-bv (gen-binding-node var2 lambda2) (set sym2) (set sym2)))
             (sym3 gen-valid-symbol)
             (var3 (gen-symbol-node sym3))
             (arg3 gen-valid-symbol-node)
             (lambda3 (gen-lambda-node (list arg3) var3))
             (extra-fv gen-valid-symbol)
             (b3 (gen-with-fv-bv (gen-binding-node var3 lambda3) (set sym3 extra-fv) (set sym3)))
             (body gen-simple-node)
             (node (gen-with-bv (gen-fix-node (list b1 b2 b3) body) (set sym1 sym2 sym3))))
            (gensym-reset!)
            (let ((result (convert-closures node (set))))
              (assert-ast result
                          (let ((binding (symbol 'env1) (primop-app '&make-env converted-args ...)))
                            (let ((binding converted-var1
                                           (primop-app '&make-closure
                                                       (symbol 'env1)
                                                       (lambda ((symbol 'env2) converted-arg1)
                                                         (primop-app '&env-ref (symbol 'env2) first1))))
                                  (binding converted-var2
                                           (primop-app '&make-closure
                                                       (symbol 'env1)
                                                       (lambda ((symbol 'env3) converted-arg2)
                                                         (primop-app '&env-ref (symbol 'env3) second1))))
                                  (binding converted-var3
                                           (primop-app '&make-closure
                                                       (symbol 'env1)
                                                       (lambda ((symbol 'env4) converted-arg3)
                                                         (primop-app '&env-ref (symbol 'env4) third1)))))
                              (do (primop-app '&set-env! (symbol 'env1) first2 converted-var4)
                                  (primop-app '&set-env! (symbol 'env1) second2 converted-var5)
                                  (primop-app '&set-env! (symbol 'env1) third2 converted-var6)
                                  converted-body)))
                          (assert converted-var1 var1)
                          (assert converted-var2 var2)
                          (assert converted-var3 var3)
                          (assert converted-arg1 arg1)
                          (assert converted-arg2 arg2)
                          (assert converted-arg3 arg3)
                          (assert converted-body body)
                          ;; NOTE The order of these is unpredictable.
                          (map (lambda (i v)
                                 (cond ((equal? (ast-number-value (ast-const-value i))
                                                (ast-number-value (ast-const-value first1)))
                                        (assert v var1))
                                       ((equal? (ast-number-value (ast-const-value i))
                                                (ast-number-value (ast-const-value second1)))
                                        (assert v var2))
                                       ((equal? (ast-number-value (ast-const-value i))
                                                (ast-number-value (ast-const-value third1)))
                                        (assert v var3))))
                               (list first2 second2 third2)
                               (list converted-var4 converted-var5 converted-var6))
                          ;; NOTE We expect three empty lists and one extra-fv symbol.
                          (assert (length (filter ast-const? converted-args)) 3)
                          (assert (ast-symbol-value (car (filter ast-symbol? converted-args))) extra-fv))
              (assert (ast-node-location result)
                      (ast-node-location node))
              (assert (generated? result))))))
