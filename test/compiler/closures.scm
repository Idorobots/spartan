;; Closure conversion

(describe
 "make-env"
 (it "should create environment correctly"
     (check ((free-vars '())
             (loc gen-location))
            (let ((result (make-env loc free-vars '())))
              (assert-ast result
                          (a-quote (list))
                          (assert (generated? result))
                          (assert (get-location result) loc))))
     (check ((free-vars (gen-list 1 gen-valid-symbol))
             (loc gen-location))
            (let ((result (make-env loc free-vars '())))
              (assert (ast-symbol-value result) (car free-vars))
              (assert (get-location result) loc)))
     (check ((free-vars (gen-list 2 gen-valid-symbol))
             (loc gen-location))
            (let ((result (make-env loc free-vars '())))
              (assert-ast result
                          (primop-app '&cons ,first ,second)
                          (assert (ast-symbol-value first) (car free-vars))
                          (assert (ast-symbol-value second) (cadr free-vars))
                          (assert (get-location result) loc))))
     (check ((free-vars (gen-list (gen-integer 3 5) gen-valid-symbol))
             (loc gen-location))
            (let ((result (make-env loc free-vars '())))
              (assert-ast result
                          (primop-app '&make-env . ,args)
                          (assert (map ast-symbol-value args) free-vars)
                          (assert (get-location result) loc)))))

 (it "should re-use bound names to preserve proper source locations"
     (check ((closures (gen-list (gen-integer 3 5) gen-valid-symbol-node))
             (free-vars (map ast-symbol-value closures))
             (loc gen-location))
            (let ((result (make-env loc free-vars closures)))
              (assert-ast result
                          (primop-app '&make-env . ,args)
                          (assert args closures)
                          (assert (get-location result) loc))))))

(describe
 "make-env-subs"
 (it "should create environment substitutions correctly"
     (check ((env gen-valid-symbol-node)
             (free-vars '()))
            (assert (empty? (make-env-subs env free-vars))))
     (check ((env gen-valid-symbol-node)
             (nodes (gen-arg-list 1))
             (node (apply gen-specific-do-node nodes))
             (free-vars (map ast-symbol-value nodes)))
            (let* ((subs (make-env-subs env free-vars))
                   (result (substitute-symbols subs node)))
              (assert-ast result
                          (do ,converted-var)
                          (assert converted-var
                                  (at (get-location (car nodes))
                                      env)))))
     (check ((env gen-valid-symbol-node)
             (nodes (gen-arg-list 2))
             (node (apply gen-specific-do-node nodes))
             (free-vars (map ast-symbol-value nodes)))
            (let* ((subs (make-env-subs env free-vars))
                   (result (substitute-symbols subs node)))
              (assert-ast result
                          (do (primop-app '&car ,converted-var1)
                              (primop-app '&cdr ,converted-var2))
                          (assert converted-var1 env)
                          (assert converted-var2 env))))
     (check ((env gen-valid-symbol-node)
             (nodes (gen-arg-list (gen-integer 3 5)))
             (node (apply gen-specific-do-node nodes))
             (free-vars (map ast-symbol-value nodes)))
            (let* ((subs (make-env-subs env free-vars))
                   (result (substitute-symbols subs node)))
              (assert-ast result
                          (do (primop-app '&env-ref ,converted-env1 '0)
                              (primop-app '&env-ref ,converted-env2 '1)
                              (primop-app '&env-ref ,converted-env3 '2)
                              .
                              ,rest)
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
             (bound (apply set (map ast-symbol-value closures)))
             (free bound))
            (let* ((env (make-env loc free closures))
                   (result (make-env-setters env env-var free bound closures)))
              (assert (length result) (length closures))
              (map (lambda (setter var)
                     (assert-ast setter
                                 (primop-app '&set-closure-env! ,converted-closure-var ,converted-env)
                                 (assert (ast-symbol-value converted-closure-var) var)
                                 (assert converted-env env)))
                   result
                   bound))))

 (it "should correctly patch large shared envs"
     (check ((env-var gen-valid-symbol-node)
             (loc gen-location)
             (closures (gen-arg-list (gen-integer 3 5)))
             (bound (apply set (map ast-symbol-value closures)))
             (extra-free (gen-list (gen-integer 0 5) gen-valid-symbol))
             (free (apply set (append bound extra-free))))
            (let* ((env (make-env loc free closures))
                   (result (make-env-setters env env-var free bound closures)))
              (assert (length result) (length closures))
              (map (lambda (setter var)
                     (assert-ast setter
                                 (primop-app '&set-env! ,converted-env-var ,converted-offset ,converted-var)
                                 (assert converted-env-var env-var)
                                 (assert (ast-number-value converted-offset) (offset var free))
                                 (assert (ast-symbol-value converted-var) var)))
                   result
                   bound)))))

(describe
 "closure-convert"
 (it "should convert application correctly"
     (check ((app gen-valid-app-node))
            (let ((result (convert-closures app '())))
              (assert-ast result
                          (primop-app '&apply ,op . ,args)
                          (assert op (ast-app-op app))
                          (assert args (ast-app-args app)))
              (assert (get-location result)
                      (get-location app)))))

 (it "should convert lambdas correctly"
     (check ((node gen-valid-lambda-node))
            (gensym-reset!)
            (let ((result (convert-closures node '())))
              (assert-ast result
                          (primop-app '&make-closure
                                      (a-quote (list))
                                      (lambda ('env1 . ,formals)
                                        ,body))
                          (assert formals (ast-lambda-formals node))
                          (assert body (ast-lambda-body node)))
              (assert (get-location result)
                      (get-location node))))
     (check ((arg gen-valid-symbol-node)
             (body gen-valid-symbol-node)
             (var (ast-symbol-value body))
             (node (gen-with-fv (gen-lambda-node (list arg) body) (set var))))
            (gensym-reset!)
            (let ((result (convert-closures node '())))
              (assert-ast result
                          (primop-app '&make-closure
                                      ,arg
                                      (lambda ('env1 _)
                                        'env1))
                          (assert (ast-symbol-value arg) var))
              (assert (get-location result)
                      (get-location node))))
     (check ((nodes (gen-arg-list 2))
             (free-vars (apply set (map ast-symbol-value nodes)))
             ;; NOTE These nodes need to be sorted, or othrwise the order of cars & cdrs is unpredictable.
             (body (apply gen-specific-do-node (sort nodes
                                                     (lambda (a b)
                                                       (symbol<? (ast-symbol-value a)
                                                                 (ast-symbol-value b))))))
             (node (gen-with-fv (gen-lambda-node '() body) free-vars)))
            (gensym-reset!)
            (let ((result (convert-closures node '())))
              (assert-ast result
                          (primop-app '&make-closure
                                      (primop-app '&cons . ,args)
                                      (lambda ('env1)
                                        (do (primop-app '&car 'env1)
                                            (primop-app '&cdr 'env1))))
                          (assert (map ast-symbol-value args) free-vars))
              (assert (get-location result)
                      (get-location node))))
     (check ((nodes (gen-arg-list (gen-integer 3 5)))
             (free-vars (apply set (map ast-symbol-value nodes)))
             ;; NOTE These nodes need to be sorted, or othrwise the order of cars & cdrs is unpredictable.
             (body (apply gen-specific-do-node (sort nodes
                                                     (lambda (a b)
                                                       (symbol<? (ast-symbol-value a)
                                                                 (ast-symbol-value b))))))
             (node (gen-with-fv (gen-lambda-node '() body) free-vars)))
            (gensym-reset!)
            (let ((result (convert-closures node '())))
              (assert-ast result
                          (primop-app '&make-closure
                                      (primop-app '&make-env . ,args)
                                      (lambda ('env1)
                                        (do (primop-app '&env-ref 'env1 '0)
                                            (primop-app '&env-ref 'env1 '1)
                                            (primop-app '&env-ref 'env1 '2)
                                            .
                                            ,rest)))
                          (assert (map ast-symbol-value args) free-vars)
                          (assert (length rest) (- (length free-vars) 3)))
              (assert (get-location result)
                      (get-location node)))))

 (it "should take global values into account"
     (check ((arg gen-valid-symbol-node)
             (body gen-valid-symbol-node)
             (var (ast-symbol-value body))
             (node (gen-with-fv (gen-lambda-node (list arg) body) (set var))))
            (gensym-reset!)
            (let ((result (convert-closures node (set var))))
              (assert-ast result
                          (primop-app '&make-closure
                                      (a-quote (list))
                                      (lambda ('env1 _)
                                        ,converted-body))
                          (assert converted-body body))
              (assert (get-location result)
                      (get-location node)))))

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
                          (let ((binding 'env1 (a-quote (list))))
                            (let ((binding ,converted-var1
                                           (primop-app '&make-closure
                                                       'env1
                                                       (lambda ('env2 ,converted-arg)
                                                         'env2))))
                              (do (primop-app '&set-closure-env!
                                              ,converted-var2
                                              ,converted-var3)
                                  ,converted-body)))
                          (assert converted-var1 var1)
                          (assert converted-arg arg1)
                          (assert converted-var2 var1)
                          (assert converted-var3 var1)
                          (assert converted-body body))
              (assert (get-location result)
                      (get-location node))
              (assert (generated? result))))

     (check ((sym1 gen-valid-symbol)
             (var1 (gen-symbol-node sym1))
             (arg1 gen-valid-symbol-node)
             (lambda1 (gen-lambda-node (list arg1) var1))
             (b1 (gen-with-fv-bv (gen-binding-node var1 lambda1) (set sym1) (set sym1)))
             ;; NOTE This symbol needs to be lexicographically behind sym1 to make the test predictable.
             (sym2 (string->symbol
                    (string-append (symbol->string sym1)
                                   "-2")))
             (var2 (gen-symbol-node sym2))
             (arg2 gen-valid-symbol-node)
             (lambda2 (gen-lambda-node (list arg2) var2))
             (b2 (gen-with-fv-bv (gen-binding-node var2 lambda2) (set sym2) (set sym2)))
             (body gen-simple-node)
             (node (gen-with-bv (gen-fix-node (list b1 b2) body) (set sym1 sym2))))
            (gensym-reset!)
            (let ((result (convert-closures node (set))))
              (assert-ast result
                          (let ((binding 'env1 (primop-app '&cons
                                                           (a-quote (list))
                                                           (a-quote (list)))))
                            (let ((binding ,converted-var1
                                           (primop-app '&make-closure
                                                       'env1
                                                       (lambda ('env2 ,converted-arg1)
                                                         (primop-app '&car 'env2))))
                                  (binding ,converted-var2
                                           (primop-app '&make-closure
                                                       'env1
                                                       (lambda ('env3 ,converted-arg2)
                                                         (primop-app '&cdr 'env3)))))
                              (do (primop-app '&set-closure-env!
                                              ,converted-var3
                                              (primop-app '&cons
                                                          ,converted-var4
                                                          ,converted-var5))
                                  (primop-app '&set-closure-env!
                                              ,converted-var6
                                              (primop-app '&cons
                                                          ,converted-var7
                                                          ,converted-var8))
                                ,converted-body)))
                          (map (lambda (var)
                                 (assert var var1))
                               (list converted-var1
                                     converted-var3
                                     converted-var4
                                     converted-var7))
                          (map (lambda (var)
                                 (assert var var2))
                               (list converted-var2
                                     converted-var5
                                     converted-var6
                                     converted-var8))
                          (assert converted-arg1 arg1)
                          (assert converted-arg2 arg2)
                          (assert converted-body body))
              (assert (get-location result)
                      (get-location node))
              (assert (generated? result))))

     (define (add-suffix symbol suffix)
       (string->symbol
        (string-append (symbol->string symbol)
                       suffix)))

     (check ((sym1 gen-valid-symbol)
             (var1 (gen-symbol-node sym1))
             (arg1 gen-valid-symbol-node)
             (lambda1 (gen-lambda-node (list arg1) var1))
             (b1 (gen-with-fv-bv (gen-binding-node var1 lambda1) (set sym1) (set sym1)))
             ;; NOTE This symbol needs to be lexicographically behind sym1 to make the test predictable.
             (sym2 (add-suffix sym1 "-2"))
             (var2 (gen-symbol-node sym2))
             (arg2 gen-valid-symbol-node)
             (lambda2 (gen-lambda-node (list arg2) var2))
             (b2 (gen-with-fv-bv (gen-binding-node var2 lambda2) (set sym2) (set sym2)))
             ;; NOTE This symbol needs to be lexicographically behind sym2 to make the test predictable.
             (sym3 (add-suffix sym1 "-3"))
             (var3 (gen-symbol-node sym3))
             (arg3 gen-valid-symbol-node)
             (lambda3 (gen-lambda-node (list arg3) var3))
             ;; NOTE This symbol needs to be lexicographically behind sym3 to make the test predictable.
             (extra-fv (add-suffix sym1 "-3-fv"))
             (b3 (gen-with-fv-bv (gen-binding-node var3 lambda3) (set sym3 extra-fv) (set sym3)))
             (body gen-simple-node)
             (node (gen-with-bv (gen-fix-node (list b1 b2 b3) body) (set sym1 sym2 sym3))))
            (gensym-reset!)
            (let ((result (convert-closures node (set))))
              (assert-ast result
                          (let ((binding 'env1 (primop-app '&make-env
                                                           (a-quote (list))
                                                           (a-quote (list))
                                                           (a-quote (list))
                                                           ,converted-extra-fv)))
                            (let ((binding ,converted-var1
                                           (primop-app '&make-closure
                                                       'env1
                                                       (lambda ('env2 ,converted-arg1)
                                                         (primop-app '&env-ref 'env2 '0))))
                                  (binding ,converted-var2
                                           (primop-app '&make-closure
                                                       'env1
                                                       (lambda ('env3 ,converted-arg2)
                                                         (primop-app '&env-ref 'env3 '1))))
                                  (binding ,converted-var3
                                           (primop-app '&make-closure
                                                       'env1
                                                       (lambda ('env4 ,converted-arg3)
                                                         (primop-app '&env-ref 'env4 '2)))))
                              (do (primop-app '&set-env! 'env1 '0 ,converted-var4)
                                  (primop-app '&set-env! 'env1 '1 ,converted-var5)
                                  (primop-app '&set-env! 'env1 '2 ,converted-var6)
                                  ,converted-body)))
                          (assert converted-var1 var1)
                          (assert converted-var4 var1)
                          (assert converted-var2 var2)
                          (assert converted-var5 var2)
                          (assert converted-var3 var3)
                          (assert converted-var6 var3)
                          (assert (ast-symbol-value converted-extra-fv) extra-fv)
                          (assert converted-arg1 arg1)
                          (assert converted-arg2 arg2)
                          (assert converted-arg3 arg3)
                          (assert converted-body body))
              (assert (get-location result)
                      (get-location node))
              (assert (generated? result))))))
