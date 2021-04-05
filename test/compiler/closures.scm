;; Closure conversion

(describe
 "make-env"
 (it "should create environment correctly"
     (check ((free-vars '())
             (loc gen-location))
            (let ((result (make-env loc free-vars)))
              (assert-ast result
                          (a-quote (list))
                          (assert (generated? result))
                          (assert (get-location result) loc))))
     (check ((free-vars (gen-list 1 gen-valid-symbol))
             (loc gen-location))
            (let ((result (make-env loc free-vars)))
              (assert (ast-symbol-value result) (car free-vars))
              (assert (get-location result) loc)))
     (check ((free-vars (gen-list 2 gen-valid-symbol))
             (loc gen-location))
            (let ((result (make-env loc free-vars)))
              (assert-ast result
                          (primop-app '&cons ,first ,second)
                          (assert (ast-symbol-value first) (car free-vars))
                          (assert (ast-symbol-value second) (cadr free-vars))
                          (assert (get-location result) loc))))
     (check ((free-vars (gen-list (gen-integer 3 5) gen-valid-symbol))
             (loc gen-location))
            (let ((result (make-env loc free-vars)))
              (assert-ast result
                          (primop-app '&make-env . ,args)
                          (assert (map ast-symbol-value args) free-vars)
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
                   (result (substitute subs node)))
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
                   (result (substitute subs node)))
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
                   (result (substitute subs node)))
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
     todo))

;; (describe
;;  "old closure-convert"
;;  (it "Converting fix works."
;;      (gensym-reset!)
;;      (assert (old-closure-convert '(fix ((foo (lambda () (foo))))
;;                                     (foo 23))
;;                               '())
;;              '(let ((env2 '()))
;;                 (let ((foo (&make-closure env2
;;                                           (lambda (env1)
;;                                             (&apply env1)))))
;;                   (do (&set-closure-env! foo foo)
;;                       (&apply foo 23)))))
;;      (gensym-reset!)
;;      (assert (old-closure-convert '(fix ((foo (lambda () (bar)))
;;                                      (bar (lambda () (foo))))
;;                                     (foo))
;;                               '())
;;              '(let ((env3 '())
;;                     (env4 '()))
;;                 (let ((foo (&make-closure env3
;;                                           (lambda (env1)
;;                                             (&apply env1))))
;;                       (bar (&make-closure env4
;;                                           (lambda (env2)
;;                                             (&apply env2)))))
;;                   (do (&set-closure-env! bar foo)
;;                       (&set-closure-env! foo bar)
;;                     (&apply foo))))))

;;  (it "Complex examples work."
;;      (gensym-reset!)
;;      (assert (old-closure-convert
;;               '(lambda (n cont4)
;;                  (fact
;;                   fact
;;                   (lambda (value11)
;;                     (let ((fact value11))
;;                       23))))
;;               '())
;;              '(&make-closure
;;                fact
;;                (lambda (env2 n cont4)
;;                  (&apply
;;                   env2
;;                   env2
;;                   (&make-closure
;;                    '()
;;                    (lambda (env1 value11)
;;                      (let ((fact value11))
;;                        23)))))))
;;      (gensym-reset!)
;;      (assert (old-closure-convert
;;               '(let ((foo (lambda (foo bar)
;;                             (lambda ()
;;                               (let ((bar (bar foo bar)))
;;                                 bar))))
;;                      (bar (lambda (foo bar)
;;                             (lambda ()
;;                               (let ((foo (foo foo bar)))
;;                                 foo)))))
;;                  (let ((foo (foo foo bar))
;;                        (bar (bar foo bar)))
;;                    (list (foo) (bar))))
;;               '())
;;              '(let ((foo (&make-closure
;;                           '()
;;                           (lambda (env2 foo bar)
;;                             (&make-closure
;;                              (&cons bar foo)
;;                              (lambda (env1)
;;                                (let ((bar (&apply (&car env1) (&cdr env1) (&car env1))))
;;                                  bar))))))
;;                     (bar (&make-closure
;;                           '()
;;                           (lambda (env4 foo bar)
;;                             (&make-closure
;;                              (&cons bar foo)
;;                              (lambda (env3)
;;                                (let ((foo (&apply (&cdr env3) (&cdr env3) (&car env3))))
;;                                  foo)))))))
;;                 (let ((foo (&apply foo foo bar))
;;                       (bar (&apply bar foo bar)))
;;                   (&apply list (&apply foo) (&apply bar)))))))
