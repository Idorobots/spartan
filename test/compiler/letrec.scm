;; Letrec conversion tests.

(define (check-deref arg node result)
  (assert-ast result (app 'deref ,inserted-node)
              (assert inserted-node node))
  (assert (generated? result))
  (assert (get-free-vars result) (set 'deref arg))
  (assert (get-location result) (get-location node))
  (assert (generated? (ast-app-op result)))
  (assert (get-location (ast-app-op result)) (get-location node)))

(describe
 "derefy"
 (it "leaves unassigned free variables intact"
     (check ((node gen-ast-node))
            (assert (derefy '() node)
                    node))
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (fun (gen-lambda-node (list node) node))
             (f (bound-vars (list arg) fun)))
            (assert (derefy (list arg) f)
                    f))
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (value gen-ast-node)
             (binding (make-binding node value))
             (let-node (gen-let-node (list binding) node))
             (l (bound-vars (list arg) let-node)))
            (assert (derefy (list arg) l)
                    l))
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (value gen-ast-node)
             (binding (make-binding node value))
             (let-node (gen-letrec-node (list binding) node))
             (l (bound-vars (list arg) let-node)))
            (assert (derefy (list arg) l)
                    l))
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (binding (gen-binding node
                                   node))
             (let-node (gen-letrec-node (list binding) gen-simple-node))
             (l (bound-vars (set arg) let-node)))
            (assert (derefy (list arg) l)
                    l)))

 (it "replaces assigned free variables with derefs in simple cases"
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg)))
            (check-deref arg node (derefy (list arg) node)))
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (op gen-valid-symbol-node)
             (app (gen-app-node op node)))
            (let ((result (derefy (list arg) app)))
              (assert-ast result (app ,operator ,deref)
                          (assert operator op)
                          (check-deref arg node deref))
              (assert (get-location result) (get-location app)))))

 (it "replaces assigned free variables with derefs in lambda"
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (fun (gen-lambda-node (gen-arg-list 2) node)))
            (let ((result (derefy (list arg) fun)))
              (assert-ast result (lambda _ ,body)
                          (check-deref arg node body))
              (assert (get-location result) (get-location fun)))))

 (it "replaces assigned free variables with derefs in let"
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (l (gen-let-node (gen-binding-list 2) node)))
            (let ((result (derefy (list arg) l)))
              (assert-ast result (let _ ,body)
                          (check-deref arg node body))
              (assert (get-location result) (get-location l))))
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (binding (gen-binding gen-valid-symbol-node node))
             (l (gen-let-node (list binding) gen-simple-node)))
            (let ((result (derefy (list arg) l)))
              (assert-ast result (let ((binding _ ,deref)) _)
                          (check-deref arg node deref))
              (assert (get-location result) (get-location l))))
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (binding (gen-binding node node))
             (let-node (gen-let-node (list binding) gen-simple-node))
             (l (bound-vars (set arg) let-node)))
            (let ((result (derefy (list arg) l)))
              (assert-ast result (let ((binding ,var ,deref)) _)
                          (assert var node)
                          (check-deref arg node deref))
              (assert (get-location result) (get-location l)))))

 (it "replaces assigned free variables with derefs in letrec"
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (l (gen-letrec-node (gen-binding-list 2) node)))
            (let ((result (derefy (list arg) l)))
              (assert-ast result (letrec _ ,body)
                          (check-deref arg node body))
              (assert (get-location result) (get-location l))))
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (binding (gen-binding gen-valid-symbol-node node))
             (l (gen-letrec-node (list binding) gen-simple-node)))
            (let ((result (derefy (list arg) l)))
              (assert-ast result (letrec ((binding _ ,deref)) _)
                          (check-deref arg node deref))
              (assert (get-location result) (get-location l))))))

(describe
 "safe-symbol-value"
 (it "should always return a name"
     (check ((name gen-valid-symbol)
             (node (gen-symbol-node name)))
            (assert (safe-symbol-value node) name))
     (check ((name gen-valid-symbol)
             (node (gen-symbol-node name))
             (error (gen-error-node node)))
            (assert (safe-symbol-value error) name))
     (check ((error gen-random-error-node))
            (unless (symbol-node? (ast-error-expr error))
                (assert (safe-symbol-value error) '<error>)))))

(describe
 "reconstruct-let-node"
 (it "should correctly recompute free & bound vars"
     (check ((body-fv (gen-list (gen-integer 3 5) gen-valid-symbol))
             (b gen-complex-node)
             (body (free-vars (apply set body-fv) b))
             (let-bv (take body-fv 2))
             (vars (gen-specific-list gen-symbol-node let-bv))
             (vals (gen-specific-list (lambda (_)
                                        gen-complex-node)
                                      let-bv))
             (vals-fv (gen-list (length let-bv) gen-valid-symbol))
             (bindings (map (lambda (var val fv)
                              (make-binding var
                                            (free-vars (set fv)
                                                       val)))
                            vars
                            vals
                            vals-fv))
             (parent gen-ast-node)
             (expected-fv (append (drop body-fv 2)
                                  vals-fv)))
            (assert (reconstruct-let-node parent '() body)
                    body)
            (let ((result (reconstruct-let-node parent bindings body)))
              (assert (let-node? result))
              (assert (get-location result) (get-location parent))
              (assert (get-bound-vars result) (apply set let-bv))
              (assert (get-free-vars result) (apply set expected-fv))
              (assert (ast-let-body result) body)
              (assert (ast-let-bindings result) bindings)))))

(describe
 "fix"
 (it "should correctly recompute free & bound vars"
     (check ((body-fv (gen-list (gen-integer 3 5) gen-valid-symbol))
             (b gen-complex-node)
             (body (free-vars (apply set body-fv) b))
             (let-bv (take body-fv 2))
             (vars (gen-specific-list gen-symbol-node let-bv))
             (vals (gen-specific-list (lambda (_)
                                        gen-complex-node)
                                      let-bv))
             (vals-fv (gen-list (length let-bv) gen-valid-symbol))
             (bindings (map (lambda (var val fv)
                              (make-binding var
                                            (free-vars (set fv)
                                                       val)))
                            vars
                            vals
                            vals-fv))
             (parent gen-ast-node)
             (expected-fv (append (drop body-fv 2)
                                  vals-fv)))
            (assert (fix parent '() body)
                    body)
            (let ((result (fix parent bindings body)))
              (assert (fix-node? result))
              (assert (get-location result) (get-location parent))
              (assert (get-bound-vars result) (apply set let-bv))
              (assert (get-free-vars result) (apply set expected-fv))
              (assert (ast-fix-body result) body)
              (assert (ast-fix-bindings result) bindings)))))

(describe
 "recoursive?"
 (it "should correctly assess recoursivity of binding groups"
     (check ((var gen-valid-symbol)
             (node (gen-symbol-node var))
             (rec-node gen-complex-node)
             (rec (free-vars (set var) rec-node))
             (rec-binding (gen-binding node rec))
             (non-rec-binding gen-valid-binding)
             (multiple-bindings (gen-binding-list (gen-integer 2 5))))
            (assert (not (recoursive? '())))
            (assert (not (recoursive? (list non-rec-binding))))
            (assert (recoursive? (list rec-binding)))
            (assert (recoursive? multiple-bindings)))))

(describe
 "derive-graph"
 (it "should correctly derive variable dependencies"
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding n1 gen-ast-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding n2 gen-ast-node))
             (node (gen-letrec-node (list b1 b2) gen-simple-node))
             (l (bound-vars (set v1 v2) node)))
            (assert (derive-dependencies l) '()))
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding n1 gen-ast-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding n2 n1))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding n3 n2))
             (node (gen-letrec-node (list b1 b2 b3) gen-simple-node))
             (l (bound-vars (set v1 v2 v3) node)))
            (assert (derive-dependencies l) `((,v3 ,v2) (,v2 ,v1))))
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding n1 gen-ast-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding n2 n1))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding n3 n1))
             (node (gen-letrec-node (list b1 b2 b3) gen-simple-node))
             (l (bound-vars (set v1 v2 v3) node)))
            (assert (derive-dependencies l) `((,v3 ,v1) (,v2 ,v1))))
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b1 (gen-binding n1 n3))
             (b2 (gen-binding n2 n1))
             (b3 (gen-binding n3 n1))
             (node (gen-letrec-node (list b1 b2 b3) gen-simple-node))
             (l (bound-vars (set v1 v2 v3) node)))
            (assert (derive-dependencies l) `((,v3 ,v1) (,v2 ,v1) (,v1 ,v3)))))

 (it "should correctly derive variable ordering"
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding n1 gen-non-value-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding n2 gen-non-value-node))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding n3 gen-non-value-node))
             (node (gen-letrec-node (list b3 b2 b1) gen-simple-node))
             (l (bound-vars (set v1 v2 v3) node)))
            (assert (derive-ordering l) `((,v2 ,v3) (,v1 ,v2))))
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding n1 gen-non-value-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding n2 gen-non-value-node))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding n3 gen-non-value-node))
             (node (gen-letrec-node (list b1 b2 b3) gen-simple-node))
             (l (bound-vars (set v1 v2 v3) node)))
            (assert (derive-ordering l) `((,v2 ,v1) (,v3 ,v2)))))

 (it "should not derive variable ordering for simple values"
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding n1 gen-non-value-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding n2 (gen-number-node 23)))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding n3 gen-non-value-node))
             (node (gen-letrec-node (list b1 b2 b3) gen-simple-node))
             (l (bound-vars (set v1 v2 v3) node)))
            (assert (derive-ordering l) `((,v3 ,v1))))))

(describe
 "reorder-bindings"
 (it "should correctly split bindings into multiple nested groups"
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding n1 gen-ast-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding n2 gen-ast-node))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding n3 gen-ast-node))
             (bindings (list b1 b2 b3))
             (body gen-simple-node)
             (l (gen-letrec-node bindings body))
             (parent (bound-vars (set v1 v2 v3) l)))
            (assert (reorder-bindings `((,v3) (,v2) (,v1)) fix parent bindings body)
                    (generated
                     (reconstruct-let-node parent
                                           (list b3)
                                           (generated
                                            (reconstruct-let-node parent
                                                                  (list b2)
                                                                  (generated
                                                                   (reconstruct-let-node parent
                                                                                         (list b1)
                                                                                         body)))))))
            (assert (reorder-bindings `((,v3) (,v1 ,v2)) fix parent bindings body)
                    (generated
                     (reconstruct-let-node parent
                                           (list b3)
                                           (generated
                                            (fix parent
                                                 (list b1 b2)
                                                 body)))))
            (assert (reorder-bindings `((,v1 ,v2 ,v3)) fix parent bindings body)
                    (generated
                     (fix parent
                          (list b1 b2 b3)
                          body)))))

 (it "should correctly handle self-recoursion"
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding n1 gen-ast-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding n2 gen-ast-node))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (rec-node gen-complex-node)
             (rec (free-vars (set v3) rec-node))
             (b3 (gen-binding n3 rec))
             (bindings (list b1 b2 b3))
             (body gen-simple-node)
             (l (gen-letrec-node bindings body))
             (parent (bound-vars (set v1 v2 v3) l)))
            (assert (reorder-bindings `((,v3) (,v2) (,v1)) fix parent bindings body)
                    (generated
                     (fix parent
                          (list b3)
                          (generated
                           (reconstruct-let-node parent
                                                 (list b2)
                                                 (generated
                                                  (reconstruct-let-node parent
                                                                        (list b1)
                                                                        body))))))))))

(describe
 "waddel"
 (it "should correctly split bindings into multiple groups"
     todo))

(describe
 "let-ref-assign"
 (it "should correctly introduce assignments"
     todo))

(describe
 "ref-conversion"
 (it "should correctly rewrite letrec expressions"
     todo))
