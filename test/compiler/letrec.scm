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
             (fun (gen-with-bv (gen-lambda-node (list node) node) (list arg))))
            (assert (derefy (list arg) fun)
                    fun))
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (value gen-ast-node)
             (binding (gen-binding-node node value))
             (let-node (gen-with-bv (gen-let-node (list binding) node) (list arg))))
            (assert (derefy (list arg) let-node)
                    let-node))
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (value gen-ast-node)
             (binding (gen-binding-node node value))
             (let-node (gen-with-bv (gen-letrec-node (list binding) node) (list arg))))
            (assert (derefy (list arg) let-node)
                    let-node))
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (binding (gen-binding-node node node))
             (let-node (gen-with-bv (gen-letrec-node (list binding) gen-simple-node) (set arg))))
            (assert (derefy (list arg) let-node)
                    let-node)))

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
             (binding (gen-binding-node gen-valid-symbol-node node))
             (l (gen-let-node (list binding) gen-simple-node)))
            (let ((result (derefy (list arg) l)))
              (assert-ast result (let ((binding _ ,deref)) _)
                          (check-deref arg node deref))
              (assert (get-location result) (get-location l))))
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (binding (gen-binding-node node node))
             (let-node (gen-with-bv (gen-let-node (list binding) gen-simple-node) (set arg))))
            (let ((result (derefy (list arg) let-node)))
              (assert-ast result (let ((binding ,var ,deref)) _)
                          (assert var node)
                          (check-deref arg node deref))
              (assert (get-location result) (get-location let-node)))))

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
             (binding (gen-binding-node gen-valid-symbol-node node))
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
             (body (gen-with-fv gen-complex-node (apply set body-fv)))
             (let-bv (take body-fv 2))
             (vars (gen-specific-list gen-symbol-node let-bv))
             (vals (gen-specific-list (lambda (_)
                                        gen-complex-node)
                                      let-bv))
             (vals-fv (gen-list (length let-bv) gen-valid-symbol))
             (bindings (map (lambda (sym var val fv)
                              (at (get-location var)
                                  (free-vars (set fv)
                                             (bound-vars (set sym)
                                                         (make-binding-node var val)))))
                            let-bv
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
             (body (gen-with-fv gen-non-value-node (apply set body-fv)))
             (let-bv (take body-fv 2))
             (vars (gen-specific-list gen-symbol-node let-bv))
             (vals (gen-specific-list (lambda (_)
                                        gen-non-value-node)
                                      let-bv))
             (vals-fv (gen-list (length let-bv) gen-valid-symbol))
             (bindings (map (lambda (sym var val fv)
                              (at (get-location var)
                                  (free-vars (set fv)
                                             (bound-vars (set sym)
                                                         (make-binding-node var val)))))
                            let-bv
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
             (rec (gen-with-fv gen-complex-node (set var)))
             (rec-binding (gen-binding-node node rec))
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
             (b1 (gen-binding-node n1 gen-ast-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding-node n2 gen-ast-node))
             (l (gen-with-bv (gen-letrec-node (list b1 b2) gen-simple-node) (set v1 v2))))
            (assert (derive-dependencies l) '()))
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding-node n1 gen-ast-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding-node n2 n1))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding-node n3 n2))
             (l (gen-with-bv (gen-letrec-node (list b1 b2 b3) gen-simple-node) (set v1 v2 v3))))
            (assert (derive-dependencies l) `((,v3 ,v2) (,v2 ,v1))))
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding-node n1 gen-ast-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding-node n2 n1))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding-node n3 n1))
             (l (gen-with-bv (gen-letrec-node (list b1 b2 b3) gen-simple-node) (set v1 v2 v3))))
            (assert (derive-dependencies l) `((,v3 ,v1) (,v2 ,v1))))
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b1 (gen-binding-node n1 n3))
             (b2 (gen-binding-node n2 n1))
             (b3 (gen-binding-node n3 n1))
             (l (gen-with-bv (gen-letrec-node (list b1 b2 b3) gen-simple-node) (set v1 v2 v3))))
            (assert (derive-dependencies l) `((,v3 ,v1) (,v2 ,v1) (,v1 ,v3)))))

 (it "should correctly derive variable ordering"
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding-node n1 gen-non-value-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding-node n2 gen-non-value-node))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding-node n3 gen-non-value-node))
             (l (gen-with-bv (gen-letrec-node (list b3 b2 b1) gen-simple-node) (set v1 v2 v3))))
            (assert (derive-ordering l) `((,v2 ,v3) (,v1 ,v2))))
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding-node n1 gen-non-value-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding-node n2 gen-non-value-node))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding-node n3 gen-non-value-node))
             (l (gen-with-bv (gen-letrec-node (list b1 b2 b3) gen-simple-node) (set v1 v2 v3))))
            (assert (derive-ordering l) `((,v2 ,v1) (,v3 ,v2)))))

 (it "should not derive variable ordering for simple values"
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding-node n1 gen-non-value-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding-node n2 (gen-number-node 23)))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding-node n3 gen-non-value-node))
             (l (gen-with-bv (gen-letrec-node (list b1 b2 b3) gen-simple-node) (set v1 v2 v3))))
            (assert (derive-ordering l) `((,v3 ,v1))))))

(describe
 "reorder-bindings"
 (it "should correctly split bindings into multiple nested groups"
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding-node n1 gen-ast-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding-node n2 gen-ast-node))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding-node n3 gen-ast-node))
             (bindings (list b1 b2 b3))
             (body gen-simple-node)
             (parent (gen-with-bv (gen-letrec-node bindings body) (set v1 v2 v3))))
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
             (b1 (gen-binding-node n1 gen-ast-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding-node n2 gen-ast-node))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (rec (gen-with-fv gen-complex-node (set v3)))
             (b3 (gen-binding-node n3 rec))
             (bindings (list b1 b2 b3))
             (body gen-simple-node)
             (parent (gen-with-bv (gen-letrec-node bindings body) (set v1 v2 v3))))
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
 "waddell"
 (it "should correctly split bindings into multiple groups"
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding-node n1 gen-value-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding-node n2 gen-non-value-node))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding-node n3 gen-valid-lambda-node))
             (bindings (list b1 b2 b3))
             (body gen-simple-node)
             (parent (gen-with-bv (gen-letrec-node bindings body) (set v1 v2 v3))))
            (assert (waddell fix reconstruct-let-node parent bindings body)
                    (generated
                     (reconstruct-let-node parent
                                           (list b1)
                                           ;; NOTE Complex group is not marked as generated, since it's using plain reconstruct-let-node.
                                           (reconstruct-let-node parent
                                                                 (list b2)
                                                                 (generated
                                                                  (reconstruct-let-node parent
                                                                                        (list b3)
                                                                                        body))))))))

 (it "should omit empty groups"
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding-node n1 gen-value-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding-node n2 gen-valid-lambda-node))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding-node n3 gen-valid-lambda-node))
             (bindings (list b1 b2 b3))
             (body gen-simple-node)
             (parent (gen-with-bv (gen-letrec-node bindings body) (set v1 v2 v3))))
            (assert (waddell fix reconstruct-let-node parent bindings body)
                    (generated
                     (reconstruct-let-node parent
                                           (list b1)
                                           ;; NOTE No complex group at all.
                                           (generated
                                            (fix parent
                                                 (list b2 b3)
                                                 body))))))
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding-node n1 gen-value-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding-node n2 gen-non-value-node))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding-node n3 gen-non-value-node))
             (bindings (list b1 b2 b3))
             (body gen-simple-node)
             (parent (gen-with-bv (gen-letrec-node bindings body) (set v1 v2 v3))))
            (assert (waddell fix reconstruct-let-node parent bindings body)
                    (generated
                     (reconstruct-let-node parent
                                           (list b1)
                                           ;; NOTE Complex group (not marked generated) is present.
                                           (reconstruct-let-node parent
                                                                 (list b2 b3)
                                                                 ;; NOTE But no lambdas are.
                                                                 body)))))
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding-node n1 gen-value-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding-node n2 gen-value-node))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding-node n3 gen-value-node))
             (bindings (list b1 b2 b3))
             (body gen-simple-node)
             (parent (gen-with-bv (gen-letrec-node bindings body) (set v1 v2 v3))))
            (assert (waddell fix reconstruct-let-node parent bindings body)
                    ;; NOTE Only simple value group is present.
                    (generated
                     (reconstruct-let-node parent
                                           (list b1 b2 b3)
                                           body))))))

(define (gen-ref value)
  (free-vars
   (set 'ref)
   (let ((l (get-location value)))
     (at l
         (generated
          (make-app-node
           (at l
               (generated
                (make-symbol-node 'ref)))
           (list (at l
                     (generated
                      (make-quote-node
                       (at l
                           (generated
                            (make-list-node '())))))))))))))

(describe
 "let-ref-assign"
 (it "should correctly introduce assignments"
     (check ((v gen-valid-symbol)
             (var (gen-symbol-node v))
             (val-fv (gen-list (gen-integer 3 5) gen-valid-symbol))
             (val (gen-with-fv gen-non-value-node (apply set val-fv)))
             (b (gen-binding-node var val))
             (bindings (list b))
             (body-fv (gen-list (gen-integer 3 5) gen-valid-symbol))
             (body (gen-with-fv gen-non-value-node (apply set body-fv)))
             (parent (gen-with-bv (gen-letrec-node bindings body) (set v))))
            (assert (let-ref-assign parent bindings body)
                    (generated
                     (reconstruct-let-node parent
                                           (list (at (get-location b)
                                                     (complexity 'complex
                                                                 (make-binding-node var (gen-ref val)))))
                                           (free-vars
                                            (set-sum (list (apply set val-fv)
                                                           (apply set body-fv)
                                                           (set v 'assign! 'deref)))
                                            (at (get-location body)
                                                (generated
                                                 (make-do-node
                                                  (list (free-vars
                                                         (set-union (apply set val-fv)
                                                                    (set v 'assign!))
                                                         (let ((l (get-location val)))
                                                           (at l
                                                               (generated
                                                                (make-app-node
                                                                 (at l
                                                                     (generated
                                                                      (make-symbol-node 'assign!)))
                                                                 (list var
                                                                       val))))))
                                                        body))))))))))

 (it "should correctly introduce derefs"
     (check ((v gen-valid-symbol)
             (var (gen-symbol-node v))
             (val-fv (gen-list (gen-integer 3 5) gen-valid-symbol))
             (val (gen-with-fv gen-non-value-node (apply set val-fv)))
             (b (gen-binding-node var val))
             (bindings (list b))
             (parent (gen-with-bv (gen-letrec-node bindings var) (set v))))
            (assert (let-ref-assign parent bindings var)
                    (generated
                     (reconstruct-let-node parent
                                           (list (at (get-location b)
                                                     (complexity 'complex
                                                                 (make-binding-node var (gen-ref val)))))
                                           (free-vars
                                            (set-union (apply set val-fv)
                                                       (set v 'assign! 'deref))
                                            (at (get-location var)
                                                (generated
                                                 (make-do-node
                                                  (list (free-vars
                                                         (set-union (apply set val-fv)
                                                                    (set v 'assign!))
                                                         (let ((l (get-location val)))
                                                           (at l
                                                               (generated
                                                                (make-app-node
                                                                 (at l
                                                                     (generated
                                                                      (make-symbol-node 'assign!)))
                                                                 (list var
                                                                       val))))))
                                                        (derefy (set v)
                                                                var))))))))))))

(describe
 "ref-conversion"
 (it "should correctly rewrite letrec expressions"
     (check ((sym1 gen-valid-symbol)
             (var1 (gen-symbol-node sym1))
             (sym2 gen-valid-symbol)
             (var2 (gen-symbol-node sym2))
             (sym3 gen-valid-symbol)
             (var3 (gen-symbol-node sym3))
             (sym4 gen-valid-symbol)
             (var4 (gen-symbol-node sym4))
             (sym5 gen-valid-symbol)
             (var5 (gen-symbol-node sym5))
             (sym6 gen-valid-symbol)
             (var6 (gen-symbol-node sym6))
             ;; value1 <- complex2 <- complex5 <- (complex6 lambda3 lambda4)
             (value1 gen-value-node)
             (complex2 (gen-with-fv gen-non-value-node (set sym1)))
             (lambda3 (gen-with-fv gen-valid-lambda-node (set sym6 sym4)))
             (lambda4 (gen-with-fv gen-valid-lambda-node (set sym1 sym3)))
             (complex5 (gen-with-fv gen-non-value-node (set sym2)))
             (complex6 (gen-with-fv gen-non-value-node (set sym5 sym3)))
             ;; NOTE Order doesn't really matter here.
             (b1 (gen-binding-node var1 value1))
             (b2 (gen-binding-node var2 complex2))
             (b3 (gen-binding-node var3 lambda3))
             (b4 (gen-binding-node var4 lambda4))
             (b5 (gen-binding-node var5 complex5))
             (b6 (gen-binding-node var6 complex6))
             (bindings (list b3 b1 b6 b4 b2 b5))
             (body-fv (gen-list (gen-integer 3 5) gen-valid-symbol))
             (body (gen-with-fv gen-non-value-node body-fv))
             (parent (gen-with-bv (gen-letrec-node bindings body)
                                  (set sym1 sym2 sym3 sym4 sym5 sym6))))
            (assert (ref-conversion parent)
                    (generated
                     (reconstruct-let-node
                      parent
                      (list b1)
                      (generated
                       (reconstruct-let-node
                        parent
                        (list b2)
                        (generated
                         (reconstruct-let-node
                         parent
                         (list b5)
                         (ast-update (let-ref-assign parent (list b6) body)
                                     'body
                                     (lambda (b)
                                       (generated
                                        (fix parent (list b3 b4) b))))))))))))))
