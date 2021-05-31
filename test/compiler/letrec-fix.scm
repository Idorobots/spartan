;; Assignment conversion tests.

(define (check-deref arg node result)
  (assert-ast result (primop-app 'deref inserted-node)
              (assert inserted-node node))
  (assert (generated? result))
  (assert (ast-node-free-vars result) (set arg))
  (assert (ast-node-location result) (ast-node-location node)))

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
             (appl (gen-app-node op node)))
            (let ((result (derefy (list arg) appl)))
              (assert-ast result
                          (app operator deref)
                          (assert operator op)
                          (check-deref arg node deref))
              (assert (ast-node-location result)
                      (ast-node-location appl)))))

 (it "replaces assigned free variables with derefs in lambda"
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (fun (gen-lambda-node (gen-arg-list 2) node)))
            (let ((result (derefy (list arg) fun)))
              (assert-ast result
                          (lambda _ body)
                          (check-deref arg node body))
              (assert (ast-node-location result) (ast-node-location fun)))))

 (it "replaces assigned free variables with derefs in let"
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (l (gen-let-node (gen-binding-list 2) node)))
            (let ((result (derefy (list arg) l)))
              (assert-ast result
                          (let _ body)
                          (check-deref arg node body))
              (assert (ast-node-location result) (ast-node-location l))))
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (b (gen-binding-node gen-valid-symbol-node node))
             (l (gen-let-node (list b) gen-simple-node)))
            (let ((result (derefy (list arg) l)))
              (assert-ast result
                          (let ((binding _ deref)) _)
                          (check-deref arg node deref))
              (assert (ast-node-location result)
                      (ast-node-location l))))
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (b (gen-binding-node node node))
             (let-node (gen-with-bv (gen-let-node (list b) gen-simple-node) (set arg))))
            (let ((result (derefy (list arg) let-node)))
              (assert-ast result
                          (let ((binding var deref)) _)
                          (assert var node)
                          (check-deref arg node deref))
              (assert (ast-node-location result)
                      (ast-node-location let-node)))))

 (it "replaces assigned free variables with derefs in letrec"
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (l (gen-letrec-node (gen-binding-list 2) node)))
            (let ((result (derefy (list arg) l)))
              (assert-ast result
                          (letrec _ body)
                          (check-deref arg node body))
              (assert (ast-node-location result) (ast-node-location l))))
     (check ((arg gen-valid-symbol)
             (node (gen-symbol-node arg))
             (b (gen-binding-node gen-valid-symbol-node node))
             (l (gen-letrec-node (list b) gen-simple-node)))
            (let ((result (derefy (list arg) l)))
              (assert-ast result
                          (letrec ((binding _ deref)) _)
                          (check-deref arg node deref))
              (assert (ast-node-location result)
                      (ast-node-location l))))))

(define (gen-ref value)
  (let ((l (ast-node-location value)))
    (generated
     (make-ast-primop-app l
                          'ref
                          (list (generated
                                 (make-ast-const l
                                                 (generated
                                                  (make-ast-list l '())))))))))

(describe
 "let-ref-assign"
 (it "should correctly introduce assignments"
     (check ((v gen-valid-symbol)
             (var (gen-symbol-node v))
             (val-fv (gen-list (gen-integer 3 5) gen-valid-symbol))
             (val (gen-with-fv gen-non-value-node val-fv))
             (b (gen-binding-node var val))
             (bindings (list b))
             (body-fv (gen-list (gen-integer 3 5) gen-valid-symbol))
             (body (gen-with-fv gen-non-value-node body-fv))
             (parent (gen-with-bv (gen-letrec-node bindings body) (set v))))
            (assert (let-ref-assign parent bindings body)
                    (generated
                     (reconstruct-let-node parent
                                           (list (set-ast-binding-complexity
                                                  (make-ast-binding (ast-node-location b)
                                                                    var
                                                                    (gen-ref val))
                                                  'simple))
                                           (set-ast-node-free-vars
                                            (set-sum (list (apply set val-fv)
                                                           (apply set body-fv)
                                                           (set v)))
                                            (generated
                                             (make-ast-do (ast-node-location body)
                                                          (list (set-ast-node-free-vars
                                                                 (set-union (apply set val-fv)
                                                                            (set v))
                                                                 (generated
                                                                  (make-ast-primop-app (ast-node-location val)
                                                                                       'assign!
                                                                                       (list var val))))
                                                                body)))))))))

 (it "should correctly introduce derefs"
     (check ((v gen-valid-symbol)
             (var (gen-symbol-node v))
             (val-fv (gen-list (gen-integer 3 5) gen-valid-symbol))
             (val (gen-with-fv gen-non-value-node val-fv))
             (b (gen-binding-node var val))
             (bindings (list b))
             (parent (gen-with-bv (gen-letrec-node bindings var) (set v))))
            (assert (let-ref-assign parent bindings var)
                    (generated
                     (reconstruct-let-node parent
                                           (list (set-ast-binding-complexity
                                                  (make-ast-binding (ast-node-location b)
                                                                    var
                                                                    (gen-ref val))
                                                  'simple))
                                           (set-ast-node-free-vars
                                            (set-union (apply set val-fv)
                                                       (set v))
                                            (generated
                                             (make-ast-do (ast-node-location var)
                                                          (list (set-ast-node-free-vars
                                                                 (set-union (apply set val-fv)
                                                                            (set v))
                                                                 (generated
                                                                  (make-ast-primop-app (ast-node-location val)
                                                                                       'assign!
                                                                                       (list var val))))
                                                                (derefy (list v)
                                                                        var)))))))))))

(describe
 "waddell"
 (it "should correctly split bindings into multiple groups"
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding-node n1 gen-const-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding-node n2 gen-non-value-node))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding-node n3 gen-valid-lambda-node))
             (bindings (list b1 b2 b3))
             (body gen-simple-node)
             (parent (gen-with-bv (gen-letrec-node bindings body) (set v1 v2 v3))))
            (assert (waddell reconstruct-fix-node reconstruct-let-node parent bindings body)
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

 (it "should fix recursive lambdas"
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-self-recursive (gen-binding-node n1 gen-valid-lambda-node)))
             (bindings (list b1))
             (body gen-simple-node)
             (parent (gen-with-bv (gen-letrec-node bindings body) (set v1))))
            (assert (waddell reconstruct-fix-node reconstruct-let-node parent bindings body)
                    (generated
                     ;; NOTE Recursive lambda is fixed.
                     (reconstruct-fix-node parent
                                           (list b1)
                                           body)))))

 (it "should omit needless fix"
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding-node n1 gen-valid-lambda-node))
             (bindings (list b1))
             (body gen-simple-node)
             (parent (gen-with-bv (gen-letrec-node bindings body) (set v1))))
            (assert (waddell reconstruct-fix-node reconstruct-let-node parent bindings body)
                    (generated
                     ;; NOTE Non-recursive lambda is not fixed.
                     (reconstruct-let-node parent
                                           (list b1)
                                           body)))))

 (it "should omit empty groups"
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding-node n1 gen-const-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding-node n2 gen-valid-lambda-node))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding-node n3 gen-valid-lambda-node))
             (bindings (list b1 b2 b3))
             (body gen-simple-node)
             (parent (gen-with-bv (gen-letrec-node bindings body) (set v1 v2 v3))))
            (assert (waddell reconstruct-fix-node reconstruct-let-node parent bindings body)
                    (generated
                     (reconstruct-let-node parent
                                           (list b1)
                                           ;; NOTE No complex group at all.
                                           (generated
                                            (reconstruct-fix-node parent
                                                                  (list b2 b3)
                                                                  body))))))
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding-node n1 gen-const-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding-node n2 gen-non-value-node))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding-node n3 gen-non-value-node))
             (bindings (list b1 b2 b3))
             (body gen-simple-node)
             (parent (gen-with-bv (gen-letrec-node bindings body) (set v1 v2 v3))))
            (assert (waddell reconstruct-fix-node reconstruct-let-node parent bindings body)
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
             (b1 (gen-binding-node n1 gen-const-node))
             (v2 gen-valid-symbol)
             (n2 (gen-symbol-node v2))
             (b2 (gen-binding-node n2 gen-const-node))
             (v3 gen-valid-symbol)
             (n3 (gen-symbol-node v3))
             (b3 (gen-binding-node n3 gen-const-node))
             (bindings (list b1 b2 b3))
             (body gen-simple-node)
             (parent (gen-with-bv (gen-letrec-node bindings body) (set v1 v2 v3))))
            (assert (waddell reconstruct-fix-node reconstruct-let-node parent bindings body)
                    ;; NOTE Only simple value group is present.
                    (generated
                     (reconstruct-let-node parent
                                           (list b1 b2 b3)
                                           body))))))
