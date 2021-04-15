;; Assignment conversion tests.


(define (check-deref arg node result)
  (assert-ast result (primop-app 'deref ,inserted-node)
              (assert inserted-node node))
  (assert (generated? result))
  (assert (get-free-vars result) (set arg))
  (assert (get-location result) (get-location node)))

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

(define (gen-ref value)
  (let ((l (get-location value)))
    (at l
        (generated
         (make-primop-app-node
          'ref
          (list (at l
                    (generated
                     (make-quote-node
                      (at l
                          (generated
                           (make-list-node '()))))))))))))

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
                                                     (complexity 'simple
                                                                 (make-binding-node var (gen-ref val)))))
                                           (free-vars
                                            (set-sum (list (apply set val-fv)
                                                           (apply set body-fv)
                                                           (set v)))
                                            (at (get-location body)
                                                (generated
                                                 (make-do-node
                                                  (list (free-vars
                                                         (set-union (apply set val-fv)
                                                                    (set v))
                                                         (let ((l (get-location val)))
                                                           (at l
                                                               (generated
                                                                (make-primop-app-node
                                                                 'assign!
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
                                                     (complexity 'simple
                                                                 (make-binding-node var (gen-ref val)))))
                                           (free-vars
                                            (set-union (apply set val-fv)
                                                       (set v))
                                            (at (get-location var)
                                                (generated
                                                 (make-do-node
                                                  (list (free-vars
                                                         (set-union (apply set val-fv)
                                                                    (set v))
                                                         (let ((l (get-location val)))
                                                           (at l
                                                               (generated
                                                                (make-primop-app-node
                                                                 'assign!
                                                                 (list var
                                                                       val))))))
                                                        (derefy (set v)
                                                                var))))))))))))

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

 (it "should fix recoursive lambdas"
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-self-recoursive (gen-binding-node n1 gen-valid-lambda-node)))
             (bindings (list b1))
             (body gen-simple-node)
             (parent (gen-with-bv (gen-letrec-node bindings body) (set v1))))
            (assert (waddell fix reconstruct-let-node parent bindings body)
                    (generated
                     ;; NOTE Recoursive lambda is fixed.
                     (fix parent
                          (list b1)
                          body)))))

 (it "should omit needless fix"
     (check ((v1 gen-valid-symbol)
             (n1 (gen-symbol-node v1))
             (b1 (gen-binding-node n1 gen-valid-lambda-node))
             (bindings (list b1))
             (body gen-simple-node)
             (parent (gen-with-bv (gen-letrec-node bindings body) (set v1))))
            (assert (waddell fix reconstruct-let-node parent bindings body)
                    (generated
                     ;; NOTE Non-recursive lambda is not fixed.
                     (reconstruct-let-node parent
                                           (list b1)
                                           body)))))

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
