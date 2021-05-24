;; AST tests.

(describe
 "AST node"
 (it "allows setting and getting arbitrary properties"
     (assert (ast-get (ast-node 'value 23) 'value)
             23)
     (assert (ast-set (ast-node 'value 23) 'other-value 5)
             (ast-node 'value 23 'other-value 5)))

 (it "`at` can add location"
     (assert (at (location 5 23)
                 (ast-node 'value 'value))
             (ast-node 'value 'value 'location (location 5 23)))
     (assert (at (location 5 23)
                 (ast-node 'value 'value 'location (location 10 15)))
             (ast-node 'value 'value 'location (location 5 23))))

 (it "`generate` can mark node artificial"
     (assert (generated (ast-node 'value 'value))
             (ast-node 'value 'value 'generated #t))
     (assert (generated (ast-node 'value 'value 'generated #f))
             (ast-node 'value 'value 'generated #t)))

 (it "`at` preserves artificial state"
     (assert (at (location 5 23)
                 (generated (ast-node 'value 'value)))
             (ast-node 'value 'value 'location (location 5 23) 'generated #t)))

 (it "`replace` preserves location & generated state"
     (let ((node (at (location 5 23)
                     (generated (ast-node 'value 'value)))))
       (assert (replace node
                        (ast-node 'value 'another-value))
               (ast-node 'value 'another-value 'location (location 5 23) 'generated #t))
       (assert (replace node
                        (at (location 23 5)
                            (ast-node 'value 'another-value)))
               (ast-node 'value 'another-value 'location (location 5 23) 'generated #t))))

 (it "`location<?` correctly compares locations"
     (assert (location<? (location 0 0)
                         (location 0 0))
             #f)
     (assert (location<? (location 0 0)
                         (location 5 0)))
     (assert (location<? (location 23 0)
                         (location 5 0))
             #f)
     (assert (location<? (location 0 23)
                         (location 5 23))))

 (it "ast-node-free-vars allow setting free vars"
     (let ((node (at (location 5 23)
                     (ast-node 'type 'not-a-symbol 'value 'value)))
           (sym (at (location 5 23)
                    (make-ast-symbol 'foo))))
       (assert (ast-node-free-vars sym)
               (set 'foo))
       (assert (set-ast-node-free-vars (set 'foo 'bar) sym)
               sym)
       (assert (set-ast-node-free-vars (set) node)
               node)
       (assert (ast-node-free-vars
                (set-ast-node-free-vars (set 'foo 'bar) node))
               (set 'foo 'bar))
       (assert (ast-node-free-vars
                (set-ast-node-free-vars (set)
                                        (set-ast-node-free-vars (set 'foo 'bar) node)))
               (set))))

 (it "ast-node-bound-vars allow setting bound vars"
     (let ((node (at (location 5 23)
                     (ast-node 'type 'not-a-symbol 'value 'value))))
       (assert (set-ast-node-bound-vars (set) node)
               node)
       (assert (ast-node-bound-vars
                (set-ast-node-bound-vars (set 'foo 'bar) node))
               (set 'foo 'bar))
       (assert (ast-node-bound-vars
                (set-ast-node-bound-vars (set)
                                         (set-ast-node-bound-vars (set 'foo 'bar)
                                                                  node)))
               (set)))))

(describe
 "AST map"
 (it "maps various AST nodes"
     (define ast (make-ast-list (list (make-ast-symbol 'foo)
                                      (make-ast-number 23))))
     (assert (map-ast id ast)
             ast)
     (assert (map-ast (lambda (e)
                        (if (ast-number? e)
                            (set-ast-number-value e (* 2 (ast-number-value e)))
                            e))
                      ast)
             (make-ast-list (list (make-ast-symbol 'foo)
                                  (make-ast-number 46))))
     (assert (map-ast (lambda (e)
                        (if (ast-number? e)
                            (set-ast-number-value e (+ 2 (ast-number-value e)))
                            e))
                      (make-ast-if (make-ast-number 23)
                                   (make-ast-number 5)
                                   (make-ast-number 0)))
             (make-ast-if (make-ast-number 25)
                          (make-ast-number 7)
                          (make-ast-number 2)))
     (assert (map-ast (lambda (e)
                        (if (ast-number? e)
                            (set-ast-number-value e (+ 2 (ast-number-value e)))
                            e))
                      (make-ast-do
                       (list (make-ast-number 23)
                             (make-ast-number 5)
                             (make-ast-number 0))))
             (make-ast-do
              (list (make-ast-number 25)
                    (make-ast-number 7)
                    (make-ast-number 2)))))

 (it "preserves property values"
     (define ast (make-ast-list
                  (list (at (location 5 23)
                            (make-ast-symbol 'foo))
                        (generated (make-ast-number 23)))))
     (assert (map-ast id ast)
             ast)))

(describe
 "ast-matches?"
 (it "underscore matches anything"
     (assert (ast-matches? (make-ast-symbol 'foo) '_)
             (empty-bindings))
     (assert (ast-matches? (make-ast-number 23) '_)
             (empty-bindings))
     (assert (ast-matches? (make-ast-list (list)) '_)
             (empty-bindings)))

 (it "quoted symbols match only the same symbol nodes"
     (assert (ast-matches? (make-ast-symbol 'foo) ''foo)
             (empty-bindings))
     (assert (not (ast-matches? (make-ast-symbol 'bar) ''foo)))
     (assert (not (ast-matches? (make-ast-number 23) ''foo)))
     (assert (not (ast-matches? (make-ast-list (list)) ''foo))))

 (it "lists map the same length list nodes"
     (assert (ast-list-matches? '() '())
             (empty-bindings))
     (assert (ast-list-matches? (list (make-ast-symbol 'foo)
                                      (make-ast-symbol 'bar))
                                '(_ _))
             (empty-bindings))
     (assert (not (ast-list-matches? (list (make-ast-symbol 'foo)
                                           (make-ast-symbol 'bar))
                                     '(_))))
     (assert (not (ast-list-matches? (list (make-ast-symbol 'foo)
                                           (make-ast-symbol 'bar))
                                     '(_ _ _))))
     (assert (ast-matches? (make-ast-list
                            (list (make-ast-symbol 'foo)
                                  (make-ast-symbol 'bar)))
                           '(list _ _))
             (empty-bindings))
     (assert (ast-matches? (make-ast-list '())
                           '())
             (empty-bindings))
     (assert (not (ast-matches? (make-ast-list
                                 (list (make-ast-symbol 'foo)
                                       (make-ast-symbol 'bar)))
                                '(list _))))
     (assert (not (ast-matches? (make-ast-list
                                 (list (make-ast-symbol 'foo)
                                       (make-ast-symbol 'bar)))
                                '(list _ _ _))))
     (assert (not (ast-matches? (make-ast-symbol 'foo)
                                '(list _ _)))))

 (it "allows matching multiple subexpressions"
     (assert (ast-list-matches? (list (make-ast-symbol 'foo)
                                      (make-ast-symbol 'bar))
                                '(_ . _))
             (empty-bindings))
     (assert (ast-list-matches? (list (make-ast-symbol 'foo)
                                      (make-ast-symbol 'bar)
                                      (make-ast-symbol 'baz)
                                      (make-ast-symbol 'faz))
                                '(_ . _))
             (empty-bindings))
     (assert (ast-matches? (make-ast-list
                            (list (make-ast-symbol 'foo)
                                  (make-ast-symbol 'bar)))
                           '(list _ . _))
             (empty-bindings))
     (assert (ast-matches? (make-ast-list
                            (list (make-ast-symbol 'foo)
                                  (make-ast-symbol 'bar)
                                  (make-ast-symbol 'baz)
                                  (make-ast-symbol 'faz)))
                           '(list _ . _))
             (empty-bindings))
     (assert (ast-matches? (make-ast-list
                            (list (make-ast-symbol 'foo)))
                           '(list _ . _))
             (empty-bindings))
     (assert (not (ast-matches? (make-ast-list '())
                                '(list _ . _))))
     (assert (not (ast-matches? (make-ast-symbol 'foo)
                                '(list _ . _)))))

 (it "allows binding subpatterns as variables"
     (assert (ast-matches? (make-ast-symbol 'foo)
                           '(unquote foo))
             (bindings 'foo (make-ast-symbol 'foo)))
     (assert (ast-matches? (make-ast-list
                            (list (make-ast-number 1)
                                  (make-ast-number 2)
                                  (make-ast-number 3)))
                           '(list ,one ,two ,three))
             (make-bindings
              (list (cons 'one (make-ast-number 1))
                    (cons 'two (make-ast-number 2))
                    (cons 'three (make-ast-number 3)))))
     (assert (ast-matches? (make-ast-list
                            (list (make-ast-number 1)
                                  (make-ast-number 2)
                                  (make-ast-number 3)))
                           '(list ,one . ,rest))
             (make-bindings
              (list (cons 'one (make-ast-number 1))
                    (cons 'rest (list (make-ast-number 2)
                                      (make-ast-number 3))))))))

(describe
 "ast-eqv?"
 (it "should find the same trees equivalent"
     (check ((node gen-ast-node))
            (assert (ast-eqv? node node))))

 (it "should find same contents with different metadata equivalent"
     (check ((var gen-valid-symbol)
             (a (gen-symbol-node var))
             (b (gen-symbol-node var)))
            (assert (ast-eqv? a b))))

 (it "should not find different trees equivalent"
     (check ((a (gen-list (gen-integer 1 5) gen-ast-node))
             (b (gen-list (gen-integer 1 5) gen-ast-node)))
            (assert (not (ast-eqv? a b))))))

(describe
 "ast-case"
 (it "can match any expression"
     (check ((node gen-ast-node))
            (ast-case node
                      ((symbol ,value)
                       (assert value node))
                      ((number ,value)
                       (assert value node))
                      ((string ,value)
                       (assert value node))
                      ((const ,value)
                       (assert value (ast-const-value node)))
                      ((a-quote ,value)
                       (assert value (ast-quote-expr node)))
                      ((a-quasiquote ,value)
                       (assert value (ast-quasiquote-expr node)))
                      ((an-unquote ,value)
                       (assert value (ast-unquote-expr node)))
                      ((an-unquote-splicing ,value)
                       (assert value (ast-unquote-splicing-expr node)))
                      ((list . ,values)
                       (assert values (ast-list-values node)))
                      ((do . ,values)
                       (assert values (ast-do-exprs node)))
                      ((body . ,values)
                       (assert values (ast-body-exprs node)))
                      ((if ,cond ,then, else)
                       (assert cond (ast-if-condition node))
                       (assert then (ast-if-then node))
                       (assert else (ast-if-else node)))
                      ((app ,op . ,args)
                       (assert op (ast-app-op node))
                       (assert args (ast-app-args node)))
                      ((primop-app ,op . ,args)
                       (assert (ast-symbol-value op) (ast-app-op node))
                       (assert args (ast-app-args node)))
                      ((lambda ,formals ,body)
                       (assert formals (ast-lambda-formals node))
                       (assert body (ast-lambda-body node)))
                      ((binding ,var ,val)
                       (assert var (ast-binding-var node))
                       (assert val (ast-binding-val node)))
                      ((let ,bindings ,body)
                       (assert bindings (ast-let-bindings node))
                       (assert body (ast-let-body node)))
                      ((letrec ,bindings ,body)
                       (assert bindings (ast-letrec-bindings node))
                       (assert body (ast-letrec-body node)))
                      ((fix ,bindings ,body)
                       (assert bindings (ast-fix-bindings node))
                       (assert body (ast-fix-body node)))
                      ((def ,name ,value)
                       (assert name (ast-def-name node))
                       (assert value (ast-def-value node)))
                      ((<error> ,error)
                       (assert error (ast-error-expr node)))
                      ((<location>)
                       (assert #t))
                      (else (assert #f)))))

 (it "can match specific primops"
     (check ((cont (gen-symbol-node 'cont1))
             (node gen-ast-node)
             (app (gen-primop-app-node '&yield-cont cont node)))
            (ast-case app
                      ((primop-app '&yield-cont 'cont1 ,some-value)
                       (assert some-value node))
                      (else
                       (assert #f)))))

 (it "can match specific lambdas"
     (check ((val (gen-symbol-node 'value2))
             (node (gen-lambda-node (list val) val)))
            (ast-case node
                      ((lambda ('value2) 'value2)
                       (assert #t))
                      (else
                       (assert #f)))))

 (it "can match specific bindings on let"
     (check ((cont (gen-symbol-node 'cont1))
             (val gen-ast-node)
             (binding (gen-binding-node cont val))
             (bod gen-ast-node)
             (node (gen-let-node (list binding) bod)))
            (ast-case node
                      ((let ((binding 'cont1 ,value)) ,body)
                       (assert value val)
                       (assert body bod))
                      (else
                       (assert #f)))))

 (it "correctly unifies bindings"
     (check ((needle gen-valid-symbol-node)
             (node (gen-lambda-node (list needle) gen-valid-symbol-node)))
            (ast-case node
                      ((lambda (,arg) ,arg)
                       (assert #f))
                      (else
                       (assert #t))))
     (check ((var gen-valid-symbol)
             (needle (gen-symbol-node var))
             (body (gen-symbol-node var))
             (node (gen-lambda-node (list needle) body)))
            (ast-case node
                      ((lambda (,arg) ,arg)
                       (assert arg body))
                      (else
                       (assert #f))))))

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
            (unless (ast-symbol? (ast-error-expr error))
              (assert (safe-symbol-value error) '<error>)))))

(describe
 "recursive?"
 (it "should correctly assess recursivity of binding groups"
     (check ((var gen-valid-symbol)
             (node (gen-symbol-node var))
             (rec gen-complex-node)
             (rec-binding (gen-self-recursive (gen-binding-node node rec)))
             (non-rec-binding gen-valid-binding-node)
             (multiple-bindings (gen-binding-list (gen-integer 2 5))))
            (assert (not (recursive? '())))
            (assert (not (recursive? (list non-rec-binding))))
            (assert (recursive? (list rec-binding)))
            (assert (recursive? multiple-bindings)))))

(describe
 "ast-size"
 (it "should estimate AST size"
     (check ((node (gen-one-of (gen-number-node gen-number)
                               (gen-string-node (gen-integer 10 20))
                               gen-valid-symbol-node
                               gen-const-node)))
            (assert (ast-size node) 1))
     (check ((size (gen-integer 1 10))
             (node (gen-do-node size gen-const-node)))
            (assert (ast-size node) size))
     (check ((f gen-valid-lambda-node))
            (assert (ast-size f)
                    (ast-size (ast-lambda-body f)))))

 (it "computes AST size for any node"
     (check ((node gen-ast-node))
            (assert (>= (ast-size node) 0)))))
