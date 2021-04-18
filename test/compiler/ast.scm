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
                         (location 5 23)))))

(describe
 "AST map"
 (it "maps various AST nodes"
     (define ast (make-list-node (list (make-symbol-node 'foo)
                                       (make-number-node 23))))
     (assert (map-ast id id ast)
             ast)
     (assert (map-ast (lambda (_)
                        (make-number-node 23))
                      id
                      ast)
             (make-number-node 23))
     (assert (map-ast id
                      (lambda (e)
                        (if (equal? 'number (ast-get e 'type))
                            (ast-update e 'value (lambda (x) (* 2 x)))
                            e))
                      ast)
             (make-list-node (list (make-symbol-node 'foo)
                                   (make-number-node 46))))
     (assert (map-ast id
                      (lambda (e)
                        (if (equal? 'number (ast-get e 'type))
                            (ast-update e 'value (lambda (x) (+ 2 x)))
                            e))
                      (make-if-node (make-number-node 23)
                                    (make-number-node 5)
                                    (make-number-node 0)))
             (make-if-node (make-number-node 25)
                           (make-number-node 7)
                           (make-number-node 2)))
     (assert (map-ast id
                      (lambda (e)
                        (if (equal? 'number (ast-get e 'type))
                            (ast-update e 'value (lambda (x) (+ 2 x)))
                            e))
                      (make-do-node
                       (list (make-number-node 23)
                             (make-number-node 5)
                             (make-number-node 0))))
             (make-do-node
              (list (make-number-node 25)
                    (make-number-node 7)
                    (make-number-node 2)))))

 (it "preserves property values"
     (define ast (make-list-node
                  (list (at (location 5 23)
                            (make-symbol-node 'foo))
                        (generated (make-number-node 23)))))
     (assert (map-ast id id ast)
             ast)))

(describe
 "ast-matches?"
 (it "underscore matches anything"
     (assert (ast-matches? (make-symbol-node 'foo) '_)
             (empty-bindings))
     (assert (ast-matches? (make-number-node 23) '_)
             (empty-bindings))
     (assert (ast-matches? (make-list-node (list)) '_)
             (empty-bindings)))

 (it "quoted symbols match only the same symbol nodes"
     (assert (ast-matches? (make-symbol-node 'foo) ''foo)
             (empty-bindings))
     (assert (not (ast-matches? (make-symbol-node 'bar) ''foo)))
     (assert (not (ast-matches? (make-number-node 23) ''foo)))
     (assert (not (ast-matches? (make-list-node (list)) ''foo))))

 (it "lists map the same length list nodes"
     (assert (ast-list-matches? '() '())
             (empty-bindings))
     (assert (ast-list-matches? (list (make-symbol-node 'foo)
                                      (make-symbol-node 'bar))
                                '(_ _))
             (empty-bindings))
     (assert (not (ast-list-matches? (list (make-symbol-node 'foo)
                                           (make-symbol-node 'bar))
                                     '(_))))
     (assert (not (ast-list-matches? (list (make-symbol-node 'foo)
                                           (make-symbol-node 'bar))
                                     '(_ _ _))))
     (assert (ast-matches? (make-list-node
                            (list (make-symbol-node 'foo)
                                  (make-symbol-node 'bar)))
                           '(list _ _))
             (empty-bindings))
     (assert (ast-matches? (make-list-node '())
                           '())
             (empty-bindings))
     (assert (not (ast-matches? (make-list-node
                                 (list (make-symbol-node 'foo)
                                       (make-symbol-node 'bar)))
                                '(list _))))
     (assert (not (ast-matches? (make-list-node
                                 (list (make-symbol-node 'foo)
                                       (make-symbol-node 'bar)))
                                '(list _ _ _))))
     (assert (not (ast-matches? (make-symbol-node 'foo)
                                '(list _ _)))))

 (it "allows matching multiple subexpressions"
     (assert (ast-list-matches? (list (make-symbol-node 'foo)
                                      (make-symbol-node 'bar))
                                '(_ . _))
             (empty-bindings))
     (assert (ast-list-matches? (list (make-symbol-node 'foo)
                                      (make-symbol-node 'bar)
                                      (make-symbol-node 'baz)
                                      (make-symbol-node 'faz))
                                '(_ . _))
             (empty-bindings))
     (assert (ast-matches? (make-list-node
                            (list (make-symbol-node 'foo)
                                  (make-symbol-node 'bar)))
                           '(list _ . _))
             (empty-bindings))
     (assert (ast-matches? (make-list-node
                            (list (make-symbol-node 'foo)
                                  (make-symbol-node 'bar)
                                  (make-symbol-node 'baz)
                                  (make-symbol-node 'faz)))
                           '(list _ . _))
             (empty-bindings))
     (assert (ast-matches? (make-list-node
                            (list (make-symbol-node 'foo)))
                           '(list _ . _))
             (empty-bindings))
     (assert (not (ast-matches? (make-list-node '())
                                '(list _ . _))))
     (assert (not (ast-matches? (make-symbol-node 'foo)
                                '(list _ . _)))))

 (it "allows binding subpatterns as variables"
     (assert (ast-matches? (make-symbol-node 'foo)
                           '(unquote foo))
             (bindings 'foo (make-symbol-node 'foo)))
     (assert (ast-matches? (make-list-node
                            (list (make-number-node 1)
                                  (make-number-node 2)
                                  (make-number-node 3)))
                           '(list ,one ,two ,three))
             (bindings 'one (make-number-node 1)
                       'two (make-number-node 2)
                       'three (make-number-node 3)))
     (assert (ast-matches? (make-list-node
                            (list (make-number-node 1)
                                  (make-number-node 2)
                                  (make-number-node 3)))
                           '(list ,one . ,rest))
             (bindings 'one (make-number-node 1)
                       'rest (list (make-number-node 2)
                                   (make-number-node 3))))))

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
                       (assert value (ast-quoted-expr node)))
                      ((a-quasiquote ,value)
                       (assert value (ast-quoted-expr node)))
                      ((an-unquote ,value)
                       (assert value (ast-quoted-expr node)))
                      ((an-unquote-splicing ,value)
                       (assert value (ast-quoted-expr node)))
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
            (assert-ast app
                        (primop-app '&yield-cont 'cont1 ,some-value)
                        (assert some-value node))))

 (it "can match specific lambdas"
     (check ((val (gen-symbol-node 'value2))
             (node (gen-lambda-node (list val) val)))
            (assert-ast node
                        (lambda ('value2) 'value2)
                        (assert #t))))

 (it "can match specific bindings on let"
     (check ((cont (gen-symbol-node 'cont1))
             (val gen-ast-node)
             (binding (gen-binding-node cont val))
             (bod gen-ast-node)
             (node (gen-let-node (list binding) bod)))
            (assert-ast node
                        (let ((binding 'cont1 ,value)) ,body)
                        (assert value val)
                        (assert body bod)))))

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
 "recoursive?"
 (it "should correctly assess recoursivity of binding groups"
     (check ((var gen-valid-symbol)
             (node (gen-symbol-node var))
             (rec gen-complex-node)
             (rec-binding (gen-self-recoursive (gen-binding-node node rec)))
             (non-rec-binding gen-valid-binding-node)
             (multiple-bindings (gen-binding-list (gen-integer 2 5))))
            (assert (not (recoursive? '())))
            (assert (not (recoursive? (list non-rec-binding))))
            (assert (recoursive? (list rec-binding)))
            (assert (recoursive? multiple-bindings)))))
