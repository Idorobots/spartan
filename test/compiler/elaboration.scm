;; Semantic elaboration.

(describe
 "elaboration"
 (it "elaborates valid ifs"
     (check ((sym (gen-symbol-node 'if))
             (condition gen-valid-symbol-node)
             (then gen-valid-symbol-node)
             (else gen-valid-symbol-node)
             (node (gen-specific-list-node sym condition then else)))
            (let ((result (elaborate-unquoted node)))
              (assert (get-location result)
                      (get-location node))
              (assert-ast result
                          (if ,condition1
                              ,then1
                              ,else1)
                          (assert condition1 condition)
                          (assert then1 then)
                          (assert else1 else)))))

 (it "disallows bad if syntax"
     (check ((sym (gen-symbol-node 'if))
             (exprs (gen-list (gen-one-of 0 1 2 4 5) gen-valid-symbol-node))
             (node (apply gen-specific-list-node sym exprs)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    "Bad `if` syntax, expected exactly three expressions - condition, then and else branches - to follow:")))

 (it "elaborates valid dos"
     (check ((sym (gen-symbol-node 'do))
             (exprs (gen-list (gen-integer 1 5) gen-valid-symbol-node))
             (node (apply gen-specific-list-node sym exprs)))
            (let ((result (elaborate-unquoted node)))
              (assert (get-location result)
                      (get-location node))
              (assert (get-context result)
                      "Bad `do` syntax")
              (assert-ast result
                          (body . ,exprs1)
                          (assert exprs1 exprs)))))

 (it "disallows bad do syntax"
     (check ((sym (gen-symbol-node 'do))
             (node (gen-specific-list-node sym)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    "Bad `do` syntax, expected at least one expression to follow:")))

 (it "elaborates valid lambdas"
     (check ((sym (gen-symbol-node 'lambda))
             (formals (gen-arg-list (gen-integer 0 3)))
             (fs (apply gen-specific-list-node formals))
             (body (gen-arg-list (gen-integer 1 3)))
             (node (apply gen-specific-list-node sym fs body)))
            (let ((result (elaborate-unquoted node)))
              (assert (get-location result)
                      (get-location node))
              (assert (get-location (ast-lambda-body result))
                      (get-location node))
              (assert (get-context (ast-lambda-body result))
                      "Bad `lambda` body syntax")
              (assert-ast result
                          (lambda ,formals1
                            (body . ,body1))
                          (assert formals1 formals)
                          (assert body1 body)))))

 (it "disallows bad lambda syntax"
     (check ((node (gen-specific-list-node (gen-symbol-node 'lambda))))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    "Bad `lambda` syntax, expected a formal arguments specification followed by a body:"))
     (check ((formals (gen-arg-list (gen-integer 0 5)))
             (args (gen-one-of gen-valid-symbol-node
                               (apply gen-specific-list-node formals)))
             (node (gen-specific-list-node (gen-symbol-node 'lambda) args)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    "Bad `lambda` syntax, expected a formal arguments specification followed by a body:"))
     (check ((contents (gen-list (gen-integer 1 5) (gen-number-node gen-number)))
             (args (apply gen-specific-list-node contents))
             (body gen-valid-symbol-node)
             (node (gen-specific-list-node (gen-symbol-node 'lambda) args body)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    "Bad `lambda` formal arguments syntax, expected a symbol but got a number instead:"))
     (check ((var gen-valid-symbol)
             (arg (gen-symbol-node var))
             (args (gen-specific-list-node arg arg))
             (node (gen-specific-list-node (gen-symbol-node 'lambda) args arg)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    (format "Bad `lambda` formal arguments syntax, duplicate formal argument `~a`:" var)))
     (check ((var (apply gen-one-of +reserved-keywords+))
             (arg (gen-symbol-node var))
             (args (gen-specific-list-node arg))
             (node (gen-specific-list-node (gen-symbol-node 'lambda) args arg)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    (format "Bad `lambda` formal arguments syntax, reserved keyword `~a` used as a formal argument:" var))))

 (it "elaborates valid let"
     (check ((sym (gen-symbol-node 'let))
             (vars (gen-arg-list (gen-integer 1 3)))
             (vals (gen-arg-list (length vars)))
             (bindings (lambda (rand)
                         (map (lambda (var val)
                                (sample (gen-specific-list-node var val)
                                        rand))
                              vars
                              vals)))
             (bs (apply gen-specific-list-node bindings))
             (body (gen-arg-list (gen-integer 1 3)))
             (node (apply gen-specific-list-node sym bs body)))
            (let ((result (elaborate-unquoted node)))
              (assert (get-location result)
                      (get-location node))
              (assert (get-location (ast-let-body result))
                      (get-location node))
              (assert (get-context (ast-let-body result))
                      "Bad `let` body syntax")
              (assert-ast result
                          (let ,bindings1
                            (body . ,body1))
                          (assert (map get-location bindings1)
                                  (map get-location bindings))
                          (assert (map ast-binding-var bindings1)
                                  vars)
                          (assert (map ast-binding-val bindings1)
                                  vals)
                          (assert body1 body)))))

 (it "disallows bad let syntax"
     (check ((node (gen-specific-list-node (gen-symbol-node 'let))))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    "Bad `let` syntax, expected a list of bindings followed by a body:"))
     (check ((identifier gen-valid-symbol-node)
             (b1 (gen-specific-list-node identifier gen-valid-symbol-node))
             (bindings (gen-one-of b1
                                   (gen-specific-list-node b1)
                                   gen-valid-symbol-node))
             (node (gen-specific-list-node (gen-symbol-node 'let) bindings)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    "Bad `let` syntax, expected a list of bindings followed by a body:"))
     (check ((identifier gen-valid-symbol-node)
             (bindings (gen-specific-list-node identifier gen-valid-symbol-node))
             (body gen-valid-symbol-node)
             (node (gen-specific-list-node (gen-symbol-node 'let) bindings body)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    "Bad `let` bindings syntax, expected a pair of an identifier and a value:"))
     (check ((identifier (gen-number-node gen-number))
             (b1 (gen-specific-list-node identifier gen-valid-symbol-node))
             (bindings (gen-specific-list-node b1))
             (body gen-valid-symbol-node)
             (node (gen-specific-list-node (gen-symbol-node 'let) bindings body)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    "Bad `let` bindings syntax, expected a symbol but got a number instead:"))
     (check ((var gen-valid-symbol)
             (identifier (gen-symbol-node var))
             (b1 (gen-specific-list-node identifier gen-valid-symbol-node))
             (b2 (gen-specific-list-node identifier gen-valid-symbol-node))
             (bindings (gen-specific-list-node b1 b2))
             (body gen-valid-symbol-node)
             (node (gen-specific-list-node (gen-symbol-node 'let) bindings body)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    (format "Bad `let` bindings syntax, duplicate binding identifier `~a`:" var)))
     (check ((var (apply gen-one-of +reserved-keywords+))
             (identifier (gen-symbol-node var))
             (b1 (gen-specific-list-node identifier gen-valid-symbol-node))
             (bindings (gen-specific-list-node b1))
             (body gen-valid-symbol-node)
             (node (gen-specific-list-node (gen-symbol-node 'let) bindings body)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    (format "Bad `let` bindings syntax, reserved keyword `~a` used as a binding identifier:" var))))

 (it "elaborates valid letrec"
     (check ((sym (gen-symbol-node 'letrec))
             (vars (gen-arg-list (gen-integer 1 3)))
             (vals (gen-arg-list (length vars)))
             (bindings (lambda (rand)
                         (map (lambda (var val)
                                (sample (gen-specific-list-node var val)
                                        rand))
                              vars
                              vals)))
             (bs (apply gen-specific-list-node bindings))
             (body (gen-arg-list (gen-integer 1 3)))
             (node (apply gen-specific-list-node sym bs body)))
            (let ((result (elaborate-unquoted node)))
              (assert (get-location result)
                      (get-location node))
              (assert (get-location (ast-letrec-body result))
                      (get-location node))
              (assert (get-context (ast-letrec-body result))
                      "Bad `letrec` body syntax")
              (assert-ast result
                          (letrec ,bindings1
                            (body . ,body1))
                          (assert (map get-location bindings1)
                                  (map get-location bindings))
                          (assert (map ast-binding-var bindings1)
                                  vars)
                          (assert (map ast-binding-val bindings1)
                                  vals)
                          (assert body1 body)))))

 (it "disallows bad letrec syntax"
     (check ((node (gen-specific-list-node (gen-symbol-node 'letrec))))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    "Bad `letrec` syntax, expected a list of bindings followed by a body:"))
     (check ((identifier gen-valid-symbol-node)
             (b1 (gen-specific-list-node identifier gen-valid-symbol-node))
             (bindings (gen-one-of b1
                                   (gen-specific-list-node b1)
                                   gen-valid-symbol-node))
             (node (gen-specific-list-node (gen-symbol-node 'letrec) bindings)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    "Bad `letrec` syntax, expected a list of bindings followed by a body:"))
     (check ((identifier gen-valid-symbol-node)
             (bindings (gen-specific-list-node identifier gen-valid-symbol-node))
             (body gen-valid-symbol-node)
             (node (gen-specific-list-node (gen-symbol-node 'letrec) bindings body)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    "Bad `letrec` bindings syntax, expected a pair of an identifier and a value:"))
     (check ((identifier (gen-number-node gen-number))
             (b1 (gen-specific-list-node identifier gen-valid-symbol-node))
             (bindings (gen-specific-list-node b1))
             (body gen-valid-symbol-node)
             (node (gen-specific-list-node (gen-symbol-node 'letrec) bindings body)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    "Bad `letrec` bindings syntax, expected a symbol but got a number instead:"))
     (check ((var gen-valid-symbol)
             (identifier (gen-symbol-node var))
             (b1 (gen-specific-list-node identifier gen-valid-symbol-node))
             (b2 (gen-specific-list-node identifier gen-valid-symbol-node))
             (bindings (gen-specific-list-node b1 b2))
             (body gen-valid-symbol-node)
             (node (gen-specific-list-node (gen-symbol-node 'letrec) bindings body)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    (format "Bad `letrec` bindings syntax, duplicate binding identifier `~a`:" var)))
     (check ((var (apply gen-one-of +reserved-keywords+))
             (identifier (gen-symbol-node var))
             (b1 (gen-specific-list-node identifier gen-valid-symbol-node))
             (bindings (gen-specific-list-node b1))
             (body gen-valid-symbol-node)
             (node (gen-specific-list-node (gen-symbol-node 'letrec) bindings body)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    (format "Bad `letrec` bindings syntax, reserved keyword `~a` used as a binding identifier:" var))))

 (it "elaborates valid quotes"
     (check ((sym (gen-one-of 'quote 'quasiquote 'unquote 'unquote-splicing))
             (op (gen-symbol-node sym))
             (val (gen-number-node gen-number))
             (node (gen-specific-list-node op val)))
            (let ((result (elaborate-unquoted node)))
              (assert (get-location result)
                      (get-location node))
              (assert (get-type result)
                      sym)
              (assert (ast-quoted-expr result)
                      val))))

 (it "disallows bad quote syntax"
     (check ((sym (gen-one-of 'quote 'quasiquote 'unquote 'unquote-splicing))
             (op (gen-symbol-node sym))
             (args (gen-list (gen-one-of 0 (gen-integer 2 5)) gen-valid-symbol))
             (node (apply gen-specific-list-node op args)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (elaborate-unquoted node))
                    (format "Bad `~a` syntax, expected exactly one expression to follow:" sym))))

 (it "handles valid defines"
     (assert (elaborate-unquoted (at (location 5 23)
                                     (make-list-node
                                      (list (make-symbol-node 'define)
                                            (make-symbol-node 'foo)
                                            (make-number-node 23)))))
             (at (location 5 23)
                 (make-def-node
                  (make-symbol-node 'foo)
                  (make-number-node 23))))
     (assert (elaborate-unquoted (at (location 5 23)
                                     (make-list-node
                                      (list (make-symbol-node 'define)
                                            (at (location 7 13)
                                                (make-list-node
                                                 (list (make-symbol-node 'foo))))
                                            (at (location 15 19)
                                                (make-number-node 23))))))
             (at (location 5 23)
                 (make-def-node
                  (make-symbol-node 'foo)
                  (at (location 5 23)
                      (generated
                       (make-lambda-node
                        '()
                        (at (location 5 23)
                            (make-body-node
                             (list (at (location 15 19)
                                       (make-number-node 23)))
                             "Bad `define` function body syntax"))))))))
     (assert (elaborate-unquoted (at (location 5 23)
                                     (make-list-node
                                      (list (make-symbol-node 'define)
                                            (at (location 7 13)
                                                (make-list-node
                                                 (list (make-symbol-node 'foo)
                                                       (make-symbol-node 'x))))
                                            (at (location 15 19)
                                                (make-symbol-node 'x))))))
             (at (location 5 23)
                 (make-def-node
                  (make-symbol-node 'foo)
                  (at (location 5 23)
                      (generated
                       (make-lambda-node
                        (list (make-symbol-node 'x))
                        (at (location 5 23)
                            (make-body-node
                             (list (at (location 15 19)
                                       (make-symbol-node 'x)))
                             "Bad `define` function body syntax"))))))))
     (assert (elaborate-unquoted (at (location 5 23)
                                     (make-list-node
                                      (list (make-symbol-node 'define)
                                            (at (location 7 13)
                                                (make-list-node
                                                 (list (make-symbol-node 'foo)
                                                       (make-symbol-node 'x))))
                                            (at (location 14 15)
                                                (make-symbol-node 'y))
                                            (at (location 17 18)
                                                (make-symbol-node 'x))))))
             (at (location 5 23)
                 (make-def-node
                  (make-symbol-node 'foo)
                  (at (location 5 23)
                      (generated
                       (make-lambda-node
                        (list (make-symbol-node 'x))
                        (at (location 5 23)
                            (make-body-node
                             (list (at (location 14 15)
                                       (make-symbol-node 'y))
                                   (at (location 17 18)
                                       (make-symbol-node 'x)))
                             "Bad `define` function body syntax")))))))))

 (it "disallows invalid defines"
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (elaborate-unquoted (at (location 5 23)
                                       (make-list-node
                                        (list
                                         (at (location 7 13)
                                             (make-symbol-node 'define)))))))
             "Bad `define` syntax, expected either an identifier and an expression or a function signature and a body to follow:")
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (elaborate-unquoted (at (location 5 23)
                                       (make-list-node
                                        (list (at (location 7 13)
                                                  (make-symbol-node 'define))
                                              (make-symbol-node 'foo))))))
             "Bad `define` syntax, expected either an identifier and an expression or a function signature and a body to follow:")
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (elaborate-unquoted (at (location 5 23)
                                       (make-list-node
                                        (list (at (location 7 13)
                                                  (make-symbol-node 'define))
                                              (at (location 9 10)
                                                  (make-number-node 23))
                                              (make-symbol-node 'foo))))))
             "Bad `define` syntax, expected a symbol but got a number instead:")
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (elaborate-unquoted (at (location 5 23)
                                       (make-list-node
                                        (list (at (location 7 13)
                                                  (make-symbol-node 'define))
                                              (make-list-node
                                               (list (make-symbol-node 'foo)
                                                     (make-number-node 23))))))))
             "Bad `define` syntax, expected either an identifier and an expression or a function signature and a body to follow:")
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (elaborate-unquoted (at (location 5 23)
                                       (make-list-node
                                        (list (at (location 5 7)
                                                  (make-symbol-node 'define))
                                              (at (location 7 13)
                                                  (make-list-node
                                                   (list (make-symbol-node 'foo)
                                                         (at (location 9 10)
                                                             (make-number-node 23)))))
                                              (make-number-node 23))))))
             "Bad `define` function signature syntax, expected a symbol but got a number instead:")
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (elaborate-unquoted (at (location 5 23)
                                       (make-list-node
                                        (list (at (location 5 7)
                                                  (make-symbol-node 'define))
                                              (at (location 7 13)
                                                  (make-list-node
                                                   (list (at (location 9 10)
                                                             (make-number-node 23))
                                                         (make-symbol-node 'foo))))
                                              (make-number-node 23))))))
             "Bad `define` syntax, expected a symbol but got a number instead:"))

 (it "elaborates valid applications"
     (assert (elaborate-unquoted (at (location 5 23)
                                     (make-list-node (list (make-symbol-node 'foo)))))
             (at (location 5 23)
                 (make-app-node (make-symbol-node 'foo) '())))
     (assert (elaborate-unquoted (at (location 5 23)
                                     (make-list-node
                                      (list (make-symbol-node 'foo)
                                            (make-symbol-node 'bar)))))
             (at (location 5 23)
                 (make-app-node (make-symbol-node 'foo)
                                (list (make-symbol-node 'bar)))))
     (assert (elaborate-unquoted (at (location 5 23)
                                     (make-list-node
                                      (list (at (location 7 13)
                                                (make-list-node
                                                 (list (make-symbol-node 'do)
                                                       (make-number-node 23)
                                                       (make-symbol-node 'foo))))
                                            (make-symbol-node 'bar)))))
             (at (location 5 23)
                 (make-app-node
                  (at (location 7 13)
                      (make-body-node
                       (list (make-number-node 23)
                             (make-symbol-node 'foo))
                       "Bad `do` syntax"))
                  (list (make-symbol-node 'bar)))))
     (assert (elaborate-unquoted (at (location 5 23)
                                     (make-list-node
                                      (list (at (location 7 13)
                                                (make-list-node
                                                 (list (make-symbol-node 'foo))))))))
             (at (location 5 23)
                 (make-app-node
                  (at (location 7 13)
                      (make-app-node (make-symbol-node 'foo) '()))
                  '()))))

 (it "doesn't allow bad applications"
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (elaborate-unquoted (at (location 5 23)
                                       (make-list-node '()))))
             "Bad call syntax, expected at least one expression within the call:")
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (elaborate-unquoted (at (location 5 23)
                                       (make-list-node
                                        (list (at (location 7 13)
                                                  (make-number-node 23)))))))
             "Bad call syntax, expected an expression that evaluates to a procedure but got a number instead:")
     (assert (with-handlers ((compilation-error?
                              compilation-error-what))
               (elaborate-unquoted (at (location 5 23)
                                       (make-list-node
                                        (list (at (location 7 13)
                                                  (make-quote-node
                                                   (make-number-node 23))))))))
             "Bad call syntax, expected an expression that evaluates to a procedure but got a quote instead:")))
