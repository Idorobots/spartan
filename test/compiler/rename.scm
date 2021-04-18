;; Renaming things for safety.

(describe
 "symbol->safe"
 (it "correctly escapes symbols"
     (assert (symbol->safe 'foo) '__foo)
     (assert (symbol->safe 'foo23) '__foo23)
     (assert (symbol->safe 'foo!) '__fooBANG)
     (assert (symbol->safe 'symbol->safe) '__symbol_GREATERsafe)))

(describe
 "mangle-names"
 (it "correctly renames simple cases"
     (check ((symbol gen-valid-symbol-node))
            (assert (mangle-names symbol)
                    (ast-update symbol 'value symbol->safe)))
     (check ((formals (gen-arg-list (gen-integer 0 5)))
             (body gen-valid-symbol-node)
             (fun (gen-lambda-node formals body)))
            (assert-ast (mangle-names fun)
                        (lambda ,renamed-formals
                          ,renamed-body)
                        (assert renamed-body (ast-update body 'value symbol->safe))
                        (map (lambda (original renamed)
                               (assert renamed (ast-update original 'value symbol->safe)))
                             formals
                             renamed-formals))))

 (it "doesn't rename quoted symbols"
     (check ((symbol gen-valid-symbol-node)
             (node (gen-specific-const-node symbol)))
            (assert-ast (mangle-names node)
                        (const ,renamed-symbol)
                        (assert renamed-symbol symbol))))

 (it "doesn't rename primop-app ops"
     (check ((op gen-valid-symbol)
             (args (gen-arg-list (gen-integer 0 5)))
             (node (apply gen-primop-app-node op args)))
            (assert-ast (mangle-names node)
                        (primop-app ,renamed-op . ,renamed-args)
                        (assert (ast-symbol-value renamed-op) op)
                        (map (lambda (original renamed)
                               (assert renamed (ast-update original 'value symbol->safe)))
                             args
                             renamed-args))))

 (it "renames all wildcards"
     (check ((symbol (gen-symbol-node '_))
             (list (gen-specific-do-node symbol symbol symbol)))
            (gensym-reset!)
            (assert-ast (mangle-names list)
                        (do 'WILD1 'WILD2 'WILD3)
                        (assert #t)))))
