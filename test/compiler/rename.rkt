#lang racket

;; Renaming things for safety.

(require "../testing.rkt")
(require "../../src/compiler/utils/gensym.rkt")
(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/passes/rename.rkt")

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
     (check ((val gen-valid-symbol)
             (symbol (gen-symbol-node val)))
            (assert (mangle-names symbol)
                    (set-ast-symbol-value symbol (symbol->safe val))))
     (check ((formals (gen-arg-list (gen-integer 0 5)))
             (val gen-valid-symbol)
             (body (gen-symbol-node val))
             (fun (gen-lambda-node formals body)))
            (assert-ast (mangle-names fun)
                        (lambda renamed-formals
                          renamed-body)
                        (assert renamed-body
                                (set-ast-symbol-value body (symbol->safe val)))
                        (map (lambda (original renamed)
                               (assert renamed
                                       (set-ast-symbol-value original (symbol->safe (ast-symbol-value original)))))
                             formals
                             renamed-formals))))

 (it "doesn't rename quoted symbols"
     (check ((symbol gen-valid-symbol-node)
             (node (gen-specific-const-node symbol)))
            (assert-ast (mangle-names node)
                        (const renamed-symbol)
                        (assert renamed-symbol symbol))))

 (it "doesn't rename primop-app ops"
     (check ((op gen-valid-symbol)
             (args (gen-arg-list (gen-integer 0 5)))
             (node (apply gen-primop-app-node op args)))
            (assert-ast (mangle-names node)
                        (primop-app renamed-op renamed-args ...)
                        (assert renamed-op op)
                        (map (lambda (original renamed)
                               (set-ast-symbol-value original (symbol->safe (ast-symbol-value original))))
                             args
                             renamed-args))))

 (it "renames all wildcards"
     (check ((sym (gen-symbol-node '_))
             (list (gen-specific-do-node sym sym sym)))
            (gensym-reset!)
            (assert-ast (mangle-names list)
                        (do (symbol '__WILD1) (symbol '__WILD2) (symbol '__WILD3))
                        (assert #t)))))
