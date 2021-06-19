#lang racket

;; Builtin inliner tests

(require "../testing.rkt")
(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/passes/builtins.rkt")
(require "../../src/compiler/utils/set.rkt")

(describe
 "builtin inliner"
 (it "should inline specific builtins"
     (check ((op (gen-symbol-node '*))
             (args (gen-arg-list (gen-integer 0 3)))
             (node (apply gen-app-node op args)))
            (assert-ast (inline-app-ops (set '*) node)
                        (primop-app '* inlined-args ...)
                        (assert inlined-args args))))

 (it "should not inline other builtins"
     (check ((op (gen-symbol-node '+))
             (args (gen-arg-list (gen-integer 0 3)))
             (node (apply gen-app-node op args)))
            (assert (inline-app-ops (set '*) node)
                    node)))

 (it "should not inline complex applications"
     (check ((op1 (gen-symbol-node '+))
             (op2 (gen-symbol-node '*))
             (op (gen-if-node gen-simple-node op1 op2))
             (args (gen-arg-list (gen-integer 0 3)))
             (node (apply gen-app-node op args)))
            (assert (inline-app-ops (set '* '+) node)
                    node)))

 (it "should not inline overridden builtins"
     (check ((op (gen-symbol-node '*))
             (args (gen-arg-list (gen-integer 0 3)))
             (app (apply gen-app-node op args))
             (val gen-valid-lambda-node)
             (binding (gen-binding-node op val))
             (node (gen-with-bv (gen-let-node (list binding)
                                              app)
                                (set '*))))
            (assert (inline-app-ops (set '*) node)
                    node)))

 (it "should adjust the free variables"
     (check ((op (gen-symbol-node '*))
             (args (gen-arg-list (gen-integer 0 3)))
             (node (apply gen-app-node op args)))
            (assert (ast-node-free-vars (inline-app-ops (set '*) node))
                    (apply set (map ast-symbol-value args))))))
