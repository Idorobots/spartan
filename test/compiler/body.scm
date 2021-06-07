;; Implicit body expansion tests.

(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/passes/body.rkt")

(define gen-body-neutral-node (gen-one-of gen-value-node gen-non-value-node))

(describe
 "body expansion"
 (it "should only operate on `body` nodes"
     (check ((node gen-body-neutral-node))
            (assert (expand-body node)
                    node)))

 (it "should expand well-formed body correctly"
     (check ((ctx (gen-text (gen-integer 10 20)))
             (expr gen-body-neutral-node)
             (node (gen-specific-body-node ctx expr)))
            (assert (expand-body node)
                    expr))
     (check ((ctx (gen-text (gen-integer 10 20)))
             (exprs (gen-list (gen-integer 2 5) gen-body-neutral-node))
             (node (apply gen-specific-body-node ctx exprs)))
            (let ((result (expand-body node)))
              (assert-ast result
                          (do expanded-exprs ...)
                          (assert expanded-exprs exprs))
              (assert (ast-node-location result)
                      (ast-node-location node))))
     (check ((ctx (gen-text (gen-integer 10 20)))
             (defs (gen-list (gen-integer 1 5) gen-valid-def-node))
             (non-defs (gen-list (gen-integer 2 5) gen-body-neutral-node))
             (node (apply gen-specific-body-node ctx (append defs non-defs))))
            (let ((result (expand-body node)))
              (assert-ast result
                          (letrec bindings
                            (do body ...))
                          (assert (length bindings)
                                  (length defs))
                          (map (lambda (b d)
                                 (assert (ast-binding-var b)
                                         (ast-def-name d))
                                 (assert (ast-binding-val b)
                                         (ast-def-value d)))
                               bindings
                               defs)
                          (assert body non-defs))
              (assert (ast-node-location result)
                      (ast-node-location node))
              (assert (ast-node-location (ast-letrec-body result))
                      (ast-node-location node)))))

 (it "should disallow not well-formed bodies"
     (check ((exprs (gen-list (gen-integer 0 5) gen-valid-def-node))
             (ctx (gen-text (gen-integer 10 20)))
             (node (apply gen-specific-body-node ctx exprs)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (expand-body node))
                    (format "~a, expected at least one non-definition expression within:" ctx))))

 (it "should disallow stray defs"
     (check ((ctx (gen-text (gen-integer 10 20)))
             (node (gen-with-ctx gen-valid-def-node ctx)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (expand-body node))
                    (format "~a, not allowed in this context:" ctx))))

 (it "should preserve error context"
     (check ((ctx (gen-text (gen-integer 10 20)))
             (exprs (gen-list (gen-integer 2 5) gen-body-neutral-node))
             (node (apply gen-specific-body-node ctx exprs)))
            (assert (ast-node-context (expand-body node)) ctx))))
