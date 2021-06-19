#lang racket

;; Letrec conversion tests.

(require "../testing.rkt")
(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/propagate.rkt")
(require "../../src/compiler/passes/letrec-bindings.rkt")
(require "../../src/compiler/utils/set.rkt")

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
             (b2 (gen-binding-node n2 gen-const-node))
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
            (assert (reorder-bindings `((,v3) (,v2) (,v1)) parent bindings body)
                    (generated
                     (reconstruct-let-node
                      parent
                      (list b3)
                      (generated
                       (reconstruct-let-node
                        parent
                        (list b2)
                        (generated
                         (reconstruct-let-node
                          parent
                          (list b1)
                          body)))))))
            (assert (reorder-bindings `((,v3) (,v1 ,v2)) parent bindings body)
                    (generated
                     (reconstruct-let-node
                      parent
                      (list b3)
                      (generated
                       (reconstruct-letrec-node
                        parent
                        (list b1 b2)
                        body)))))
            (assert (reorder-bindings `((,v1 ,v2 ,v3)) parent bindings body)
                    (generated
                     (reconstruct-letrec-node
                      parent
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
             (rec gen-complex-node)
             (b3 (gen-self-recursive (gen-binding-node n3 rec)))
             (bindings (list b1 b2 b3))
             (body gen-simple-node)
             (parent (gen-with-bv (gen-letrec-node bindings body) (set v1 v2 v3))))
            (assert (reorder-bindings `((,v3) (,v2) (,v1)) parent bindings body)
                    (generated
                     (reconstruct-letrec-node
                      parent
                      (list b3)
                      (generated
                       (reconstruct-let-node
                        parent
                        (list b2)
                        (generated
                         (reconstruct-let-node parent
                                               (list b1)
                                               body))))))))))

(define gen-non-letrec-node
  (gen-one-of gen-valid-do-node
              gen-valid-if-node
              gen-valid-app-node
              gen-valid-primop-app-node
              gen-valid-let-node))

(describe
 "reorder-letrec"
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
             (complex2 (gen-with-fv gen-non-letrec-node (set sym1)))
             (lambda3 (gen-with-fv gen-valid-lambda-node (set sym6 sym4)))
             (lambda4 (gen-with-fv gen-valid-lambda-node (set sym1 sym3)))
             (complex5 (gen-with-fv gen-non-letrec-node (set sym2)))
             (complex6 (gen-with-fv gen-non-letrec-node (set sym5 sym3)))
             ;; NOTE Order doesn't really matter here.
             (b1 (gen-binding-node var1 value1))
             (b2 (gen-binding-node var2 complex2))
             (b3 (gen-binding-node var3 lambda3))
             (b4 (gen-binding-node var4 lambda4))
             (b5 (gen-binding-node var5 complex5))
             (b6 (gen-binding-node var6 complex6))
             (bindings (list b3 b1 b6 b4 b2 b5))
             (body-fv (gen-list (gen-integer 3 5) gen-valid-symbol))
             (body (gen-with-fv gen-non-letrec-node body-fv))
             (parent (gen-with-bv (gen-letrec-node bindings body)
                                  (set sym1 sym2 sym3 sym4 sym5 sym6))))
            (assert (reorder-letrec parent)
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
                          (generated
                           (reconstruct-letrec-node
                            parent
                            (list b3 b6 b4)
                            body)))))))))))

 (it "should preserve lambda self-recursivity"
     (check ((sym1 gen-valid-symbol)
             (var1 (gen-symbol-node sym1))
             (lambda1 (gen-with-fv gen-valid-lambda-node (set sym1)))
             (b1 (gen-self-recursive (gen-with-fv-bv (gen-binding-node var1 lambda1) (set sym1) (set sym1))))
             (bindings (list b1))
             (body gen-value-node)
             (parent (gen-with-bv (gen-letrec-node bindings body)
                                  (set sym1))))
            (assert (reorder-letrec parent)
                    (generated
                     (reconstruct-letrec-node parent (list b1) body))))))
