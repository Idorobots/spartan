#lang racket

;; Binding annotation tests

(require "../testing.rkt")
(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/passes/bindings.rkt")
(require "../../src/compiler/utils/set.rkt")

(describe
 "compute-complexity"
 (it "should recognize simple values"
     (check ((node gen-const-node))
            (assert (compute-complexity node)
                    'simple)))
 (it "should recognize lambdas"
     (check ((node gen-valid-lambda-node))
            (assert (compute-complexity node)
                    'lambda)))
 (it "should recognize complex expressions"
     (check ((node gen-non-value-node))
            (assert (compute-complexity node)
                    'complex))))

(describe
 "analyze-bindings"
 (it "should annotate let bindings with complexity & recursivity"
     (check ((sym1 gen-valid-symbol)
             (var1 (gen-symbol-node sym1))
             (sym2 gen-valid-symbol)
             (var2 (gen-symbol-node sym2))
             (complex1 gen-non-value-node)
             (lambda2 gen-valid-lambda-node)
             ;; (complex1 lambda2)
             (b1 (gen-with-fv-bv (gen-binding-node var1 complex1) (set sym1 sym2) (set sym1)))
             (b2 (gen-with-fv-bv (gen-binding-node var2 lambda2) (set sym2) (set sym2)))
             (bindings (list b1 b2))
             (parent (gen-let-node bindings gen-simple-node)))
            (let* ((result (analyze-bindings #f parent))
                   (bindings (ast-let-bindings result)))
              (assert (map ast-binding-complexity bindings)
                      '(complex lambda))
              (assert (map ast-binding-self-recursive bindings)
                      '(#f #f)))))

 (it "should annotate letrec bindings with complexity & recursivity"
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
             (value1 gen-const-node)
             (complex2 gen-non-value-node)
             (lambda3 gen-valid-lambda-node)
             (lambda4 gen-valid-lambda-node)
             (complex5 gen-non-value-node)
             (complex6 gen-non-value-node)
             ;; value1 <- (complex2) <- complex5 <- (complex6 lambda3 lambda4)
             (b1 (gen-with-bv (gen-binding-node var1 value1) (set sym1)))
             (b2 (gen-with-fv-bv (gen-binding-node var2 complex2) (set sym1 sym2) (set sym2)))
             (b3 (gen-with-fv-bv (gen-binding-node var3 lambda3) (set sym6 sym4) (set sym3)))
             (b4 (gen-with-fv-bv (gen-binding-node var4 lambda4) (set sym1 sym3 sym4) (set sym4)))
             (b5 (gen-with-fv-bv (gen-binding-node var5 complex5) (set sym2) (set sym5)))
             (b6 (gen-with-fv-bv (gen-binding-node var6 complex6) (set sym5 sym3) (set sym6)))
             (bindings (list b1 b2 b3 b4 b5 b6))
             (parent (gen-letrec-node bindings gen-simple-node)))
            (let* ((result (analyze-bindings #f parent))
                   (bindings (ast-letrec-bindings result)))
              (assert (map ast-binding-complexity bindings)
                      '(simple complex lambda lambda complex complex))
              (assert (map ast-binding-self-recursive bindings)
                      ;; NOTE The last complex value is not considered self-recursive. At least not directly.
                      '(#f #t #f #t #f #f))))))
