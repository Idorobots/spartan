#lang racket

;; Validation tests

(require "../testing.rkt")
(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/errors.rkt")
(require "../../src/compiler/passes/validate.rkt")
(require "../../src/compiler/utils/set.rkt")

(describe
 "validation"
 (it "should disallow undefined variables"
     (check ((symbols (gen-arg-list (gen-integer 1 5)))
             (undefined (apply set (map ast-symbol-value symbols)))
             (node (apply gen-specific-do-node symbols)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (validate-ast (set) undefined (set) (set) node))
                    (format "Undefined variable `~a`:" (ast-symbol-value (car symbols))))))

 (it "should propose only suitable replacements for undefined variables"
     (check ((symbols (gen-arg-list (gen-integer 1 5)))
             (first-symbol (ast-symbol-value (car symbols)))
             (undefined (apply set (map ast-symbol-value symbols)))
             (known-short (string->symbol (string-append (symbol->string first-symbol)
                                                   "*")))
             (known-long (string->symbol (string-append (symbol->string first-symbol)
                                                   "*23456")))
             (node (apply gen-specific-do-node symbols)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (validate-ast (set known-short) undefined (set) (set) node))
                    (format "Undefined variable `~a`, did you mean `~a`:" first-symbol known-short))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (validate-ast (set known-long) undefined (set) (set) node))
                    (format "Undefined variable `~a`:" first-symbol))))

 (it "should not report actual defined variables"
     (check ((args1 (gen-arg-list (gen-integer 1 3)))
             (args2 (gen-arg-list (gen-integer 1 3)))
             (fv1 (apply set (map ast-symbol-value args1)))
             (fv2 (apply set (map ast-symbol-value args2)))
             (undefined (set-union fv1 fv2))
             (body (gen-with-fv (apply gen-specific-do-node (append args1 args2))
                                undefined))
             (node (gen-with-fv-bv (gen-lambda-node args1 body)
                                   fv2
                                   fv1)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (validate-ast (set) undefined (set) (set) node))
                    (format "Undefined variable `~a`:" (ast-symbol-value (car args2))))))

 (it "should report unused variables"
     (check ((args (gen-arg-list (gen-integer 1 5)))
             (bound (apply set (map ast-symbol-value args)))
             (body (apply gen-specific-do-node args))
             (node (gen-with-bv (gen-lambda-node args body)
                                bound)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (validate-ast (set) (set) (set) (set) node))
                    (format "Unused variable `~a`, rename to `_` to avoid this error:" (ast-symbol-value (car args))))))

 (it "should not report unused `_`"
     (check ((args (gen-list (gen-integer 1 5) (gen-symbol-node '_)))
             (bound (apply set (map ast-symbol-value args)))
             (body (gen-number-node gen-number))
             (node (gen-with-bv (gen-lambda-node args body)
                                bound)))
            (assert (validate-ast (set) (set) (set) (set) node)
                    node)))

 (it "should report variables used before definition"
     (check ((symbols (gen-arg-list (gen-integer 1 5)))
             (used-before-def (apply set (map ast-symbol-value symbols)))
             (node (apply gen-specific-do-node symbols)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (validate-ast (set) (set) (set) used-before-def node))
                    (format "Variable `~a` used before its definition:" (ast-symbol-value (car symbols))))))

 (it "should not report lazy variables used before definition"
     (check ((symbols (gen-arg-list (gen-integer 1 5)))
             (used-before-def (apply set (map ast-symbol-value symbols)))
             (args (gen-arg-list (gen-integer 1 5)))
             (bound (apply set (map ast-symbol-value args)))
             (body (gen-with-fv (gen-number-node gen-number)
                                used-before-def))
             (node (gen-lambda-node args body)))
            (assert (validate-ast (set) (set) (set) used-before-def node)
                    node)))

 (it "should report invalid application operations"
     (check ((op (gen-one-of (gen-number-node gen-number)
                             (gen-string-node (gen-text (gen-integer 10 20)))
                             (gen-specific-const-node gen-valid-symbol-node)))
             (args (gen-list (gen-integer 1 3)
                                 (gen-one-of (gen-number-node gen-number)
                                             (gen-quote-node gen-simple-node))))
             (node (apply gen-app-node op args)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (validate-ast (set) (set) (set) (set) node))
                    (format "Bad call syntax, expected an expression that evaluates to a procedure but got a ~a instead:"
                            (extract-node-type op)))))

  (it "should disallow stray defs"
     (check ((ctx (gen-text (gen-integer 10 20)))
             (node (gen-with-ctx gen-valid-def-node ctx)))
            (assert (with-handlers ((compilation-error?
                                     compilation-error-what))
                      (validate-ast (set) (set) (set) (set) node))
                    (format "~a, not allowed in this context:" ctx)))))
