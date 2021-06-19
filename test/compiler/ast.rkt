#lang racket

;; AST tests.

(require "../testing.rkt")
(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/utils/utils.rkt")
(require "../../src/compiler/utils/set.rkt")

(describe
 "AST node"
 (it "`generate` can mark node artificial"
     (assert (generated? (generated (make-ast-number (location 5 23) 23)))))

 (it "`replace` preserves location & generated state"
     (let ((node (generated (make-ast-number (location 5 23) 23))))
       (assert (replace node
                        (make-ast-symbol (location 7 13) 5))
               (generated (make-ast-symbol (location 5 23) 5)))))

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
     (let ((node (make-ast-number (location 5 23) 23))
           (sym (make-ast-symbol (location 5 23) 'foo)))
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
     (let ((node (make-ast-number (location 5 23) 23)))
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
     (define l (location 5 23))
     (define ast (make-ast-list l (list (make-ast-symbol l 'foo)
                                        (make-ast-number l 23))))
     (assert (map-ast id ast)
             ast)
     (assert (map-ast (lambda (e)
                        (if (ast-number? e)
                            (set-ast-number-value e (* 2 (ast-number-value e)))
                            e))
                      ast)
             (make-ast-list l (list (make-ast-symbol l 'foo)
                                    (make-ast-number l 46))))
     (assert (map-ast (lambda (e)
                        (if (ast-number? e)
                            (set-ast-number-value e (+ 2 (ast-number-value e)))
                            e))
                      (make-ast-if l
                                   (make-ast-number l 23)
                                   (make-ast-number l 5)
                                   (make-ast-number l 0)))
             (make-ast-if l
                          (make-ast-number l 25)
                          (make-ast-number l 7)
                          (make-ast-number l 2)))
     (assert (map-ast (lambda (e)
                        (if (ast-number? e)
                            (set-ast-number-value e (+ 2 (ast-number-value e)))
                            e))
                      (make-ast-do l
                                   (list (make-ast-number l 23)
                                         (make-ast-number l 5)
                                         (make-ast-number l 0))))
             (make-ast-do l
                          (list (make-ast-number l 25)
                                (make-ast-number l 7)
                                (make-ast-number l 2)))))

 (it "preserves property values"
     (define ast (make-ast-list (location 5 23)
                                (list (make-ast-symbol (location 5 23) 'foo)
                                      (generated (make-ast-number (location 7 13) 23)))))
     (assert (map-ast id ast)
             ast)))

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
 "match-ast"
 (it "can match any expression"
     (check ((node gen-ast-node))
            (println node)
            (match-ast node
                      ((symbol value)
                       (assert value (ast-symbol-value node)))
                      ((number value)
                       (assert value (ast-number-value node)))
                      ((string value)
                       (assert value (ast-string-value node)))
                      ((const value)
                       (assert value (ast-const-value node)))
                      ((ast-quote value)
                       (assert value (ast-quote-expr node)))
                      ((ast-quasiquote value)
                       (assert value (ast-quasiquote-expr node)))
                      ((ast-unquote value)
                       (assert value (ast-unquote-expr node)))
                      ((ast-unquote-splicing value)
                       (assert value (ast-unquote-splicing-expr node)))
                      ((list values ...)
                       (assert values (ast-list-values node)))
                      ((do values ...)
                       (assert values (ast-do-exprs node)))
                      ((ast-body values ...)
                       (assert values (ast-body-exprs node)))
                      ((if cond then else)
                       (assert cond (ast-if-condition node))
                       (assert then (ast-if-then node))
                       (assert else (ast-if-else node)))
                      ((app op args ...)
                       (assert op (ast-app-op node))
                       (assert args (ast-app-args node)))
                      ((primop-app op args ...)
                       (assert op (ast-primop-app-op node))
                       (assert args (ast-primop-app-args node)))
                      ((lambda formals body)
                       (assert formals (ast-lambda-formals node))
                       (assert body (ast-lambda-body node)))
                      ((binding var val)
                       (assert var (ast-binding-var node))
                       (assert val (ast-binding-val node)))
                      ((let bindings body)
                       (assert bindings (ast-let-bindings node))
                       (assert body (ast-let-body node)))
                      ((letrec bindings body)
                       (assert bindings (ast-letrec-bindings node))
                       (assert body (ast-letrec-body node)))
                      ((fix bindings body)
                       (assert bindings (ast-fix-bindings node))
                       (assert body (ast-fix-body node)))
                      ((def name value)
                       (assert name (ast-def-name node))
                       (assert value (ast-def-value node)))
                      ((ast-error error)
                       (assert error (ast-error-expr node)))
                      ((ast-location _)
                       (assert #t))
                      (else (assert #f)))))

 (it "can match specific primops"
     (check ((cont (gen-symbol-node 'cont1))
             (node gen-ast-node)
             (app (gen-primop-app-node '&yield-cont cont node)))
            (match-ast app
                       ((primop-app '&yield-cont (symbol 'cont1) some-value)
                        (assert some-value node))
                       (else
                        (assert #f)))))

 (it "can match specific lambdas"
     (check ((val (gen-symbol-node 'value2))
             (node (gen-lambda-node (list val) val)))
            (match-ast node
                      ((lambda ((symbol 'value2)) (symbol 'value2))
                       (assert #t))
                      (else
                       (assert #f)))))

 (it "can match specific bindings on let"
     (check ((cont (gen-symbol-node 'cont1))
             (val gen-ast-node)
             (b (gen-binding-node cont val))
             (bod gen-ast-node)
             (node (gen-let-node (list b) bod)))
            (match-ast node
                      ((let ((binding (symbol 'cont1) value)) body)
                       (assert value val)
                       (assert body bod))
                      (else
                       (assert #f)))))

 (it "correctly unifies bindings"
     (check ((needle gen-valid-symbol-node)
             (node (gen-lambda-node (list needle) gen-valid-symbol-node)))
            (match-ast node
                       ((lambda (arg1) arg2)
                        #:when (ast-eqv? arg1 arg2)
                        (assert #f))
                       (else
                        (assert #t))))
     (check ((var gen-valid-symbol)
             (needle (gen-symbol-node var))
             (body (gen-symbol-node var))
             (node (gen-lambda-node (list needle) body)))
            (match-ast node
                       ((lambda (arg1) arg2)
                        #:when (ast-eqv? arg1 arg2)
                        (assert arg2 body))
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
