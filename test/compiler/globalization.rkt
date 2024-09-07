#lang racket

;; Globalization tests.

(require "../testing.rkt")
(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/utils/set.rkt")
(require "../../src/compiler/utils/gensym.rkt")
(require "../../src/compiler/utils/utils.rkt")
(require "../../src/compiler/passes/globalization.rkt")

(describe
 "globalization"

 (it "should hoist complex constants"
     (define gen-simple
       (gen-one-of (gen-specific-const-node (gen-number-node gen-number))
                   (gen-specific-const-node (gen-list-node 0))))
     (check ((simple gen-simple)
             (values (gen-list (gen-integer 1 10) gen-simple))
             (complex (gen-one-of (gen-specific-const-node (gen-string-node (gen-text (gen-integer 0 20))))
                                  (gen-specific-const-node (apply gen-specific-list-node values))))
             (node (gen-specific-do-node simple complex)))
            (assert (hoist-values simple)
                    (list '() simple))
            (gensym-reset!)
            (let* ((result (hoist-values complex))
                   (hoisted (car result))
                   (init (cadr result)))
              (assert-ast init
                          (symbol name)
                          (assert set-member? (set 'list1 'string1) name)
                          (assert (length hoisted) 1)
                          (assert (cdr (assoc name hoisted)) complex)))
            (gensym-reset!)
            (let* ((result (hoist-values node))
                   (hoisted (car result))
                   (init (cadr result)))
              (assert-ast init
                          (do simple1 (symbol name))
                          (assert simple1 simple)
                          (assert (set-member? (set 'list1 'string1) name))
                          (assert (length hoisted) 1)
                          (assert (cdr (assoc name hoisted)) complex)))))

 (it "should re-use symbol names"
     (check ((sym gen-valid-symbol)
             (node (gen-specific-const-node (gen-symbol-node sym))))
            (gensym-reset!)
            (let* ((result (hoist-values node))
                   (hoisted (car result))
                   (init (cadr result)))
              (assert-ast init
                          (symbol name)
                          (assert name (string->symbol (string-append (symbol->string sym) "1")))
                          (assert (cdr (assoc name hoisted)) node)))))

 (it "should hoist lambdas"
     (check ((fun gen-valid-lambda-node)
             (value1 (gen-specific-const-node (gen-number-node gen-number)))
             (value2 (gen-specific-const-node (gen-number-node gen-number)))
             (node (gen-specific-do-node value1 fun value2)))
            (gensym-reset!)
            (let* ((result (hoist-values node))
                   (hoisted (car result))
                   (init (cadr result)))
              (assert-ast init
                          (do val1
                              (symbol 'function1)
                              val2)
                          (assert val1 value1)
                          (assert val2 value2)
                          (assert (length hoisted) 1)
                          (assert (cdr (assoc 'function1 hoisted)) fun)))))

 (it "should hoist non-capturing closures"
     (check ((fun gen-valid-lambda-node)
             (empty-env (gen-specific-const-node (gen-list-node 0)))
             (empty (gen-primop-app-node '&make-closure empty-env fun))
             (capturing-env (gen-primop-app-node 'cons gen-valid-symbol-node gen-valid-symbol-node))
             (capturing (gen-primop-app-node '&make-closure capturing-env fun))
             (node (gen-specific-do-node empty capturing)))
            (gensym-reset!)
            (let* ((result (hoist-values node))
                   (hoisted (car result))
                   (init (cadr result)))
              (assert-ast init
                          (do (symbol name1)
                              (primop-app &make-closure env (symbol name2)))
                          (assert name1 'closure2)
                          (assert name2 'function3)
                          (assert env capturing-env)
                          (assert (length hoisted) 3)
                          (assert-ast (cdr (assoc name1 hoisted))
                                      (primop-app &make-closure (const (list)) (symbol name3))
                                      (assert name3 'function1)
                                      (assert (cdr (assoc name3 hoisted))
                                              fun))
                          (assert (cdr (assoc name2 hoisted))
                                  fun))))))
