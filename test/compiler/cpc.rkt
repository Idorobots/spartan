#lang racket

;; Continuation Passing Style Conversion

(require "../testing.rkt")
(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/passes/cpc.rkt")
(require "../../src/compiler/utils/gensym.rkt")
(require "../../src/compiler/utils/utils.rkt")

(define gen-simple-cpc-node
  (gen-one-of gen-valid-symbol-node gen-const-node))

(describe
 "CPC"
 (it "works for the simple cases"
     (check ((node gen-const-node))
            (assert (cpc node #f id)
                    node)))

 (it "converts if correctly"
     (check ((cond gen-simple-cpc-node)
             (then gen-simple-cpc-node)
             (else gen-simple-cpc-node)
             (node (gen-if-node cond then else)))
            (gensym-reset!)
            (let ((result (cpc node #f id)))
              (assert-ast result
                          (let ((binding (symbol 'cont1) (lambda ((symbol 'value2)) (symbol 'value2))))
                            (if converted-cond
                                (primop-app '&yield-cont (symbol 'cont1) converted-then)
                                (primop-app '&yield-cont (symbol 'cont1) converted-else)))
                          (assert converted-cond cond)
                          (assert converted-then then)
                          (assert converted-else else))
              (assert (ast-node-location result)
                      (ast-node-location node)))))

 (it "converts do correctly"
     (check ((one gen-simple-cpc-node)
             (two gen-simple-cpc-node)
             (three gen-simple-cpc-node)
             (node (gen-specific-do-node one two three)))
            (gensym-reset!)
            (assert (cpc node #f id) node))
     (check ((cond gen-simple-cpc-node)
             (then gen-simple-cpc-node)
             (else gen-simple-cpc-node)
             (one (gen-if-node cond then else))
             (two gen-simple-cpc-node)
             (three gen-simple-cpc-node)
             (node (gen-specific-do-node one two three)))
            (gensym-reset!)
            (let ((result (cpc node #f id)))
              (assert-ast result
                          (let ((binding (symbol 'cont1)
                                         (lambda ((symbol 'value2))
                                                  (do (symbol 'value2) values ...))))
                            (if _
                                _
                                _))
                          (assert values (list two three)))
              (assert (ast-node-location result)
                      (ast-node-location one))))
     (check ((op1 gen-valid-symbol)
             (arg1 gen-simple-cpc-node)
             (app1 (gen-primop-app-node op1 arg1))
             (op2 gen-valid-symbol)
             (arg2 gen-simple-cpc-node)
             (app2 (gen-primop-app-node op2 arg2))
             (node (gen-specific-do-node app1 app2)))
            (gensym-reset!)
            (let ((result (cpc node #f id)))
              (assert-ast result
                          (let ((binding (symbol 'value1) (primop-app converted-op1 converted-arg1)))
                            (let ((binding (symbol 'value2) (primop-app converted-op2 converted-arg2)))
                              (do (symbol 'value1)
                                  (symbol 'value2))))
                          (assert converted-op1 op1)
                          (assert converted-op2 op2)
                          (assert converted-arg1 arg1)
                          (assert converted-arg2 arg2))
              (assert (ast-node-location result)
                      (ast-node-location app1)))))

 (it "converts lambda correctly"
     (check ((arg gen-valid-symbol-node)
             (body gen-simple-cpc-node)
             (node (gen-lambda-node (list arg) body)))
            (gensym-reset!)
            (let ((result (cpc node #f id)))
              (assert-ast result
                          (lambda (converted-arg (symbol 'cont1))
                            (primop-app '&yield-cont (symbol 'cont1) converted-body))
                          (assert converted-arg arg)
                          (assert converted-body body))
              (assert (ast-node-location result)
                      (ast-node-location node))))
     (check ((arg gen-valid-symbol-node)
             (then gen-simple-cpc-node)
             (else gen-simple-cpc-node)
             (body (gen-if-node arg then else))
             (node (gen-lambda-node (list arg) body)))
            (gensym-reset!)
            (let ((result (cpc node #f id)))
              (assert-ast result
                          (lambda (converted-arg (symbol 'cont1))
                            (let ((binding (symbol 'cont2)
                                           (lambda ((symbol 'value3))
                                                    (primop-app '&yield-cont (symbol 'cont1) (symbol 'value3)))))
                              (if cond
                                (primop-app '&yield-cont (symbol 'cont2) converted-then)
                                (primop-app '&yield-cont (symbol 'cont2) converted-else))))
                          (assert converted-arg arg)
                          (assert converted-then then)
                          (assert converted-else else))
              (assert (ast-node-location result)
                      (ast-node-location node)))))

 (it "converts primop-app correctly"
     (check ((op gen-valid-symbol)
             (args (gen-list (gen-integer 0 5) gen-simple-cpc-node))
             (node (apply gen-primop-app-node op args)))
            (gensym-reset!)
            (let ((result (cpc node #f id)))
              (assert-ast result
                          (let ((binding (symbol 'value1)
                                         (primop-app converted-op converted-args ...)))
                            (symbol 'value1))
                          (assert converted-op op)
                          (assert converted-args args))
              (assert (ast-node-location result)
                      (ast-node-location node))))
     (check ((op1 gen-valid-symbol-node)
             (arg1 gen-simple-cpc-node)
             (app1 (gen-app-node op1 arg1))
             (op2 gen-valid-symbol)
             (app2 (gen-primop-app-node op2 app1)))
            (gensym-reset!)
            (let ((result (cpc app2 #f id)))
              (assert-ast result
                          (app converted-op1
                               converted-arg1
                               (lambda ((symbol 'value2))
                                 (let ((binding (symbol 'value1)
                                                (primop-app converted-op2 (symbol 'value2))))
                                   (symbol 'value1))))
                          (assert converted-op1 op1)
                          (assert converted-op2 op2)
                          (assert converted-arg1 arg1))
              (assert (ast-node-location result)
                      (ast-node-location app1)))))

 (it "converts app correctly"
     (check ((op gen-simple-cpc-node)
             (arg gen-simple-cpc-node)
             (node (gen-app-node op arg)))
            (gensym-reset!)
            (let ((result (cpc node #f id)))
              (assert-ast result
                          (app converted-op converted-arg (lambda ((symbol 'value1)) (symbol 'value1)))
                          (assert converted-op op)
                          (assert converted-arg arg))
              (assert (ast-node-location result)
                      (ast-node-location node))))
     (check ((op1 gen-simple-cpc-node)
             (arg1 gen-simple-cpc-node)
             (app1 (gen-app-node op1 arg1))
             (arg2 gen-simple-cpc-node)
             (app2 (gen-app-node app1 arg2)))
            (gensym-reset!)
            (let ((result (cpc app2 #f id)))
              (assert-ast result
                          (app converted-op1
                               converted-arg1
                               (lambda ((symbol 'value2))
                                 (app (symbol 'value2) converted-arg2 (lambda ((symbol 'value1)) (symbol 'value1)))))
                          (assert converted-op1 op1)
                          (assert converted-arg1 arg1)
                          (assert converted-arg2 arg2))
              (assert (ast-node-location result)
                      (ast-node-location app1))))
     (check ((op1 gen-simple-cpc-node)
             (arg1 gen-simple-cpc-node)
             (app1 (gen-app-node op1 arg1))
             (arg2 gen-simple-cpc-node)
             (app2 (gen-app-node app1 arg2))
             (op3 gen-simple-cpc-node)
             (app3 (gen-app-node op3 app2)))
            (gensym-reset!)
            (let ((result (cpc app3 #f id)))
              (assert-ast result
                          (app converted-op1
                               converted-arg1
                               (lambda ((symbol 'value3))
                                 (app (symbol 'value3)
                                      converted-arg2
                                      (lambda ((symbol 'value2))
                                        (app converted-op3 (symbol 'value2) (lambda ((symbol 'value1)) (symbol 'value1)))))))
                          (assert converted-op1 op1)
                          (assert converted-op3 op3)
                          (assert converted-arg1 arg1)
                          (assert converted-arg2 arg2))
              (assert (ast-node-location result)
                      (ast-node-location app1))))
     (check ((op1 gen-simple-cpc-node)
             (arg1 gen-simple-cpc-node)
             (app1 (gen-app-node op1 arg1))
             (op2 gen-simple-cpc-node)
             (arg2 gen-simple-cpc-node)
             (app2 (gen-app-node op2 arg2))
             (app3 (gen-app-node app1 app2)))
            (gensym-reset!)
            (let ((result (cpc app3 #f id)))
              (assert-ast result
                          (app converted-op1
                               converted-arg1
                               (lambda ((symbol 'value2))
                                 (app converted-op2
                                      converted-arg2
                                      (lambda ((symbol 'value3))
                                        (app (symbol 'value2) (symbol 'value3) (lambda ((symbol 'value1)) (symbol 'value1)))))))
                          (assert converted-op1 op1)
                          (assert converted-op2 op2)
                          (assert converted-arg1 arg1)
                          (assert converted-arg2 arg2))
              (assert (ast-node-location result)
                      (ast-node-location app1)))))

 (it "converts fix correctly"
     (check ((var1 gen-valid-symbol-node)
             (arg1 gen-valid-symbol-node)
             (val1 (gen-lambda-node (list arg1) gen-simple-cpc-node))
             (binding1 (gen-binding-node var1 val1))
             (var2 gen-valid-symbol-node)
             (arg2 gen-valid-symbol-node)
             (val2 (gen-lambda-node (list arg2) gen-simple-cpc-node))
             (binding2 (gen-binding-node var2 val2))
             (body (gen-app-node var1 var2))
             (node (gen-fix-node (list binding1 binding2) body)))
            (gensym-reset!)
            (let ((result (cpc node #f id)))
              (assert-ast result
                          (fix ((binding converted-var1
                                         (lambda (_ (symbol 'cont1))
                                           (primop-app '&yield-cont (symbol 'cont1) _)))
                                (binding converted-var2
                                         (lambda (_ (symbol 'cont2))
                                           (primop-app '&yield-cont (symbol 'cont2) _))))
                               (app converted-op converted-arg (lambda ((symbol 'value3)) (symbol 'value3))))
                          (assert converted-var1 var1)
                          (assert converted-var2 var2)
                          (assert converted-op var1)
                          (assert converted-arg var2))
              (assert (ast-node-location result)
                      (ast-node-location node)))))

 (it "converts let correctly"
     (check ((var1 gen-valid-symbol-node)
             (op1 gen-simple-cpc-node)
             (val1 (gen-app-node op1 gen-simple-cpc-node))
             (binding1 (gen-binding-node var1 val1))
             (var2 gen-valid-symbol-node)
             (op2 gen-simple-cpc-node)
             (val2 (gen-app-node op2 gen-simple-cpc-node))
             (binding2 (gen-binding-node var2 val2))
             (body (gen-app-node var1 var2))
             (node (gen-let-node (list binding1 binding2)
                                 body)))
            (gensym-reset!)
            (let ((result (cpc node #f id)))
              (assert-ast result
                          (app converted-op1
                               _
                               (lambda ((symbol 'value1))
                                 (app converted-op2
                                      _
                                      (lambda ((symbol 'value2))
                                        (let ((binding converted-var1 (symbol 'value1))
                                              (binding converted-var2 (symbol 'value2)))
                                          (app _ _ (lambda ((symbol 'value3)) (symbol 'value3))))))))
                          (assert converted-op1 op1)
                          (assert converted-op2 op2)
                          (assert converted-var1 var1)
                          (assert converted-var2 var2))
              (assert (ast-node-location result)
                      (ast-node-location val1)))))

 (it "maps &current-continuation to the lexical continuation where appropriate"
     (check ((arg gen-valid-symbol-node)
             (kont gen-valid-symbol-node)
             (primop (gen-primop-app-node '&current-continuation))
             (fun (gen-lambda-node (list arg) primop)))
            (gensym-reset!)
            ;; Primop is replaced with the name...
            (let ((result (cpc primop kont id)))
              (assert-ast result
                          (symbol s)
                          (assert s (ast-symbol-value kont)))
              (assert (ast-node-location result)
                      (ast-node-location primop)))
            ;; ...unless not within a function.
            (assert string-contains?
                    (with-handlers ((exn? exn-message))
                      (cpc primop #f id))
                    "Invalid `&current-continuation` application: ")
            ;; Lambda updates the cont name.
            (assert-ast (cpc fun kont id)
                        (lambda (converted-arg s1)
                          (primop-app '&yield-cont s2 s3))
                        (assert s1 s2)
                        (assert (ast-symbol-value s1)
                                (ast-symbol-value s3))
                        (assert (ast-node-location s3)
                                (ast-node-location primop))
                        (assert converted-arg arg)))))
