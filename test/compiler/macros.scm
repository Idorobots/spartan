;; Macro-expander tests.

(define bm (make-builtin-macros))

(define (l n) (location n n))

(describe
 "built-in macros"
 (it "let* macro works"
     (assert (expand-macros (at (l 1) (make-list-node
                                       (list (at (l 2) (make-symbol-node 'let*))
                                             (at (l 3) (make-list-node '()))
                                             (at (l 10) (make-symbol-node 'c)))))
                            bm)
             (at (l 10)
                 (generated
                  (context "Bad `let*` body syntax"
                           (make-do-node
                            (list (at (l 10)
                                      (make-symbol-node 'c))))))))
     (assert (expand-macros (at (l 1) (make-list-node
                                       (list (at (l 2) (make-symbol-node 'let*))
                                             (at (l 3) (make-list-node
                                                        (list (at (l 4)
                                                                  (make-list-node
                                                                   (list (at (l 5) (make-symbol-node 'a))
                                                                         (at (l 6) (make-number-node 23))))))))
                                             (at (l 10) (make-symbol-node 'c)))))
                            bm)
             (at (l 1)
                 (make-let-node
                  (list (at (l 4)
                            (make-binding-node
                             (at (l 5) (make-symbol-node 'a))
                             (at (l 6) (make-number-node 23)))))
                  (at (l 10)
                      (generated
                       (context "Bad `let*` body syntax"
                                (make-do-node
                                 (list (at (l 10) (make-symbol-node 'c))))))))))
     (assert (expand-macros (at (l 1) (make-list-node
                                       (list (at (l 2) (make-symbol-node 'let*))
                                             (at (l 3) (make-list-node
                                                        (list (at (l 4)
                                                                  (make-list-node
                                                                   (list (at (l 5) (make-symbol-node 'a))
                                                                         (at (l 6) (make-number-node 23)))))
                                                              (at (l 7)
                                                                  (make-list-node
                                                                   (list (at (l 8) (make-symbol-node 'b))
                                                                         (at (l 9) (make-number-node 5))))))))
                                             (at (l 10) (make-symbol-node 'c)))))
                            bm)
             (at (l 1)
                 (make-let-node
                  (list (at (l 4)
                            (make-binding-node
                             (at (l 5) (make-symbol-node 'a))
                             (at (l 6) (make-number-node 23)))))
                  (at (l 1)
                      (make-let-node
                       (list (at (l 7)
                                 (make-binding-node
                                  (at (l 8) (make-symbol-node 'b))
                                  (at (l 9) (make-number-node 5)))))
                       (at (l 10)
                           (generated
                            (context "Bad `let*` body syntax"
                                     (make-do-node
                                      (list (at (l 10) (make-symbol-node 'c)))))))))))))

 (it "handle macro works"
     (assert (expand-macros (at (l 1) (make-list-node
                                       (list (at (l 2) (make-symbol-node 'handle))
                                             (at (l 3) (make-symbol-node 'expr))
                                             (at (l 4) (make-symbol-node 'handler)))))
                            bm)
             (at (l 1) (make-app-node (at (l 1) (make-symbol-node 'call/handler))
                                      (list (at (l 4) (make-symbol-node 'handler))
                                            (at (l 1)
                                                (make-lambda-node '()
                                                                  (at (l 3) (make-symbol-node 'expr)))))))))

 (it "shift macro works"
     (assert (expand-macros (at (l 1) (make-list-node
                                       (list (at (l 2) (make-symbol-node 'shift))
                                             (at (l 3) (make-symbol-node 'kont))
                                             (at (l 4) (make-symbol-node 'expr)))))
                            bm)
             (at (l 1) (make-app-node (at (l 1) (make-symbol-node 'call/shift))
                                      (list (at (l 1)
                                                (make-lambda-node (list (at (l 3) (make-symbol-node 'kont)))
                                                                  (at (l 4)
                                                                      (generated
                                                                       (context "Bad `shift` body syntax"
                                                                                (make-do-node
                                                                                 (list (at (l 4) (make-symbol-node 'expr))))))))))))))

 (it "reset macro works"
     (assert (expand-macros (at (l 1) (make-list-node
                                       (list (at (l 2) (make-symbol-node 'reset))
                                             (at (l 3) (make-symbol-node 'expr)))))
                            bm)
             (at (l 1) (make-app-node (at (l 1) (make-symbol-node 'call/reset))
                                      (list (at (l 1)
                                                (make-lambda-node '()
                                                                  (at (l 3)
                                                                      (generated
                                                                       (context "Bad `reset` body syntax"
                                                                                (make-do-node
                                                                                 (list (at (l 3) (make-symbol-node 'expr))))))))))))))

 (it "letcc macro works"
     (assert (expand-macros (at (l 1) (make-list-node
                                       (list (at (l 2) (make-symbol-node 'letcc))
                                             (at (l 3) (make-symbol-node 'kont))
                                             (at (l 4) (make-symbol-node 'expr)))))
                            bm)
             (at (l 1) (make-app-node (at (l 1) (make-symbol-node 'call/current-continuation))
                                      (list (at (l 1)
                                                (make-lambda-node (list (at (l 3) (make-symbol-node 'kont)))
                                                                  (at (l 4)
                                                                      (generated
                                                                       (context "Bad `letcc` body syntax"
                                                                                (make-do-node
                                                                                 (list (at (l 4) (make-symbol-node 'expr)))))))))))))
     (assert (expand-macros (at (l 1) (make-list-node
                                       (list (at (l 2) (make-symbol-node 'letcc))
                                             (at (l 3) (make-symbol-node 'kont))
                                             (at (l 4) (make-symbol-node 'expr1))
                                             (at (l 5) (make-symbol-node 'expr2)))))
                            bm)
             (at (l 1) (make-app-node (at (l 1) (make-symbol-node 'call/current-continuation))
                                      (list (at (l 1)
                                                (make-lambda-node (list (at (l 3) (make-symbol-node 'kont)))
                                                                  (at (location 4 5)
                                                                      (generated
                                                                       (context "Bad `letcc` body syntax"
                                                                                (make-do-node
                                                                                 (list (at (l 4) (make-symbol-node 'expr1))
                                                                                       (at (l 5) (make-symbol-node 'expr2)))))))))))))))
