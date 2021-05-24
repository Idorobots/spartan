;; Macro-expander tests.

(define bm (make-builtin-macros))

(define (l n) (location n n))

(describe
 "built-in macros"
 (it "let* macro works"
     (assert (expand-macros (at (l 1) (make-ast-list
                                       (list (at (l 2) (make-ast-symbol 'let*))
                                             (at (l 3) (make-ast-list '()))
                                             (at (l 10) (make-ast-symbol 'c)))))
                            bm)
             (at (l 1)
                 (make-ast-body
                  (list (at (l 10)
                            (make-ast-symbol 'c)))
                  "Bad `let*` body syntax")))
     (assert (expand-macros (at (l 1) (make-ast-list
                                       (list (at (l 2) (make-ast-symbol 'let*))
                                             (at (l 3) (make-ast-list
                                                        (list (at (l 4)
                                                                  (make-ast-list
                                                                   (list (at (l 5) (make-ast-symbol 'a))
                                                                         (at (l 6) (make-ast-number 23))))))))
                                             (at (l 10) (make-ast-symbol 'c)))))
                            bm)
             (at (l 1)
                 (make-ast-let
                  (list (at (l 4)
                            (make-ast-binding
                             (at (l 5) (make-ast-symbol 'a))
                             (at (l 6) (make-ast-number 23)))))
                  (at (l 1)
                      (make-ast-body
                       (list (at (l 10) (make-ast-symbol 'c)))
                       "Bad `let*` body syntax")))))
     (assert (expand-macros (at (l 1) (make-ast-list
                                       (list (at (l 2) (make-ast-symbol 'let*))
                                             (at (l 3) (make-ast-list
                                                        (list (at (l 4)
                                                                  (make-ast-list
                                                                   (list (at (l 5) (make-ast-symbol 'a))
                                                                         (at (l 6) (make-ast-number 23)))))
                                                              (at (l 7)
                                                                  (make-ast-list
                                                                   (list (at (l 8) (make-ast-symbol 'b))
                                                                         (at (l 9) (make-ast-number 5))))))))
                                             (at (l 10) (make-ast-symbol 'c)))))
                            bm)
             (at (l 1)
                 (make-ast-let
                  (list (at (l 4)
                            (make-ast-binding
                             (at (l 5) (make-ast-symbol 'a))
                             (at (l 6) (make-ast-number 23)))))
                  (at (l 1)
                      (make-ast-let
                       (list (at (l 7)
                                 (make-ast-binding
                                  (at (l 8) (make-ast-symbol 'b))
                                  (at (l 9) (make-ast-number 5)))))
                       (at (l 1)
                           (make-ast-body
                            (list (at (l 10) (make-ast-symbol 'c)))
                            "Bad `let*` body syntax"))))))))

 (it "handle macro works"
     (assert (expand-macros (at (l 1) (make-ast-list
                                       (list (at (l 2) (make-ast-symbol 'handle))
                                             (at (l 3) (make-ast-symbol 'expr))
                                             (at (l 4) (make-ast-symbol 'handler)))))
                            bm)
             (at (l 1) (make-ast-app (at (l 1) (make-ast-symbol 'call/handler))
                                     (list (at (l 4) (make-ast-symbol 'handler))
                                           (at (l 1)
                                               (make-ast-lambda '()
                                                                (at (l 3) (make-ast-symbol 'expr)))))))))

 (it "shift macro works"
     (assert (expand-macros (at (l 1) (make-ast-list
                                       (list (at (l 2) (make-ast-symbol 'shift))
                                             (at (l 3) (make-ast-symbol 'kont))
                                             (at (l 4) (make-ast-symbol 'expr)))))
                            bm)
             (at (l 1) (make-ast-app (at (l 1) (make-ast-symbol 'call/shift))
                                     (list (at (l 1)
                                               (make-ast-lambda (list (at (l 3) (make-ast-symbol 'kont)))
                                                                (at (l 1)
                                                                    (make-ast-body
                                                                     (list (at (l 4) (make-ast-symbol 'expr)))
                                                                     "Bad `shift` body syntax")))))))))

 (it "reset macro works"
     (assert (expand-macros (at (l 1) (make-ast-list
                                       (list (at (l 2) (make-ast-symbol 'reset))
                                             (at (l 3) (make-ast-symbol 'expr)))))
                            bm)
             (at (l 1) (make-ast-app (at (l 1) (make-ast-symbol 'call/reset))
                                     (list (at (l 1)
                                               (make-ast-lambda '()
                                                                (at (l 1)
                                                                    (make-ast-body
                                                                     (list (at (l 3) (make-ast-symbol 'expr)))
                                                                     "Bad `reset` body syntax")))))))))

 (it "letcc macro works"
     (assert (expand-macros (at (l 1) (make-ast-list
                                       (list (at (l 2) (make-ast-symbol 'letcc))
                                             (at (l 3) (make-ast-symbol 'kont))
                                             (at (l 4) (make-ast-symbol 'expr)))))
                            bm)
             (at (l 1) (make-ast-app (at (l 1) (make-ast-symbol 'call/current-continuation))
                                     (list (at (l 1)
                                               (make-ast-lambda (list (at (l 3) (make-ast-symbol 'kont)))
                                                                (at (l 1)
                                                                    (make-ast-body
                                                                     (list (at (l 4) (make-ast-symbol 'expr)))
                                                                     "Bad `letcc` body syntax"))))))))
     (assert (expand-macros (at (l 1) (make-ast-list
                                       (list (at (l 2) (make-ast-symbol 'letcc))
                                             (at (l 3) (make-ast-symbol 'kont))
                                             (at (l 4) (make-ast-symbol 'expr1))
                                             (at (l 5) (make-ast-symbol 'expr2)))))
                            bm)
             (at (l 1) (make-ast-app (at (l 1) (make-ast-symbol 'call/current-continuation))
                                     (list (at (l 1)
                                               (make-ast-lambda (list (at (l 3) (make-ast-symbol 'kont)))
                                                                (at (l 1)
                                                                    (make-ast-body
                                                                     (list (at (l 4) (make-ast-symbol 'expr1))
                                                                           (at (l 5) (make-ast-symbol 'expr2)))
                                                                     "Bad `letcc` body syntax"))))))))))
