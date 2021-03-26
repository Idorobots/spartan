;; Letrec conversion tests.

(describe
 "derefy"
 (it "replaces assigned variables with derefs"
     (assert (derefy '() (make-list-node '()))
             (make-list-node '()))
     (assert (derefy '() (make-symbol-node 'foo))
             (make-symbol-node 'foo))
     (assert (derefy '(foo)
                     (at (location 5 23)
                         (make-symbol-node 'foo)))
             (free-vars (set 'deref 'foo)
                        (at (location 5 23)
                            (generated
                             (make-app-node
                              (at (location 5 23)
                                  (generated
                                   (make-symbol-node 'deref)))
                              (list (at (location 5 23)
                                        (make-symbol-node 'foo))))))))
     (assert (derefy '(foo)
                     (make-app-node (make-symbol-node 'bar)
                                    (list (at (location 5 23)
                                              (make-symbol-node 'foo)))))
             (make-app-node (make-symbol-node 'bar)
                            (list (free-vars (set 'deref 'foo)
                                             (at (location 5 23)
                                                 (generated
                                                  (make-app-node
                                                   (at (location 5 23)
                                                       (generated
                                                        (make-symbol-node 'deref)))
                                                   (list (at (location 5 23)
                                                             (make-symbol-node 'foo))))))))))
     (assert (derefy '(foo)
                     (bound-vars (set 'foo)
                                 (make-lambda-node (list (make-symbol-node 'foo))
                                                   (make-symbol-node 'foo))))
             (bound-vars (set 'foo)
                         (make-lambda-node (list (make-symbol-node 'foo))
                                           (make-symbol-node 'foo))))
     (assert (derefy '(foo)
                     (bound-vars (set 'bar)
                                 (make-lambda-node (list (make-symbol-node 'bar))
                                                   (at (location 5 23)
                                                       (make-symbol-node 'foo)))))
             (bound-vars (set 'bar)
                         (make-lambda-node (list (make-symbol-node 'bar))
                                           (free-vars (set 'deref 'foo)
                                                      (at (location 5 23)
                                                          (generated
                                                           (make-app-node
                                                            (at (location 5 23)
                                                                (generated
                                                                 (make-symbol-node 'deref)))
                                                            (list (at (location 5 23)
                                                                      (make-symbol-node 'foo))))))))))
     (assert (derefy '(foo)
                     (bound-vars (set 'foo)
                                 (make-let-node (list (make-binding (make-symbol-node 'foo)
                                                                    (at (location 5 23)
                                                                        (make-symbol-node 'foo))))
                                                (make-symbol-node 'foo))))
             (bound-vars (set 'foo)
                         (make-let-node (list (make-binding (make-symbol-node 'foo)
                                                            (free-vars (set 'deref 'foo)
                                                                       (at (location 5 23)
                                                                           (generated
                                                                            (make-app-node
                                                                             (at (location 5 23)
                                                                                 (generated
                                                                                  (make-symbol-node 'deref)))
                                                                             (list (at (location 5 23)
                                                                                       (make-symbol-node 'foo)))))))))
                                        (make-symbol-node 'foo))))
     (assert (derefy '(foo)
                     (bound-vars (set 'foo)
                                 (make-letrec-node (list (make-binding (make-symbol-node 'foo)
                                                                       (make-symbol-node 'foo)))
                                                   (make-symbol-node 'foo))))
             (bound-vars (set 'foo)
                         (make-letrec-node (list (make-binding (make-symbol-node 'foo)
                                                               (make-symbol-node 'foo)))
                                           (make-symbol-node 'foo))))))

(describe
 "ref-conversion"
 (it "creates fix operator forms"
     (assert (ref-conversion
              '(letrec ((foo (lambda () (foo)))) (foo)))
             '(fix ((foo (lambda () (foo))))
                   (foo)))
     (assert (ref-conversion
              '(letrec ((foo (lambda () (bar)))
                        (bar (lambda () (foo))))
                 (foo)))
             '(fix ((foo (lambda () (bar)))
                    (bar (lambda () (foo))))
                   (foo)))))
