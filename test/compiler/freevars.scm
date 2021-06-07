;; Free vars computation.

(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/passes/freevars.rkt")

(describe
 "free-vars"
 (it "handles values correctly"
     (define l (location 5 23))
     (assert (compute-free-vars (make-ast-number l 23))
             (make-ast-number l 23))
     (assert (compute-free-vars (make-ast-list l '()))
             (make-ast-list l '()))
     (assert (compute-free-vars
              (make-ast-quote l
                              (make-ast-list l
                                             (list (make-ast-symbol l 'foo)
                                                   (make-ast-number l 23)))))
             (make-ast-quote l
                             (make-ast-list l
                                            (list (make-ast-symbol l 'foo)
                                                  (make-ast-number l 23)))))
     (assert (compute-free-vars (make-ast-string l "foo"))
             (make-ast-string l "foo"))
     (assert (compute-free-vars (make-ast-error l
                                                (make-ast-location l)))
             (make-ast-error l
                             (make-ast-location l))))

 (it "handles syntax forms correctly"
     (define l (location 5 23))
     (assert (compute-free-vars
              (make-ast-do l
                           (list (make-ast-symbol l 'a)
                                 (make-ast-symbol l 'b)
                                 (make-ast-symbol l 'c))))
             (set-ast-node-free-vars
              (set 'a 'b 'c)
              (make-ast-do l
                           (list (make-ast-symbol l 'a)
                                 (make-ast-symbol l 'b)
                                 (make-ast-symbol l 'c)))))
     (assert (compute-free-vars
              (make-ast-if l
                           (make-ast-symbol l 'a)
                           (make-ast-symbol l 'b)
                           (make-ast-symbol l 'c)))
             (set-ast-node-free-vars
              (set 'a 'b 'c)
              (make-ast-if l
                           (make-ast-symbol l 'a)
                           (make-ast-symbol l 'b)
                           (make-ast-symbol l 'c))))
     (assert (compute-free-vars
              (make-ast-app l
                            (make-ast-symbol l 'a)
                            (list (make-ast-symbol l 'b)
                                  (make-ast-symbol l 'c))))
             (set-ast-node-free-vars
              (set 'a 'b 'c)
              (make-ast-app l
                            (make-ast-symbol l 'a)
                            (list (make-ast-symbol l 'b)
                                  (make-ast-symbol l 'c)))))
     (assert (compute-free-vars
              (make-ast-primop-app l
                                   'a
                                   (list (make-ast-symbol l 'b)
                                         (make-ast-symbol l 'c))))
             (set-ast-node-free-vars
              (set 'b 'c)
              (make-ast-primop-app l
                                   'a
                                   (list (make-ast-symbol l 'b)
                                         (make-ast-symbol l 'c))))))

 (it "handles bindings correctly"
     (define (b-n-f-vars bv fv expr)
       (set-ast-node-bound-vars bv
                                (set-ast-node-free-vars fv
                                                        expr)))
     (define l (location 5 23))
     (assert (compute-free-vars
              (make-ast-lambda l
                               '()
                               (make-ast-symbol l 'x)))
             (set-ast-node-free-vars
              (set 'x)
              (make-ast-lambda l
                               '()
                               (make-ast-symbol l 'x))))
     (assert (compute-free-vars
              (make-ast-lambda l
                               (list (make-ast-symbol l 'x))
                               (make-ast-symbol l 'x)))
             (set-ast-node-bound-vars
              (set 'x)
              (make-ast-lambda l
                               (list (make-ast-symbol l 'x))
                               (make-ast-symbol l 'x))))
     (assert (compute-free-vars
              (make-ast-let l
                            (list (make-ast-binding l
                                                    (make-ast-symbol l 'x)
                                                    (make-ast-symbol l 'y)))
                            (make-ast-symbol l 'z)))
             (set-ast-node-bound-vars
              (set 'x)
              (set-ast-node-free-vars
               (set 'y 'z)
               (make-ast-let l
                             (list (b-n-f-vars
                                    (set 'x) (set 'y)
                                    (make-ast-binding l
                                                      (make-ast-symbol l 'x)
                                                      (make-ast-symbol l 'y))))
                             (make-ast-symbol l 'z)))))
     (assert (compute-free-vars
              (make-ast-let l
                            (list (make-ast-binding l
                                                    (make-ast-symbol l 'x)
                                                    (make-ast-symbol l 'y)))
                            (make-ast-symbol l 'x)))
             (set-ast-node-bound-vars
              (set 'x)
              (set-ast-node-free-vars
               (set 'y)
               (make-ast-let l
                             (list (b-n-f-vars
                                    (set 'x) (set 'y)
                                    (make-ast-binding l
                                                      (make-ast-symbol l 'x)
                                                      (make-ast-symbol l 'y))))
                             (make-ast-symbol l 'x)))))
     (assert (compute-free-vars
              (make-ast-let l
                            (list (make-ast-binding l
                                                    (make-ast-symbol l 'x)
                                                    (make-ast-symbol l 'y))
                                  (make-ast-binding l
                                                    (make-ast-symbol l 'z)
                                                    (make-ast-symbol l 'x)))
                            (make-ast-symbol l 'z)))
             (set-ast-node-bound-vars
              (set 'x 'z)
              (set-ast-node-free-vars
               (set 'x 'y)
               (make-ast-let l
                             (list (b-n-f-vars
                                    (set 'x) (set 'y)
                                    (make-ast-binding l
                                                      (make-ast-symbol l 'x)
                                                      (make-ast-symbol l 'y)))
                                   (b-n-f-vars
                                    (set 'z) (set 'x)
                                    (make-ast-binding l
                                                      (make-ast-symbol l 'z)
                                                      (make-ast-symbol l 'x))))
                             (make-ast-symbol l 'z)))))
     (assert (compute-free-vars
              (make-ast-letrec l
                               (list (make-ast-binding l
                                                       (make-ast-symbol l 'x)
                                                       (make-ast-symbol l 'y)))
                               (make-ast-symbol l 'z)))
             (set-ast-node-bound-vars
              (set 'x)
              (set-ast-node-free-vars
               (set 'y 'z)
               (make-ast-letrec l
                                (list (b-n-f-vars
                                       (set 'x) (set 'y)
                                       (make-ast-binding l
                                                         (make-ast-symbol l 'x)
                                                         (make-ast-symbol l 'y))))
                                (make-ast-symbol l 'z)))))
     (assert (compute-free-vars
              (make-ast-letrec l
                               (list (make-ast-binding l
                                                       (make-ast-symbol l 'x)
                                                       (make-ast-symbol l 'y)))
                               (make-ast-symbol l 'x)))
             (set-ast-node-bound-vars
              (set 'x)
              (set-ast-node-free-vars
               (set 'y)
               (make-ast-letrec l
                                (list (b-n-f-vars
                                       (set 'x) (set 'y)
                                       (make-ast-binding l
                                                         (make-ast-symbol l 'x)
                                                         (make-ast-symbol l 'y))))
                                (make-ast-symbol l 'x)))))
     (assert (compute-free-vars
              (make-ast-letrec l
                               (list (make-ast-binding l
                                                       (make-ast-symbol l 'x)
                                                       (make-ast-symbol l 'y))
                                     (make-ast-binding l
                                                       (make-ast-symbol l 'z)
                                                       (make-ast-symbol l 'x)))
                               (make-ast-symbol l 'z)))
             (set-ast-node-bound-vars
              (set 'x 'z)
              (set-ast-node-free-vars
               (set 'y)
               (make-ast-letrec l
                                (list (b-n-f-vars
                                       (set 'x) (set 'y)
                                       (make-ast-binding l
                                                         (make-ast-symbol l 'x)
                                                         (make-ast-symbol l 'y)))
                                      (b-n-f-vars
                                       (set 'z) (set 'x)
                                       (make-ast-binding l
                                                         (make-ast-symbol l 'z)
                                                         (make-ast-symbol l 'x))))
                                (make-ast-symbol l 'z)))))))
