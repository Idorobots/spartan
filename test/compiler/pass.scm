;; Pass definition & schema tests

(describe
 "pass schema"
 (it "should validate simple values"
     (assert (begin (non-empty-string? "foo")
                    #t))
     (assert (with-handlers ((exn:fail?
                              (constantly #t)))
               (begin (non-empty-string? "")
                    #f)))
     (assert (begin (non-empty-list? '(foo bar baz))
                    #t))
     (assert (with-handlers ((exn:fail?
                              (constantly #t)))
               (begin (non-empty-list? '())
                    #f)))
     (assert (begin (a-list? '())
                    #t))
     (assert (with-handlers ((exn:fail?
                              (constantly #t)))
               (begin (a-list? "")
                    #f))))

 (it "should validate key existance"
     (assert (begin ((schema 'test a-list?)
                     (env 'test '()))
                    #t))
     (assert (with-handlers ((exn:fail?
                              (constantly #t)))
               (begin ((schema 'test a-list?)
                       (env 'foo-bar '()))
                      #f))))

 (it "should validate AST subset"
     (check ((node gen-valid-symbol-node))
            (assert (begin ((ast-subset? '(symbol))
                            node)
                           #t)))
     (check ((node gen-ast-node))
            (assert (with-handlers ((exn:fail?
                                     (constantly #t)))
                      (begin ((ast-subset? '())
                            node)
                           #t))))))
