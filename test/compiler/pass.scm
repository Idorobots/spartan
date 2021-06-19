;; Pass definition & schema tests

(require "../testing.rkt")
(require "../../src/compiler/ast.rkt")
(require "../../src/compiler/env.rkt")
(require "../../src/compiler/pass.rkt")

(describe
 "pass schema"
 (it "should validate simple values"
     (assert (begin (non-empty-string? "foo")
                    #t))
     (assert (with-handlers ((schema-validation-error?
                              (constantly #t)))
               (begin (non-empty-string? "")
                    #f)))
     (assert (begin (non-empty-list? '(foo bar baz))
                    #t))
     (assert (with-handlers ((schema-validation-error?
                              (constantly #t)))
               (begin (non-empty-list? '())
                    #f)))
     (assert (begin (a-list? '())
                    #t))
     (assert (with-handlers ((schema-validation-error?
                              (constantly #t)))
               (begin (a-list? "")
                    #f))))

 (it "should validate AST subset"
     (check ((node gen-valid-symbol-node))
            (assert (begin ((ast-subset? '(symbol))
                            node)
                           #t)))
     (check ((node gen-ast-node))
            (assert (with-handlers ((schema-validation-error?
                                     (constantly #t)))
                      (begin ((ast-subset? '())
                            node)
                           #t)))))

 (it "should validate key existance"
     (assert (begin ((schema "test" 'test a-list?)
                     (env 'test '()))
                    #t))
     (assert (with-handlers ((exn:fail?
                              (constantly #t)))
               (begin ((schema "test" 'test a-list?)
                       (env 'foo-bar '()))
                      #f)))))
