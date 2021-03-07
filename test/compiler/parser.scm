;; Parser tests.

(define (p string)
  (env-get (parse (env 'input string
                       'module 'test))
           'ast))

(describe
 "parser"
 (it "parses simple expressions"
     (assert (p "foo")
             (at (location 0 3)
                 (make-symbol-node 'foo)))
     (assert (p "(define (foo x) 23)")
             (at (location 0 19)
                 (make-list-node (list (at (location 1 7)
                                           (make-symbol-node 'define))
                                       (at (location 8 15)
                                           (make-list-node (list (at (location 9 12)
                                                                     (make-symbol-node 'foo))
                                                                 (at (location 13 14)
                                                                     (make-symbol-node 'x)))))
                                       (at (location 16 18)
                                           (make-number-node 23))))))
     (assert (p "(define (oof x) 32)")
             (at (location 0 19)
                 (make-list-node (list (at (location 1 7)
                                           (make-symbol-node 'define))
                                       (at (location 8 15)
                                           (make-list-node (list (at (location 9 12)
                                                                     (make-symbol-node 'oof))
                                                                 (at (location 13 14)
                                                                     (make-symbol-node 'x)))))
                                       (at (location 16 18)
                                           (make-number-node 32)))))))

 (it "parses structure refs"
     (assert (p "foo.bar")
             (at (location 0 7)
                 (make-structure-ref-node
                  (list 'foo 'bar))))
     (assert (p "foo.bar.baz.faz")
             (at (location 0 15)
                 (make-structure-ref-node
                  (list 'foo 'bar 'baz 'faz)))))

 (it "parses strings"
     (assert (p "\"this is a string\"")
             (at (location 0 18)
                 (make-string-node "this is a string")))
     (assert (p "(define foo \"this is a string\")")
             (at (location 0 31)
                 (make-list-node (list (at (location 1 7)
                                           (make-symbol-node 'define))
                                       (at (location 8 11)
                                           (make-symbol-node 'foo))
                                       (at (location 12 30)
                                           (make-string-node "this is a string")))))))

 (it "parses comments"
     (assert (p "(define (foo x) ;; Coments should be removed!
                   true)")
             (at (location 0 70)
                 (make-list-node (list (at (location 1 7)
                                           (make-symbol-node 'define))
                                       (at (location 8 15)
                                           (make-list-node (list (at (location 9 12)
                                                                     (make-symbol-node 'foo))
                                                                 (at (location 13 14)
                                                                     (make-symbol-node 'x)))))
                                       (at (location 65 69)
                                           (make-symbol-node 'true)))))))

 (it "handles invalid symbols gracefully"
     (assert (p "foo.")
             (at (location 0 4)
                 (make-invalid-symbol-node "foo.")))
     (assert (p "foo.bar.")
             (at (location 0 8)
                 (make-invalid-symbol-node "foo.bar.")))
     (assert (p ".foo")
             (at (location 0 4)
                 (make-invalid-symbol-node ".foo")))
     (assert (p "foo..bar")
             (at (location 0 8)
                 (make-invalid-symbol-node "foo..bar")))
     (assert (p "...")
             (at (location 0 3)
                 (make-invalid-symbol-node "...")))
     (assert (p ".")
             (at (location 0 1)
                 (make-invalid-symbol-node "."))))

 (it "handles unterminated lists gracefully"
     (assert (p "(")
             (at (location 0 1)
                 (make-unterminated-list-node '())))
     (assert (p "(()")
            (at (location 0 3)
                (make-unterminated-list-node
                 (list (at (location 1 3)
                           (make-list-node '()))))))
     (assert (p "(define (foo x) 23")
             (at (location 0 18)
                 (make-unterminated-list-node
                  (list (at (location 1 7)
                            (make-symbol-node 'define))
                        (at (location 8 15)
                            (make-list-node (list (at (location 9 12)
                                                      (make-symbol-node 'foo))
                                                  (at (location 13 14)
                                                      (make-symbol-node 'x)))))
                        (at (location 16 18)
                            (make-number-node 23))))))
     (assert (p "(define (foo x 23)")
             (at (location 0 18)
                 (make-unterminated-list-node
                  (list (at (location 1 7)
                            (make-symbol-node 'define))
                        (at (location 8 18)
                            (make-list-node (list (at (location 9 12)
                                                      (make-symbol-node 'foo))
                                                  (at (location 13 14)
                                                      (make-symbol-node 'x))
                                                  (at (location 15 17)
                                                      (make-number-node 23))))))))))

 (it "handles unterminated strings gracefully"
     (assert (p "\"This is an unterminated string")
             (at (location 0 31)
                 (make-unterminated-string-node "This is an unterminated string")))
     (assert (p "(define foo \"This is an unterminated string)")
             (at (location 0 44)
                 (make-unterminated-list-node
                  (list (at (location 1 7)
                            (make-symbol-node 'define))
                        (at (location 8 11)
                            (make-symbol-node 'foo))
                        (at (location 12 44)
                            (make-unterminated-string-node "This is an unterminated string)")))))))

 (it "handles unterminated comments"
     (assert (p "(define (foo x) ;; Coments should be removed!")
             (at (location 0 45)
                 (make-unterminated-list-node
                  (list (at (location 1 7)
                            (make-symbol-node 'define))
                        (at (location 8 15)
                            (make-list-node (list (at (location 9 12)
                                                      (make-symbol-node 'foo))
                                                  (at (location 13 14)
                                                      (make-symbol-node 'x))))))))))

 (it "handles unterminated quotation"
     (assert (p "'")
             (at (location 0 1)
                 (make-unterminated-quote-node "'")))
     (assert (p "`")
             (at (location 0 1)
                 (make-unterminated-quote-node "`")))
     (assert (p ",")
             (at (location 0 1)
                 (make-unterminated-quote-node ",")))
     (assert (p ",@")
             (at (location 0 2)
                 (make-unterminated-quote-node ",@")))
     (assert (p "(define (foo x) ')")
             (at (location 0 18)
                 (make-list-node (list (at (location 1 7)
                                           (make-symbol-node 'define))
                                       (at (location 8 15)
                                           (make-list-node (list (at (location 9 12)
                                                                     (make-symbol-node 'foo))
                                                                 (at (location 13 14)
                                                                     (make-symbol-node 'x)))))
                                       (at (location 16 17)
                                           (make-unterminated-quote-node "'")))))))

 (it "handles extra unmatched tokens"
     (assert (p ")")
             (at (location 0 1)
                 (make-list-node
                  (list (at (location 0 1)
                            (make-unmatched-token-node ")"))))))
     (assert (p "(define (foo x) x))")
             (at (location 0 19)
                 (make-list-node
                  (list (at (location 0 18)
                            (make-list-node (list (at (location 1 7)
                                                      (make-symbol-node 'define))
                                                  (at (location 8 15)
                                                      (make-list-node (list (at (location 9 12)
                                                                                (make-symbol-node 'foo))
                                                                            (at (location 13 14)
                                                                                (make-symbol-node 'x)))))
                                                  (at (location 16 17)
                                                      (make-symbol-node 'x)))))
                        (at (location 18 19)
                            (make-unmatched-token-node ")"))))))
     (assert (p "(define (foo x)) x)")
             (at (location 0 19)
                 (make-list-node
                  (list (at (location 0 16)
                            (make-list-node (list (at (location 1 7)
                                                      (make-symbol-node 'define))
                                                  (at (location 8 15)
                                                      (make-list-node (list (at (location 9 12)
                                                                                (make-symbol-node 'foo))
                                                                            (at (location 13 14)
                                                                                (make-symbol-node 'x))))))))
                        (at (location 17 18)
                            (make-symbol-node 'x))
                        (at (location 18 19)
                            (make-unmatched-token-node ")")))))))

 (it "parses all the examples"
     (define (expected-read input)
       (with-input-from-string input
         (lambda ()
           (read))))
     (map (lambda (filename)
            (let ((contents (slurp filename)))
              (assert (ast->plain (env-get (p contents) 'ast))
                      (expected-read contents))))
          (filter (compose (lambda (filename)
                             ;; FIXME This now somewhat parses the full file, while previously it only parsed the first expr.
                             (equal? filename "../test/foof/modules.foo"))
                           (lambda (filename)
                    (string-suffix? filename ".foo")))
                  (map (lambda (path)
                         (string-append "../test/foof/"
                                        (path->string path)))
                       (directory-list "../test/foof/"))))))
