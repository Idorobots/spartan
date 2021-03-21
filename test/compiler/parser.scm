;; Parser tests.

(define (p string)
  (env-get (parse (env 'input string
                       'module 'test
                       'errors '()))
           'ast))

(define (pe string)
  (map (lambda (e)
         (format "~a ~a ~a"
                 (compilation-error-what e)
                 (car (compilation-error-location e))
                 (cdr (compilation-error-location e))))
       (env-get (parse (env 'input string
                            'module 'test
                            'errors '()))
                'errors)))

(describe
 "parser"
 (it "`expand-structure-refs` expands symbol into proper structure accessors"
     (let ((loc (location 5 23)))
       (assert (expand-structure-refs loc 'foo '(bar))
               (at loc
                   (generated
                    (make-primop-app-node
                     (at loc
                         (generated
                          (make-symbol-node '&structure-ref)))
                     (list (at loc
                               (make-symbol-node 'foo))
                           (at loc
                               (generated
                                (make-quote-node
                                 (at loc
                                     (make-symbol-node 'bar))))))))))
       (assert (expand-structure-refs loc 'foo '(bar baz faz))
               (at loc
                   (generated
                    (make-primop-app-node
                     (at loc
                         (generated
                          (make-symbol-node '&structure-ref)))
                     (list (at loc
                               (generated
                                (make-primop-app-node
                                 (at loc
                                     (generated
                                      (make-symbol-node '&structure-ref)))
                                 (list (at loc
                                           (generated
                                            (make-primop-app-node
                                             (at loc
                                                 (generated
                                                  (make-symbol-node '&structure-ref)))
                                             (list (at loc
                                                       (make-symbol-node 'foo))
                                                   (at loc
                                                       (generated
                                                        (make-quote-node
                                                         (at loc
                                                             (make-symbol-node 'bar)))))))))
                                       (at loc
                                           (generated
                                            (make-quote-node
                                             (at loc
                                                 (make-symbol-node 'baz)))))))))
                           (at loc
                               (generated
                                (make-quote-node
                                 (at loc
                                     (make-symbol-node 'faz))))))))))))

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
             (expand-structure-refs (location 0 7) 'foo '(bar)))
     (assert (p "foo.bar.baz.faz")
             (expand-structure-refs (location 0 15) 'foo '(bar baz faz))))

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
     (assert (pe "foo.")
             (list "Invalid symbol `foo.` specified at: 0 4"))
     (assert (pe "foo.bar.")
             (list "Invalid symbol `foo.bar.` specified at: 0 8"))
     (assert (pe ".foo")
             (list "Invalid symbol `.foo` specified at: 0 4"))
     (assert (pe "foo..bar")
             (list "Invalid symbol `foo..bar` specified at: 0 8"))
     (assert (pe "...")
             (list "Invalid symbol `...` specified at: 0 3"))
     (assert (pe ".")
             (list "Invalid symbol `.` specified at: 0 1")))

 (it "handles unterminated lists gracefully"
     (assert (pe "(")
             (list "Unterminated list, expected a closing `)` to follow: 0 1"))
     (assert (pe "(()")
             (list "Unterminated list, expected a closing `)` to follow: 0 3"))
     (assert (pe "(define (foo x) 23")
             (list "Unterminated list, expected a closing `)` to follow: 0 18"))
     (assert (pe "(define (foo x 23)")
             (list "Unterminated list, expected a closing `)` to follow: 0 18")))

 (it "handles unterminated strings gracefully"
     (assert (pe "\"This is an unterminated string")
             (list "Unterminated string literal, expected a closing `\"` to follow: 0 31"))
     (assert (pe "(define foo \"This is an unterminated string)")
             (list "Unterminated list, expected a closing `)` to follow: 0 44"
                   "Unterminated string literal, expected a closing `\"` to follow: 12 44")))

 (it "handles unterminated comments"
     (assert (pe "(define (foo x) ;; Coments should be removed!")
             (list "Unterminated list, expected a closing `)` to follow: 0 45")))

 (it "handles unterminated quotation"
     (assert (pe "'")
             (list "No expression following `'`: 0 1"))
     (assert (pe "`")
             (list "No expression following ```: 0 1"))
     (assert (pe ",")
             (list "No expression following `,`: 0 1"))
     (assert (pe ",@")
             (list "No expression following `,@`: 0 2"))
     (assert (pe "(define (foo x) ')")
             (list "No expression following `'`: 16 17")))

 (it "handles extra unmatched tokens"
     (assert (pe ")")
             (list "Unmatched `)`, expected an opening `(` to come before: 0 1"))
     (assert (pe "(define (foo x) x))")
             (list "Unmatched `)`, expected an opening `(` to come before: 18 19"))
     (assert (pe "(define (foo x)) x)")
             (list "Unmatched `)`, expected an opening `(` to come before: 18 19")))

 (it "parses all the examples"
     (define (expected-read input)
       (with-input-from-string input
         (lambda ()
           (read))))
     (map (lambda (filename)
            (let ((contents (slurp filename)))
              (assert (ast->plain (p contents))
                      (expected-read contents))))
          (filter (lambda (filename)
                    (not (equal? filename "../test/foof/logger.foo")))
                  (filter (lambda (filename)
                            (string-suffix? filename ".foo"))
                          (map (lambda (path)
                                 (string-append "../test/foof/"
                                                (path->string path)))
                               (directory-list "../test/foof/")))))))
