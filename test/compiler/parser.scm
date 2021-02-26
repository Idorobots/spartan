;; Parser tests.

(describe
 "parser"
 (it "parses simple expressions"
     (assert (parse "foo")
             (at (parse-location 0 3)
                 (make-symbol-node 'foo)))
     (assert (parse "(define (foo x) 23)")
             (at (parse-location 0 19)
                 (make-list-node (list (at (parse-location 1 7)
                                           (make-symbol-node 'define))
                                       (at (parse-location 8 15)
                                           (make-list-node (list (at (parse-location 9 12)
                                                                     (make-symbol-node 'foo))
                                                                 (at (parse-location 13 14)
                                                                     (make-symbol-node 'x)))))
                                       (at (parse-location 16 18)
                                           (make-number-node 23))))))
     (assert (parse "(define (oof x) 32)")
             (at (parse-location 0 19)
                 (make-list-node (list (at (parse-location 1 7)
                                           (make-symbol-node 'define))
                                       (at (parse-location 8 15)
                                           (make-list-node (list (at (parse-location 9 12)
                                                                     (make-symbol-node 'oof))
                                                                 (at (parse-location 13 14)
                                                                     (make-symbol-node 'x)))))
                                       (at (parse-location 16 18)
                                           (make-number-node 32)))))))

 (it "parses comments"
     (assert (parse "(define (foo x) ;; Coments should be removed!
                   true)")
             (at (parse-location 0 70)
                 (make-list-node (list (at (parse-location 1 7)
                                           (make-symbol-node 'define))
                                       (at (parse-location 8 15)
                                           (make-list-node (list (at (parse-location 9 12)
                                                                     (make-symbol-node 'foo))
                                                                 (at (parse-location 13 14)
                                                                     (make-symbol-node 'x)))))
                                       (at (parse-location 65 69)
                                           (make-symbol-node 'true)))))))

 (it "parses all the examples"
     (define (expected-read input)
       (with-input-from-string input
         (lambda ()
           (read))))
     (map (lambda (filename)
            (let ((contents (slurp filename)))
              (assert (ast->plain (parse contents))
                      (expected-read contents))))
          (filter (lambda (filename)
                    (string-suffix? filename ".foo"))
                  (map (lambda (path)
                         (string-append "../test/foof/"
                                        (path->string path)))
                       (directory-list "../test/foof/"))))))
