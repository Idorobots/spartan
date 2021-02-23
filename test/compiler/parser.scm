;; Parser tests.

(describe
 "parser"
 (it "parses simple expressions"
     (assert (parse "foo") 'foo)
     (assert (parse "(define (foo x) 23)") '(define (foo x) 23))
     (assert (parse "(define (oof x) 32)") '(define (oof x) 32)))
 (it "parses comments"
     (assert (parse "(define (foo x) ;; Coments should be removed!
                   true)")
             '(define (foo x) true)))
 (it "parses all the examples"
     (define (expected-read input)
       (with-input-from-string input
         (lambda ()
           (read))))
     (map (lambda (filename)
            (let ((contents (slurp filename)))
              (assert (parse contents)
                      (expected-read contents))))
          (filter (lambda (filename)
                    (string-suffix? filename ".foo"))
                  (map (lambda (path)
                         (string-append "../test/foof/"
                                        (path->string path)))
                       (directory-list "../test/foof/"))))))
