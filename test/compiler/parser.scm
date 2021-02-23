;; Parser tests.

(assert (parse "foo") 'foo)
(assert (parse "(define (foo x) 23)") '(define (foo x) 23))
(assert (parse "(define (oof x) 32)") '(define (oof x) 32))
(assert (parse "(define (foo x) ;; Coments should be removed!
                   true)")
        '(define (foo x) true))

(define (expected-read input)
  (with-input-from-string input
    (lambda ()
      (read))))

(map (lambda (filename)
       (let ((contents (slurp filename)))
         (assert (parse contents)
                 (expected-read contents))))
     (list
      "../test/foof/hello.foo"
      "../test/foof/fibonacci.foo"
      "../test/foof/logger.foo"
      "../test/foof/errors.foo"
      "../test/foof/coroutines.foo"
      "../test/foof/coroutines2.foo"
      "../test/foof/uprocs.foo"
      "../test/foof/uprocs2.foo"
      "../test/foof/msgwait.foo"
      "../test/foof/fibonacci2.foo"
      "../test/foof/errors2.foo"
      "../test/foof/rbs2.foo"
      "../test/foof/rbs.foo"
      "../test/foof/cep.foo"))
