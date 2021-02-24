;; Perf tests

(describe
 "performance"
 (it "fibonacci"
     (test-perf "../test/foof/fibonacci.foo" 1.5))

 (it "parser"
     (test-perf
      "../test/compiler/parser.scm.perf" 1.5
      (let ((inputs (map (lambda (reps)
                           (let* ((expr (slurp "../test/foof/coroutines2.foo")))
                             (format "(begin ~a)"
                                     (foldl string-append
                                            ""
                                            (make-list reps expr)))))
                         (iota 0 50 5))))
        (printf "~a, ~a, ~a, ~a~n" 'file-size 'cpu 'real 'gc)
        (map (lambda (input)
               (collect-garbage)
               (let ((size (+ 1 (count (partial equal? #\newline) (string->list input))))
                     (time (time-execution (parse input))))
                 (apply printf "~a, ~a, ~a, ~a~n" size time)
                 (car time)))
             inputs)))))
