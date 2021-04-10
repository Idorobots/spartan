;; Perf tests

(define (build-input-program reps)
  (let* ((expr (slurp "../test/foof/coroutines2.foo")))
    (format "(do 23 ~a)"
            (foldl string-append
                   ""
                   (make-list reps expr)))))

(describe
 "performance"
 (it "fibonacci"
     (test-perf "../test/foof/fibonacci.foo.perf" 2
                (collect-garbage 'major)
                (let ((time (time-execution
                             (run-test-file "../test/foof/fibonacci.foo"))))
                  ;; NOTE The GC time makes this test very flakey.
                  (list (car time)
                        (cadr time)))))

 (it "parser"
     (test-perf
      "../test/compiler/parser.scm.perf" 2.5
      (let ((inputs (map build-input-program (iota 0 50 5))))
        (printf "~a, ~a, ~a, ~a~n" 'file-size 'cpu 'real 'gc)
        (map (lambda (input)
               (collect-garbage 'major)
               (let ((size (+ 1 (count (partial equal? #\newline) (string->list input))))
                     (time (time-execution (parse (env 'input input
                                                       'module 'perf
                                                       'errors '())))))
                 (apply printf "~a, ~a, ~a, ~a~n" size time)
                 ;; NOTE It's hard to avoid the GC here, so in case it kicks in anyway we
                 ;; NOTE lie and deceive about the time it took to parse the file.
                 (- (car time) (caddr time))))
             inputs))))

 (it "compiler"
     (test-perf
      "../test/compiler/compiler.scm.perf" 2.5
      (let ((inputs (map build-input-program (iota 1 10 2))))
        (printf "~a, ~a, ~a, ~a~n" 'file-size 'cpu 'real 'gc)
        (map (lambda (input)
               (collect-garbage 'major)
               (let ((size (+ 1 (count (partial equal? #\newline) (string->list input))))
                     (time (time-execution (compile (env 'input input
                                                         'module 'perf)))))
                 (apply printf "~a, ~a, ~a, ~a~n" size time)
                 ;; NOTE It's hard to avoid the GC here, so in case it kicks in anyway we
                 ;; NOTE lie and deceive about the time it took to parse the file.
                 (- (car time) (caddr time))))
             inputs)))))
