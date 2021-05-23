;; Perf tests

(define (build-input-program reps)
  (let* ((expr (slurp "../test/sprtn/coroutines2.sprtn")))
    (format "(do 23 ~a)"
            (foldl string-append
                   ""
                   (make-list reps expr)))))

(define (run-perf-test filename)
  (test-perf (string-append filename ".perf") 2
             (collect-garbage 'major)
             (let* ((compiled (compile
                               (env 'input (slurp filename)
                                    'module "perf"
                                    'no-validation #t)))
                    (time (time-execution
                           (run-code compiled))))
               (list (car time)
                     (cadr time)))))

;; NOTE The GC time makes these test very flakey, so it is mostly skipped.
(describe
 "performance"
 (it "parser"
     (test-perf
      "../test/compiler/parser.scm.perf" 2.5
      (let ((inputs (map build-input-program (iota 0 50 5))))
        (printf "~a, ~a, ~a, ~a~n" 'file-size 'cpu 'real 'gc)
        (map (lambda (input)
               (collect-garbage 'major)
               (let ((size (+ 1 (count (partial equal? #\newline) (string->list input))))
                     (time (time-execution ((pass-transform parse)
                                            (env 'input input
                                                 'module "perf"
                                                 'errors '()
                                                 'no-validation #t)))))
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
                                                         'module "perf"
                                                         'no-validation #t)))))
                 (apply printf "~a, ~a, ~a, ~a~n" size time)
                 ;; NOTE It's hard to avoid the GC here, so in case it kicks in anyway we
                 ;; NOTE lie and deceive about the time it took to parse the file.
                 (- (car time) (caddr time))))
             inputs))))

 (it "fibonacci"
     (run-perf-test "../test/sprtn/fibonacci.sprtn"))

 (it "rsa"
     (run-perf-test "../test/sprtn/rsa.sprtn"))

 (it "amb"
     (run-perf-test "../test/sprtn/amb.sprtn")))
