#lang racket

;; Perf tests

(require "../testing.rkt")
(require "../../src/main.rkt")
(require "../../src/compiler/compiler.rkt")
(require "../../src/compiler/env.rkt")
(require "../../src/compiler/pass.rkt")
(require "../../src/compiler/passes/parser.rkt")
(require "../../src/compiler/utils/io.rkt")
(require "../../src/compiler/utils/utils.rkt")

(define (build-input-program reps)
  (let* ((expr (slurp "examples/rsa.sprtn")))
    (format "(do 23 ~a)"
            (foldl string-append
                   ""
                   (make-list reps expr)))))

;; NOTE The GC time makes these test very flakey, so it is mostly skipped.
(describe
 "performance"
 (it "parser"
     (test-perf
      "test/data/perf/parser.rkt.perf" 2.5
      (let ((inputs (map build-input-program (iota 1 51 5))))
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
      "test/data/perf/compiler.rkt.perf" 2.5
      (let ((inputs (map build-input-program (iota 1 21 5))))
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
     (test-perf "examples/fibonacci.sprtn" 2))

 (it "rsa"
     (test-perf "examples/rsa.sprtn" 2))

 (it "amb"
     (test-perf "examples/amb.sprtn" 2)))
