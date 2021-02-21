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

;; Some benchmarks

(define (iota from to step)
  (if (> from to)
      '()
      (cons from (iota (+ from step) to step))))

(printf "~a, ~a, ~a, ~a~n" 'file-size 'cpu 'real 'gc)
(map (lambda (reps)
       (collect-garbage)
       (let* ((expr (slurp "../test/foof/coroutines2.foo"))
              (input (format "(begin ~a)"
                             (foldl string-append
                                    ""
                                    (make-list reps expr))))
              (size (+ 1 (count (partial equal? #\newline) (string->list input))))
              (time (time-execution (parse (string->immutable-string input)))))
         (apply printf "~a, ~a, ~a, ~a~n" size time)
         time))
     (iota 0 50 5))

;; file-size, cpu, real, gc
;; 1, 1, 1, 0
;; 166, 119, 120, 3
;; 331, 241, 242, 11
;; 496, 384, 384, 38
;; 661, 539, 539, 65
;; 826, 694, 694, 108
;; 991, 955, 955, 215
;; 1156, 1159, 1159, 293
;; 1321, 1510, 1509, 499
;; 1486, 1879, 1877, 734
;; 1651, 2065, 2062, 808
