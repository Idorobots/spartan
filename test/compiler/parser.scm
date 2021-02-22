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
;; 166, 38, 38, 0
;; 331, 72, 72, 0
;; 496, 104, 105, 0
;; 661, 137, 136, 0
;; 826, 214, 213, 30
;; 991, 255, 256, 35
;; 1156, 362, 361, 93
;; 1321, 358, 358, 59
;; 1486, 469, 468, 134
;; 1651, 528, 528, 147
