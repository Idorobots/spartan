;; Parser tests.

(assert (parse "foo") 'foo)
(assert (parse "(define (foo x) 23)") '(define (foo x) 23))
(assert (parse "(define (foo x) ;; Coments should be removed!
                   true)")
        '(define (foo x) true))
(assert (parse (slurp "../test/foof/cep.foo"))
        '(letrec ((process (lambda (prefix t count)
                             (unless (= count 0)
                               (assert! `(notify ,prefix ,(random)))
                               (sleep t)
                               (process prefix t (- count 1)))))
                  (listen (lambda (count)
                            (unless (= count 0)
                              (let ((m (recv)))
                                (display "Complex event: ")
                                (display m)
                                (newline)
                                (listen (- count 1)))))))
           (notify-whenever (spawn (lambda ()
                                     (listen 10)))
                            '(filter (and (?notify foo ?foo)
                                          (?notify bar ?bar))
                                     (>= ?foo 0.5)
                                     (< ?foo 0.75)
                                     (<= ?bar 0.1)))
           (process 'foo 1000 100)
           (process 'bar 5000 100)))

;; Some benchmarks

(time-execution
 (parse
  (let ((expr (slurp "../test/foof/coroutines2.foo")))
    (format "(begin ~a)"
            (foldl string-append
                   ""
                   (make-list 10 expr))))))
