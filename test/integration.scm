;; Actual code exmaples

(define *random* 0.05)

(describe
 "Spartan"
 (it "should support basic language features"
     (test-file "../test/sprtn/math.sprtn")
     (test-file "../test/sprtn/rsa.sprtn")
     (test-file "../test/sprtn/hello.sprtn")
     (test-file "../test/sprtn/fibonacci.sprtn")
     (test-file "../test/sprtn/logger.sprtn"))

 (it "should support continuations"
     (with-test-bindings
      (;; Silence task info logs since these might vary in the specific timings.
       (__task_info (bootstrap (lambda () '())))
       ;; Ensure that timeouts take very short time.
       (__sleep (bootstrap (lambda (time)
                             (wait 25)))))
      (test-file "../test/sprtn/errors.sprtn")
      (test-file "../test/sprtn/errors3.sprtn")
      (test-file "../test/sprtn/coroutines.sprtn")
      (test-file "../test/sprtn/coroutines2.sprtn")
      (test-file "../test/sprtn/coroutines3.sprtn")
      (test-file "../test/sprtn/amb.sprtn")))

 (it "should support Actor Model"
     (with-test-bindings
      (;; Silence task info logs since these might vary in the specific timings.
       (__task_info (bootstrap (lambda () '())))
       ;; Ensure that timeouts take very short time.
       (__sleep (bootstrap (lambda (time)
                             (wait 25))))
       ;; Ensure that monitor task doesn't ever hang the execution.
       (__monitor (bootstrap (lambda (time)
                               '()))))
      (test-file "../test/sprtn/uprocs.sprtn")
      (test-file "../test/sprtn/uprocs2.sprtn" sort-lines)
      (test-file "../test/sprtn/msgwait.sprtn")
      (test-file "../test/sprtn/fibonacci2.sprtn")
      (test-file "../test/sprtn/errors2.sprtn")))

 (it "should support the RBS"
     (with-test-bindings
      (;; Silence task info logs since these might vary in the specific timings.
       (__task_info (bootstrap (lambda () '())))
       ;; Ensure that timeouts take very short time.
       (__sleep (bootstrap (lambda (time)
                             (wait 25))))
       ;; Ensure that monitor task doesn't ever hang the execution.
       (__monitor (bootstrap (lambda (time)
                               '())))
       ;; Determined by a fairly random dice roll.
       (__random (bootstrap (lambda ()
                              (let ((r *random*))
                                (set! *random* (+ r 0.05))
                                (when (> *random* 1.0)
                                  (set! *random* 0.05))
                                r)))))
      (test-file "../test/sprtn/rbs2.sprtn")
      (test-file "../test/sprtn/rbs.sprtn")
      (test-file "../test/sprtn/cep.sprtn")))

 (ignore "handles reused variables correctly"
         (assert (run '(letrec ((fact (lambda (n)
                                        (if (< n 2)
                                            n
                                            (* n ;; NOTE the `n` here would be propagated into the `let` resulting in wrong computation.
                                               (let ((n (- n 1)))
                                                 (if (< n 2)
                                                     n
                                                     (* n (fact (- n 1))))))))))
                         (if (< 10 2)
                             10
                             (* 10 (fact 9)))))
                 3628800))

 (it "optimizes bindings out correctly"
     (assert (run '(list
                    (let ((x1 '7)) (if (= '0 x1) 't nil))
                    ;; NOTE This `x2` would remain as a free variable (and therefore an undefined variable) despite being optimized out.
                    (let ((x2 '7)) (if (= '0 x2) nil 't))))
             '(() t))))
