;; Actual code exmaples

(define *random* 0.05)

(describe
 "FOOF"
 (it "should support basic language features"
     (test-file "../test/foof/hello.foo")
     (test-file "../test/foof/fibonacci.foo")
     (test-file "../test/foof/logger.foo"))
 (it "should support continuations"
     (with-test-bindings
      (;; Silence task info logs since these might vary in the specific timings.
       (__task_info (bootstrap (lambda () '())))
       ;; Ensure that timeouts take very short time.
       (__sleep (bootstrap (lambda (time)
                             (wait 25)))))
      (test-file "../test/foof/errors.foo")
      (test-file "../test/foof/errors3.foo")
      (test-file "../test/foof/coroutines.foo")
      (test-file "../test/foof/coroutines2.foo")
      (test-file "../test/foof/coroutines3.foo")))
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
      (test-file "../test/foof/uprocs.foo")
      (test-file "../test/foof/uprocs2.foo" sort-lines)
      (test-file "../test/foof/msgwait.foo") ;; FIXME Sometimes broken.
      (test-file "../test/foof/fibonacci2.foo")
      (test-file "../test/foof/errors2.foo")))
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
      (test-file "../test/foof/rbs2.foo")
      (test-file "../test/foof/rbs.foo") ;; FIXME Kinda broken.
      (test-file "../test/foof/cep.foo"))))
