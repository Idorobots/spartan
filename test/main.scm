;; Top level for tests.

(load "../test/utils.scm")

;; Compiler unit tests:
(load "../test/compiler/utils.scm")
(load "../test/compiler/freevars.scm")
(load "../test/compiler/substitute.scm")
(load "../test/compiler/peggen.scm")
(load "../test/compiler/parser.scm")
(load "../test/compiler/ast.scm")
(load "../test/compiler/syntax.scm")
(load "../test/compiler/macros.scm")
(load "../test/compiler/letrec.scm")
(load "../test/compiler/anormal.scm")
(load "../test/compiler/cpc.scm")
(load "../test/compiler/closures.scm")
(load "../test/compiler/rename.scm")

;; Runtime unit tests:
(load "../test/rt/queue.scm")
(load "../test/rt/scheduler.scm")
(load "../test/rt/recurse.scm")
(load "../test/rt/continuations.scm")
(load "../test/rt/exception.scm")
(load "../test/rt/actor.scm")
(load "../test/rt/modules.scm")

;; Some integration tests:

;; Silence task info logs since these might vary in the specific timings.
(define __task_info (bootstrap (lambda () '())))

;; Ensure that timeouts take very short time.
(define __sleep (bootstrap (lambda (time)
                             (wait 25))))

;; Ensure that monitor task doesn't ever hang the execution.
(define __monitor (bootstrap (lambda (time)
                               '())))

;; Determined by a fairly random dice roll.
(define *random* 0.05)
(define __random (bootstrap (lambda ()
                              (let ((r *random*))
                                (set! *random* (+ r 0.05))
                                (when (> *random* 1.0)
                                    (set! *random* 0.05))
                                r))))

(describe
 "FOOF"
 (it "should support basic language features"
     (test-file "../test/foof/hello.foo")
     (test-file "../test/foof/fibonacci.foo")
     ;; (test-file "../test/foof/logger.foo") ;; FIXME Makes no sense to run it untill proper module handling is implemented.
     )
 (it "should support continuations"
     (test-file "../test/foof/errors.foo")
     (test-file "../test/foof/coroutines.foo")
     (test-file "../test/foof/coroutines2.foo"))
 (it "should support Actor Model"
     (test-file "../test/foof/uprocs.foo")
     (test-file "../test/foof/uprocs2.foo" sort-lines)
     (test-file "../test/foof/msgwait.foo") ;; FIXME Sometimes broken.
     (test-file "../test/foof/fibonacci2.foo")
     (test-file "../test/foof/errors2.foo"))
 (it "should support the RBS"
     (test-file "../test/foof/rbs2.foo")
     (test-file "../test/foof/rbs.foo") ;; FIXME Kinda broken.
     (test-file "../test/foof/cep.foo")))

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
