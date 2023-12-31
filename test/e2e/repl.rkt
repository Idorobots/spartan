#lang racket

;; REPL tests.

(require "../testing.rkt")
(require "../../src/compiler/utils/io.rkt")

(define +compiler-bin+ "./build/bin/sprtn")

(define (run-repl filename)
  (let* ((args "repl")
         (input (open-input-file filename)))
    (let-values (((sp out in err) (subprocess #f input #f +compiler-bin+ args)))
      (displayln (format "Running repl input file ~a with: ~a ~a" filename +compiler-bin+ args))
      (subprocess-wait sp)
      (let ((status (subprocess-status sp))
            (output (port->string out))
            (errors (port->string err)))
        (close-input-port input)
        (close-input-port out)
        (close-input-port err)
        (displayln (format "Result: ~a\nOutput: ~a\nErrors: ~a" status output errors))
        (assert return-status-equal? status 0)
        (if (equal? errors "")
            output
            errors)))))

(describe
 "REPL"

 (it "should support commands"
     (run-with-snapshot run-repl "test/data/repl/quit.txt")
     (run-with-snapshot run-repl "test/data/repl/settings.txt")
     (run-with-snapshot run-repl "test/data/repl/colors.txt"))

 (it "should support standard interactive mode"
     (run-with-snapshot run-repl "test/data/repl/interactive.txt")
     (run-with-snapshot run-repl "test/data/repl/multiline.txt"))

 (it "should allow line editing"
     (run-with-snapshot run-repl "test/data/repl/lineediting.txt")
     (run-with-snapshot run-repl "test/data/repl/straydefine.txt"))

 (it "should print errors nicely"
     (run-with-snapshot run-repl "test/data/repl/errors.txt")
     (run-with-snapshot run-repl "test/data/repl/errorsonrun.txt"))

 (it "should display debug compilation output"
     (run-with-snapshot run-repl "test/data/repl/debug.txt")))
