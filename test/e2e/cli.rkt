#lang racket

;; REPL tests.

(require "../testing.rkt")
(require "../../src/compiler/utils/io.rkt")

(define +compiler-bin+ "./build/bin/sprtn")

(define (run-compiler args)
  (lambda (filename)
    (let-values (((sp out in err) (subprocess #f #f #f +compiler-bin+ (string-append args " " filename))))
      (displayln (format "Running compilation with: ~a ~a ~a" +compiler-bin+ args filename))
      (subprocess-wait sp)
      (let ((status (subprocess-status sp))
            (output (port->string out))
            (errors (port->string err)))
        (close-output-port in)
        (close-input-port out)
        (close-input-port err)
        (displayln (format "Result: ~a\nOutput: ~a\nErrors: ~a" status output errors))
        (assert return-status-equal? status 0)
        (if (equal? errors "")
            output
            errors)))))

(describe
 "CLI"

 (it "should output a given phase output"
     (run-with-snapshot (run-compiler "compile --phase optimize-early -i") "test/data/compiler/tight-inline1.sprtn")
     (run-with-snapshot (run-compiler "compile --phase optimize-late -i") "test/data/compiler/tight-inline2.sprtn")
     (run-with-snapshot (run-compiler "compile --phase codegen -i") "test/data/compiler/tight-inline3.sprtn")))
