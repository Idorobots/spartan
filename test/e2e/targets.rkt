#lang racket

;; Actual compiler tests for different targets.

(require "../testing.rkt")
(require "../../src/compiler/utils/io.rkt")

(define +compiler-bin+ "./build/bin/sprtn")
(define +js-bin+ (path->string (find-executable-path "node")))

(define (return-status-equal? a b)
  (equal? a b))

(define (make-r7rs-runner opts)
  (lambda (f)
    (let ((args (format "run --target r7rs -i ~a ~a" f opts)))
      (let-values (((sp out in err) (subprocess #f #f #f +compiler-bin+ args)))
        (displayln (format "Running file ~a with: ~a ~a" f +compiler-bin+ args))
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
              errors))))))

(define (make-es6-runner opts)
  (lambda (f)
    (let* ((tmp (path->string (make-temporary-file "~a.js")))
           (args (format "compile --target ES6 -i ~a -o ~a ~a" f tmp opts))
           (js-args tmp))
      (displayln (format "Compiling file ~a with: ~a ~a" f +compiler-bin+ args))
      (let-values (((sp out in err)
                    (subprocess #f #f #f +compiler-bin+ args)))
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
              (begin (displayln (format "Running file ~a with: ~a ~a" tmp +js-bin+ js-args))
                     (let-values (((sp out in err)
                                   (subprocess #f #f #f +js-bin+ js-args)))
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
                             errors))))
            errors))))))

(parameterized
 ;; Examples supported by all targets.
 ((target "es6" "O0 es6" "03 naive es6" "O3 super es6"
          "r7rs" "O0 r7rs" "O3 naive r7rs" "O3 super r7rs")
  (run (make-es6-runner "")
       (make-es6-runner "-O0")
       (make-es6-runner "-O3 --optimizer naive")
       (make-es6-runner "-O3 --optimizer super")
       (make-r7rs-runner "")
       (make-r7rs-runner "-O0")
       (make-r7rs-runner "-O3 --optimizer naive")
       (make-r7rs-runner "-O3 --optimizer super")))

 (describe
  (format "~a target" target)

  (it "should support basic language features"
      (run-with-snapshot run "examples/hello.sprtn")
      (run-with-snapshot run "examples/math.sprtn")
      (run-with-snapshot run "examples/letrec.sprtn")
      (run-with-snapshot run "examples/fibonacci.sprtn")
      (run-with-snapshot run "examples/logger.sprtn"))

  (it "should support continuations"
      (run-with-snapshot run "examples/coroutines.sprtn"))))

(parameterized
 ;; Examples supported by R7RS.
 ((target "r7rs" "O0 r7rs" "O3 naive r7rs" "O3 super r7rs")
  (run (make-r7rs-runner "")
       (make-r7rs-runner "-O0")
       (make-r7rs-runner "-O3 --optimizer naive")
       (make-r7rs-runner "-O3 --optimizer super")))

 (describe
  (format "~a target" target)

  (it "should support basic language features"
      (run-with-snapshot run "examples/fact.sprtn")
      (run-with-snapshot run "examples/rsa.sprtn"))

  (it "should support continuations"
      (run-with-snapshot run "examples/continuations.sprtn" sort-lines)
      (run-with-snapshot run "examples/coroutines2.sprtn")
      (run-with-snapshot run "examples/coroutines3.sprtn")
      (run-with-snapshot run "examples/amb.sprtn"))

  (it "should support Actor Model"
      (run-with-snapshot run "examples/msgwait.sprtn"))

  (it "should support the RBS"
      (run-with-snapshot run "examples/rbs2.sprtn"))))
