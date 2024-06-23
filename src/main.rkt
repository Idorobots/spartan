#lang racket

;; The main entry point.

(require "compiler/utils/assets.rkt")
(require "compiler/utils/utils.rkt")
(require "compiler/utils/io.rkt")
(require "compiler/env.rkt")
(require "compiler/compiler.rkt")
(require "runtime/rt.rkt")

(provide run-code
         run
         run-instrumented
         run-string
         run-instrumented-string
         run-file
         run-instrumented-file
         compile-string
         compile-instrumented-string
         compile-file
         compile-instrumented-file
         ;; FIXME For test access
         import-defaults!)

(define (run-code expr)
  (let* ((rt (bootstrap-rt!)))
      (import-defaults! rt)
      (rt-execute! rt expr)))

(define +core-import+ #f)
(define (import-defaults! rt)
  (unless +core-import+
    (set! +core-import+ (rt-execute-no-init! rt +core-spartan+)))
  (rt-import! rt
              +core-import+))

(define (run-instrumented expr instrument)
  (run-code
   (compile
    (env 'input (with-output-to-string
                  (lambda ()
                    (pretty-write expr)))
         'module "expr"
         'instrument instrument))))

(define (run expr)
  (run-instrumented expr id))

(define (compile-instrumented-string input instrument)
  (compile
    (env 'input input
         'module "string"
         'instrument instrument)))

(define (compile-string input)
  (compile-instrumented-string input id))

(define (run-instrumented-string input instrument)
  (run-code
   (compile-instrumented-string input instrument)))

(define (run-string input)
  (run-instrumented-string input id))

(define (compile-instrumented-file filename instrument)
  (compile
    (env 'input (slurp filename)
         'module filename
         'instrument instrument)))

(define (compile-file filename)
  (compile-instrumented-file filename id))

(define (run-instrumented-file filename instrument)
  (run-code
   (compile-instrumented-file filename instrument)))

(define (run-file filename)
  (run-instrumented-file filename id))

;; Default modules to be included in the binary.

(define +core-spartan+ (compile-string (embed-file-contents "./runtime/core.sprtn")))
