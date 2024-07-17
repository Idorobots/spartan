#lang racket

;; The main entry point.

(require "compiler/utils/assets.rkt")
(require "compiler/utils/set.rkt")
(require "compiler/utils/utils.rkt")
(require "compiler/utils/io.rkt")
(require "compiler/ast.rkt")
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
         make-intrinsics-list
         make-global-definitions-list
         ;; FIXME For test access
         import-defaults!)

;; Core module bootstrap

(define *global-definitions* '(list yield))

(define (make-global-definitions-list)
  (apply set *global-definitions*))

(define (make-intrinsics-list)
  (env-get* +core-spartan+ 'intrinsics '()))

(define (extract-global-definitions ast)
  ;; FIXME Assumes that the module is turned into a large letrec expression
  (match-ast ast
    ((letrec bindings body)
     (map (lambda (b)
            (set! *global-definitions*
                  (cons (ast-symbol-value (ast-binding-var b))
                        *global-definitions*)))
          bindings)
     ast)
    (else
     ;; NOTE Should be a compilation error, but since this is a built-in module it should always work.
     ast)))

(define +core-spartan+
  ;; FIXME Replace with a pre-compiled module load.
  (let ((init (compile
               (env 'module "core"
                    'input (format "(structure ~a)" (embed-file-contents "./runtime/core.sprtn"))
                    ;; FIXME This still needs the two "built-in" global procedures.
                    'globals (make-global-definitions-list)
                    'instrument extract-global-definitions
                    'last-phase 'instrument))))
    (compile (env-set init
                      'first-phase 'instrument
                      'last-phase 'codegen
                      ;; FIXME Needs the full globals and intrinsics lists for the optimizations do do anything.
                      ;; FIXME These global functions are still added to the closure envs despite technically being global.
                      'globals (make-global-definitions-list)))))

(define *core-import* #f)

(define (import-defaults! rt)
  ;; FIXME Currently the rt.rkt namespace is reused as the evaluation namespace.
  ;; FIXME This should be done for each new runtime instance.
  (let ((should-import? (or (not +use-global-namespace+)
                            (not *core-import*))))
    (unless *core-import*
      (set! *core-import*
            (rt-execute-no-init! rt
                                 (env-get +core-spartan+ 'generated))))
    (when should-import?
      (rt-import! rt
                  *core-import*))))

(define (run-code expr)
  (let* ((rt (bootstrap-rt!)))
    (import-defaults! rt)
    (rt-execute! rt expr)))

;; Compilation

(define (compile-string input)
  (compile-instrumented-string input id))

(define (compile-instrumented-string input instrument)
  (compile
    (env 'module "string"
         'input input
         'intrinsics (make-intrinsics-list)
         'globals (make-global-definitions-list)
         'instrument instrument)))

(define (compile-file filename)
  (compile-instrumented-file filename id))

(define (compile-instrumented-file filename instrument)
  (compile
    (env 'module filename
         'input (slurp filename)
         'intrinsics (make-intrinsics-list)
         'globals (make-global-definitions-list)
         'instrument instrument)))

;; Execution

(define (run expr)
  (run-instrumented expr id))

(define (run-instrumented expr instrument)
  (-> (env 'module "expr"
           'input (with-output-to-string
                    (lambda ()
                      (pretty-write expr)))
           'intrinsics (make-intrinsics-list)
           'globals (make-global-definitions-list)
           'instrument instrument)
      (compile)
      (env-get 'generated)
      (run-code)))

(define (run-string input)
  (run-instrumented-string input id))

(define (run-instrumented-string input instrument)
  (-> input
      (compile-instrumented-string instrument)
      (env-get 'generated)
      (run-code)))

(define (run-file filename)
  (run-instrumented-file filename id))

(define (run-instrumented-file filename instrument)
  (-> filename
      (compile-instrumented-file instrument)
      (env-get 'generated)
      (run-code)))
