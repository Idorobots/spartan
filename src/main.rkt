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

(define *primitives* '())

(define (make-intrinsics-list)
  *primitives*)

(define *global-definitions* '(list yield))

(define (make-global-definitions-list)
  (apply set *global-definitions*))

(define (extract-intrinsics-metadata ast)
  (define (replace-with-nil expr)
    (let ((loc (ast-node-location expr)))
      (replace expr
               (generated
                (make-ast-symbol loc 'nil)))))

  (map-ast (lambda (expr)
             (match-ast expr
              ;; NOTE Extract intrinsic metadata.
              ((primop-app '&primitive-metadata (const (list meta ...)))
               #:when (every? ast-symbol? meta)
               (set! *primitives*
                     (cons (map ast-symbol-value meta)
                           *primitives*))
               (replace-with-nil expr))

              ((primop-app '&primitive-metadata args ...)
               ;; NOTE Should be a compilation error, but since this is a built-in module it should always work.
               (replace-with-nil expr))

              (else
               expr)))
           ast))

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
                    'instrument (compose extract-global-definitions
                                         extract-intrinsics-metadata)
                    'last-phase 'instrument))))
    (compile (env-set init
                      'first-phase 'instrument
                      'last-phase 'codegen
                      ;; FIXME Needs the full globals and  intrinsics lists for the optimizations do do anything.
                      'globals (make-global-definitions-list)
                      'intrinsics (make-intrinsics-list)))))

(define *core-import* #f)

(define (import-defaults! rt)
  ;; FIXME Currently the rt.rkt namespace is reused as the evaluation namespace.
  ;; FIXME This should be done for each new runtime instance.
  (let ((should-import? (or (not +use-global-namespace+)
                            (not *core-import*))))
    (unless *core-import*
      (set! *core-import* (rt-execute-no-init! rt +core-spartan+)))
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
  (run-code
   (compile
    (env 'module "expr"
         'input (with-output-to-string
                  (lambda ()
                    (pretty-write expr)))
         'intrinsics (make-intrinsics-list)
         'globals (make-global-definitions-list)
         'instrument instrument))))

(define (run-string input)
  (run-instrumented-string input id))

(define (run-instrumented-string input instrument)
  (run-code
   (compile-instrumented-string input instrument)))

(define (run-file filename)
  (run-instrumented-file filename id))

(define (run-instrumented-file filename instrument)
  (run-code
   (compile-instrumented-file filename instrument)))
