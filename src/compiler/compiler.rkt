#lang racket

;; The compiler
(require "env.rkt")
(require "pass.rkt")
(require "utils/utils.rkt")

;; The frontend
(require "passes/parser.rkt")
(require "passes/macro-expander.rkt")
(require "passes/qq.rkt")
(require "passes/const.rkt")
(require "passes/validate.rkt")
(require "passes/errors.rkt")

;; Optimizations
(require "passes/alpha-conversion.rkt")
(require "passes/optimize.rkt")
(require "passes/bindings.rkt")
(require "passes/freevars.rkt")
(require "passes/builtins.rkt")
(require "passes/lambdas.rkt")
(require "passes/copy-propagation.rkt")
(require "passes/const-propagation.rkt")
(require "passes/const-folding.rkt")
(require "passes/dce.rkt")
(require "passes/cse.rkt")
(require "passes/letrec-bindings.rkt")
(require "passes/letrec-fix.rkt")
(require "passes/cpc.rkt")

;; The backend
(require "passes/instrument.rkt")
(require "passes/closures.rkt")
(require "passes/globalization.rkt")
(require "passes/rename.rkt")
(require "passes/generator.rkt")

(provide compile add-defaults assemble-pipeline run-pipeline)

(define (add-defaults env)
  (env-set env
           ;; Compiler specific
           'errors '()
           'last-phase (env-get* env 'last-phase 'codegen)
           ;; Expander & compilation envs.
           'static-env (make-static-environment)
           'globals (make-global-definitions-list)
           ;; Parse transforms
           'instrument (env-get* env 'instrument id)
           ;; Optimization
           'optimizer (env-get* env 'optimizer 'naive)
           'optimization-level (env-get* env 'optimization-level 2)
           ;; Code generation
           'target (env-get* env 'target 'r7rs)))

(define (full-pipeline opts-early opts-late opts-final)
  (list parse
        'parse
        macro-expand
        quasiquote-expand
        'expand
        annotate-constants
        annotate-free-vars
        annotate-bindings
        validate
        report-errors
        alpha-convert
        'alpha
        (optimize opts-early)
        'optimize-early
        annotate-free-vars
        annotate-bindings
        reorder-letrec-bindings
        fix-letrec
        'letrec
        continuation-passing-convert
        'cps
        (optimize opts-late)
        'optimize-late
        instrument
        'instrument
        annotate-free-vars
        closure-convert
        'closures
        (optimize opts-final)
        'optimize-final
        globalize
        'hoist
        symbol-rename
        'rename
        generate-target-code
        'codegen))

(define (assemble-pipeline env)
  (let* ((opt-level (env-get env 'optimization-level))
         (opts-early (if (> opt-level 0)
                         (list (sequence annotate-free-vars
                                         inline-lambdas)
                               inline-builtins
                               propagate-constants
                               fold-constants
                               (sequence annotate-free-vars
                                         eliminate-common-subexpressions)
                               propagate-copies
                               (sequence annotate-free-vars
                                         eliminate-dead-code))
                         '()))
         (opts-late (if (> opt-level 1)
                        (list (sequence annotate-free-vars
                                        inline-lambdas)
                              propagate-constants
                              fold-constants
                              (sequence annotate-free-vars
                                        eliminate-common-subexpressions)
                              propagate-copies
                              (sequence annotate-free-vars
                                        eliminate-dead-code))
                        '()))
         (opts-final (if (> opt-level 2)
                         (list propagate-constants
                               fold-constants
                               (sequence annotate-free-vars
                                         eliminate-common-subexpressions)
                               propagate-copies
                               (sequence annotate-free-vars
                                         eliminate-dead-code))
                         '()))
         (pipeline (full-pipeline opts-early opts-late opts-final))
         (phase (env-get env 'last-phase)))
    (let loop ((p pipeline))
      (cond ((empty? p)
             '())
            ((equal? (car p) phase)
             '())
            ((symbol? (car p))
             (loop (cdr p)))
            (else
             (cons (car p)
                   (loop (cdr p))))))))

(define (run-pipeline pipeline env)
  (foldl run-pass env pipeline))

(define (compile env)
  (let ((e (add-defaults env)))
    (run-pipeline (assemble-pipeline e) e)))
