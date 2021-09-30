#lang racket

;; The compiler

(require "env.rkt")
(require "pass.rkt")
(require "utils/utils.rkt")

;; The frontend
(require "passes/parser.rkt")
(require "passes/macro-expander.rkt")
(require "passes/elaboration.rkt")
(require "passes/body.rkt")
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

(provide compile)

(define (compile env)
  (foldl run-pass
         (env-set env
                  'errors '()
                  'macros (make-builtin-macros)
                  'globals (make-global-definitions-list)
                  'instrument (env-get* env 'instrument id)
                  'optimize (env-get* env 'optimize optimize-naive))
         (list parse
               macro-expand
               elaborate
               body-expand
               quasiquote-expand
               annotate-constants
               annotate-free-vars
               annotate-bindings
               validate
               report-errors
               alpha-convert
               (optimize
                (list (sequence annotate-free-vars
                               inline-lambdas)
                      inline-builtins
                      propagate-constants
                      fold-constants
                      (sequence annotate-free-vars
                                eliminate-common-subexpressions)
                      propagate-copies
                      (sequence annotate-free-vars
                                eliminate-dead-code)))
               annotate-free-vars
               annotate-bindings
               reorder-letrec-bindings
               fix-letrec
               continuation-passing-convert
               (optimize
                (list (sequence annotate-free-vars
                               inline-lambdas)
                      propagate-constants
                      fold-constants
                      (sequence annotate-free-vars
                                eliminate-common-subexpressions)
                      propagate-copies
                      (sequence annotate-free-vars
                                eliminate-dead-code)))
               instrument
               annotate-free-vars
               closure-convert
               globalize
               symbol-rename
               generate-target-code)))
