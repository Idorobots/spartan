#lang racket

;; The compiler

(require "env.rkt")
(require "pass.rkt")

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
(require "passes/generator.rkt")
(require "passes/closures.rkt")
(require "passes/rename.rkt")

(provide compile)

(define (compile env)
  (foldl run-pass
         (env-set env
                  'errors '()
                  'macros (make-builtin-macros)
                  'globals (make-global-definitions-list))
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
               (optimize
                (list annotate-free-vars
                      inline-lambdas
                      annotate-free-vars
                      inline-builtins
                      propagate-constants
                      fold-constants
                      eliminate-common-subexpressions
                      propagate-copies
                      eliminate-dead-code))
               annotate-free-vars
               annotate-bindings
               reorder-letrec-bindings
               fix-letrec
               continuation-passing-convert
               (optimize
                (list annotate-free-vars
                      inline-lambdas
                      annotate-free-vars
                      propagate-constants
                      fold-constants
                      eliminate-common-subexpressions
                      propagate-copies
                      eliminate-dead-code))
               annotate-free-vars
               closure-convert
               symbol-rename
               generate-target-code)))
