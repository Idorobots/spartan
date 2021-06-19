#lang racket

;; Top level for tests.

(require "testing.rkt")

;; Compiler unit tests:
(require "compiler/utils.rkt")
(require "compiler/freevars.rkt")
(require "compiler/substitute.rkt")
(require "compiler/propagate.rkt")
(require "compiler/peggen.rkt")
(require "compiler/ast.rkt")
(require "compiler/pass.rkt")
(require "compiler/parser.rkt")
(require "compiler/errors.rkt")
(require "compiler/macros.rkt")
(require "compiler/elaboration.rkt")
(require "compiler/body.rkt")
(require "compiler/qq.rkt")
(require "compiler/const.rkt")
(require "compiler/validate.rkt")
(require "compiler/bindings.rkt")
(require "compiler/dce.rkt")
(require "compiler/cse.rkt")
(require "compiler/copy-propagation.rkt")
(require "compiler/const-propagation.rkt")
(require "compiler/const-folding.rkt")
(require "compiler/letrec-bindings.rkt")
(require "compiler/letrec-fix.rkt")
(require "compiler/builtins.rkt")
(require "compiler/lambdas.rkt")
(require "compiler/cpc.rkt")
(require "compiler/closures.rkt")
(require "compiler/rename.rkt")
(require "compiler/compiler.rkt")

;; Runtime unit tests:
(require "rt/queue.rkt")
(require "rt/scheduler.rkt")
(require "rt/continuations.rkt")
(require "rt/exception.rkt")
(require "rt/actor.rkt")

;; Some integration tests:
(require "recurse.rkt")
(require "modules.rkt")
(require "integration.rkt")

;; Performance tests:
(require "performance.rkt")

;; Actual test running.
;; NOTE This is required for the code fragment evaluation.
(require "../src/runtime/rt.rkt")
(provide (all-from-out "../src/runtime/rt.rkt"))
(provide (all-from-out "integration.rkt"))

;; NOTE These ones for the parser generator tests.
(require "../src/compiler/peggen.rkt")
(provide (all-from-out "../src/compiler/peggen.rkt"))
(provide (all-from-out "compiler/peggen.rkt"))

(provide run-all-tests)