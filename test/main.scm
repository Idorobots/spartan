;; Top level for tests.

(load "../test/testing.scm")
(load "../test/gen.scm")

;; Compiler unit tests:
(load "../test/compiler/utils.scm")
(load "../test/compiler/freevars.scm")
(load "../test/compiler/substitute.scm")
(load "../test/compiler/peggen.scm")
(load "../test/compiler/tree-ast.scm")
(load "../test/compiler/parser.scm")
(load "../test/compiler/errors.scm")
(load "../test/compiler/macros.scm")
(load "../test/compiler/elaboration.scm")
(load "../test/compiler/body.scm")
(load "../test/compiler/qq.scm")
(load "../test/compiler/ast.scm")
(load "../test/compiler/validate.scm")
(load "../test/compiler/bindings.scm")
(load "../test/compiler/letrec.scm")
(load "../test/compiler/cpc.scm")
(load "../test/compiler/closures.scm")
(load "../test/compiler/rename.scm")
(load "../test/compiler/compiler.scm")

;; Runtime unit tests:
(load "../test/rt/queue.scm")
(load "../test/rt/scheduler.scm")
(load "../test/rt/continuations.scm")
(load "../test/rt/exception.scm")
(load "../test/rt/actor.scm")

;; Some integration tests:
(load "../test/recurse.scm")
(load "../test/modules.scm")
(load "../test/integration.scm")

;; Performance tests:
(load "../test/performance.scm")

(run-all-tests)
