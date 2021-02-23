;; Top level for tests.

(load "../test/utils.scm")

;; Compiler unit tests:
(load "../test/compiler/utils.scm")
(load "../test/compiler/freevars.scm")
(load "../test/compiler/substitute.scm")
(load "../test/compiler/peggen.scm")
(load "../test/compiler/parser.scm")
(load "../test/compiler/ast.scm")
(load "../test/compiler/syntax.scm")
(load "../test/compiler/macros.scm")
(load "../test/compiler/letrec.scm")
(load "../test/compiler/anormal.scm")
(load "../test/compiler/cpc.scm")
(load "../test/compiler/closures.scm")
(load "../test/compiler/rename.scm")

;; Runtime unit tests:
(load "../test/rt/queue.scm")
(load "../test/rt/scheduler.scm")
(load "../test/rt/recurse.scm")
(load "../test/rt/continuations.scm")
(load "../test/rt/exception.scm")
(load "../test/rt/actor.scm")
(load "../test/rt/modules.scm")

;; Some integration tests:
(load "../test/integration.scm")

;; Performance tests:
(load "../test/performance.scm")
