;; Top level for tests.

(load "../test/utils.scm")

;; Compiler unit tests:
(load "../test/compiler/utils.scm")
(load "../test/compiler/freevars.scm")
(load "../test/compiler/substitute.scm")
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

;; (test-file "../test/foof/cep.foo")
(test-file "../test/foof/coroutines.foo")
(test-file "../test/foof/coroutines2.foo")
;; (test-file "../test/foof/errors.foo")
;; (test-file "../test/foof/errors2.foo")
(test-file "../test/foof/fibonacci.foo")
;; (test-file "../test/foof/fibonacci2.foo")
;; (test-file "../test/foof/hello.foo")
;; (test-file "../test/foof/logger.foo")
(test-file "../test/foof/msgwait.foo")
;; (test-file "../test/foof/rbs.foo")
(test-file "../test/foof/rbs2.foo")
;; (test-file "../test/foof/uprocs.foo")
;; (test-file "../test/foof/uprocs2.foo")
