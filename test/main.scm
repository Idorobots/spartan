;; Top level for tests.

(require "testing.rkt")

(define *imported-modules* '())

(define-syntax load-once
  (syntax-rules (*imported-modules*)
    ((load-once file)
     (unless (member file *imported-modules*)
       (set! *imported-modules* (cons file *imported-modules*))
       (load file)))))

;; Compiler unit tests:
(load-once "../test/compiler/utils.scm")
(load-once "../test/compiler/freevars.scm")
(load-once "../test/compiler/substitute.scm")
(load-once "../test/compiler/propagate.scm")
(load-once "../test/compiler/peggen.scm")
(load-once "../test/compiler/ast.scm")
(load-once "../test/compiler/pass.scm")
(load-once "../test/compiler/parser.scm")
(load-once "../test/compiler/errors.scm")
(load-once "../test/compiler/macros.scm")
(load-once "../test/compiler/elaboration.scm")
(load-once "../test/compiler/body.scm")
(load-once "../test/compiler/qq.scm")
(load-once "../test/compiler/const.scm")
(load-once "../test/compiler/validate.scm")
(load-once "../test/compiler/bindings.scm")
(load-once "../test/compiler/dce.scm")
(load-once "../test/compiler/cse.scm")
(load-once "../test/compiler/copy-propagation.scm")
(load-once "../test/compiler/const-propagation.scm")
(load-once "../test/compiler/const-folding.scm")
(load-once "../test/compiler/letrec-bindings.scm")
(load-once "../test/compiler/letrec-fix.scm")
(load-once "../test/compiler/builtins.scm")
(load-once "../test/compiler/lambdas.scm")
(load-once "../test/compiler/cpc.scm")
(load-once "../test/compiler/closures.scm")
(load-once "../test/compiler/rename.scm")
(load-once "../test/compiler/compiler.scm")

;; Runtime unit tests:
(require "rt/queue.rkt")
(require "rt/scheduler.rkt")
(require "rt/continuations.rkt")
(require "rt/exception.rkt")
(require "rt/actor.rkt")

;; Some integration tests:
(load-once "../test/recurse.scm")
(load-once "../test/modules.scm")
(load-once "../test/integration.scm")

;; Performance tests:
(load-once "../test/performance.scm")

(run-all-tests)
