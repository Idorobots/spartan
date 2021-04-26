;; AST

(load-once "compiler/utils/set.scm")
(load-once "compiler/utils/utils.scm")

(load-once "compiler/ast/utils.scm")
(load "compiler/ast/nodes.scm") ;; FIXME Breaks the compiler if loaded once.
(load-once "compiler/ast/eqv.scm")
(load-once "compiler/ast/match.scm")

(load-once "compiler/errors.scm")
