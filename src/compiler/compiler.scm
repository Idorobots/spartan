;; The compiler

(load "compiler/env.scm")
(load "compiler/tree-ast.scm")

;; The frontend
(load "compiler/parser.scm")
(load "compiler/macro-expander.scm")
(load "compiler/elaboration.scm")
(load "compiler/body.scm")
(load "compiler/qq.scm")
(load "compiler/validate.scm")
(load "compiler/errors.scm")

;; The backend
(load "compiler/bindings.scm")
(load "compiler/freevars.scm")
(load "compiler/letrec.scm")
(load "compiler/cpc.scm")
(load "compiler/closures.scm")
(load "compiler/rename.scm")

(define (compile env)
  (foldl (lambda (phase expr)
           (phase expr))
         (env-set env
                  'errors '()
                  'macros (make-builtin-macros)
                  'globals (make-global-definitions-list))
         (list parse
               macro-expand
               elaborate
               body-expand
               quasiquote-expand
               annotate-free-vars
               annotate-bindings
               validate
               letrec-expand
               report-errors
               continuation-passing-convert
               annotate-free-vars
               annotate-bindings
               closure-convert
               adapt-ast
               mangle)))

;; FIXME This should be removed once all the phases use the new AST.
(define (adapt-ast env)
  (ast->plain (env-get env 'ast)))
