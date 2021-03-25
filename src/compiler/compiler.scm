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
(load "compiler/letrec.scm")
(load "compiler/anormal.scm")
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
               validate
               report-errors
               letrec-expand
               adapt-ast
               (flip normalize (make-identity-continuation))
               (flip cpc (make-identity-continuation))
               (flip closure-convert (make-global-definitions-list))
               mangle)))

;; FIXME This should be removed once all the phases use the new AST.
(define (adapt-ast env)
  (ast->plain (env-get env 'ast)))
