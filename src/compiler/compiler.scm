;; Tha compiler.

(load "compiler/utils.scm")
(load "compiler/env.scm")
(load "compiler/tree-ast.scm")
(load "compiler/parser.scm")
(load "compiler/errors.scm")
(load "compiler/elaboration.scm")
(load "compiler/qq.scm")
(load "compiler/macro-expander.scm")
(load "compiler/letrec.scm")
(load "compiler/anormal.scm")
(load "compiler/cpc.scm")
(load "compiler/closures.scm")
(load "compiler/rename.scm")

(define (compile env)
  (foldl (lambda (phase expr)
           (phase expr))
         (env-set env
                  'errors '())
         (list parse
               elaborate
               quasiquote-expand
               report-errors
               adapt-ast
               (flip macro-expand (make-builtin-macros))
               letrec-expand
               (flip normalize (make-identity-continuation))
               (flip cpc (make-identity-continuation))
               (flip closure-convert (make-internal-applicatives))
               (flip mangle (make-internal-applicatives)))))

;; FIXME This should be removed once all the phases use the new AST.
(define (adapt-ast env)
  (ast->plain (env-get env 'ast)))
