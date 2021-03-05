;; Tha compiler.

(load "compiler/utils.scm")
(load "compiler/env.scm")
(load "compiler/tree-ast.scm")
(load "compiler/parser.scm")
(load "compiler/errors.scm")
(load "compiler/syntax-elaboration.scm")
(load "compiler/syntax.scm")
(load "compiler/macro-expander.scm")
(load "compiler/letrec.scm")
(load "compiler/anormal.scm")
(load "compiler/cpc.scm")
(load "compiler/closures.scm")
(load "compiler/rename.scm")

(define (compile env)
  (foldl (lambda (phase expr)
           (phase expr))
         env
         (list parse
               elaborate-syntax
               report-errors
               adapt-ast
               syntax-expand
               (flip macro-expand (make-builtin-macros))
               lint
               letrec-expand
               (flip normalize (make-identity-continuation))
               (flip cpc (make-identity-continuation))
               (flip closure-convert (make-internal-applicatives))
               optimize
               (flip mangle (make-internal-applicatives))
               generate)))

;; FIXME This should be removed once all the phases use the new AST.
(define (adapt-ast env)
  (ast->plain (env-get env 'ast)))

(define (lint expr)
  ;; TODO Lint the code
  expr)

(define (optimize expr)
  ;; TOOD Optimize redundant bindings etc.
  expr)

(define (generate expr)
  ;; TODO Generate target-specific code.
  expr)
