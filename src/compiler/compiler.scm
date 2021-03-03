;; Tha compiler.

(load "compiler/utils.scm")
(load "compiler/env.scm")
(load "compiler/tree-ast.scm")
(load "compiler/parser.scm")
(load "compiler/validate.scm")
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
               validate
               ast->plain
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

;; FIXME Ideally this is no longer needed.
(define (ast->plain expr)
  (map-ast id
           (lambda (expr)
             (case (ast-get expr 'type 'undefined)
               ('plain-quote (list 'quote (ast-get expr 'value '())))
               ('quasiquote (list 'quasiquote (ast-get expr 'value '())))
               ('unquote (list 'unquote (ast-get expr 'value '())))
               ('unquote-splicing (list 'unquote-splicing (ast-get expr 'value '())))
               (else (ast-get expr 'value '()))))
           (env-get expr 'ast)))

(define (lint expr)
  ;; TODO Lint the code
  expr)

(define (optimize expr)
  ;; TOOD Optimize redundant bindings etc.
  expr)

(define (generate expr)
  ;; TODO Generate target-specific code.
  expr)
