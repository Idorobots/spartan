;; Tha compiler.

(load "compiler/utils.scm")
(load "compiler/lint.scm")
(load "compiler/syntax.scm")
(load "compiler/macro-expander.scm")
(load "compiler/letrec.scm")
(load "compiler/anormal.scm")
(load "compiler/cpc.scm")
(load "compiler/closures.scm")
(load "compiler/rename.scm")

(define (compile expr)
  (foldl (lambda (phase expr)
           (phase expr))
         expr
         (list validate
               syntax-expand
               (flip macro-expand (make-builtin-macros))
               letrec-expand
               (flip normalize (make-identity-continuation))
               (flip cpc (make-identity-continuation))
               (flip closure-convert (make-internal-applicatives))
               optimize
               (flip mangle (make-internal-applicatives))
               generate)))

(define (optimize expr)
  ;; TOOD Optimize redundant bindings etc.
  expr)

(define (generate expr)
  ;; TODO Generate target-specific code.
  expr)
