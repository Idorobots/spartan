;; Tha compiler.

(load "compiler/utils.scm")
(load "compiler/syntax.scm")
(load "compiler/macro-expander.scm")
(load "compiler/cpc.scm")
(load "compiler/closures.scm")

(define (compile expr)
  (foldl (lambda (phase expr)
           (phase expr))
         expr
         (list syntax-expand
               (flip macro-expand (make-builtin-macros))
               (flip cpc (make-identity-continuation))
               (flip closure-convert (make-global-environment))
               optimize
               generate)))

(define (optimize expr)
  ;; TOOD Optimize redundant bindings etc.
  expr)

(define (generate expr)
  ;; TODO Generate target-specific code.
  expr)
