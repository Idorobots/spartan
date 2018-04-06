;; Tha compiler.

(load "compiler/closures.scm")
(load "compiler/cpc.scm")
(load "compiler/macro-expander.scm")
(load "compiler/utils.scm")

(define (compile expr)
  (foldl (lambda (phase expr)
           (phase expr))
         expr
         (list preprocess
               (flip macro-expand (make-builtin-macros))
               (flip cpc (make-identity-continuation))
               (flip closure-convert (make-global-environment))
               optimize
               generate)))

(define (preprocess expr)
  ;; TODO Pre-process the expression.
  expr)

(define (optimize expr)
  ;; TOOD Optimize redundant bindings etc.
  expr)

(define (generate expr)
  ;; TODO Generate target-specific code.
  expr)
