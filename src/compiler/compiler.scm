;; Tha compiler.

(load "compiler/closures.scm")
(load "compiler/cpc.scm")
(load "compiler/macro-expander.scm")
(load "compiler/utils.scm")

(define (compile expr)
  (optimize (cpc (macro-expand (preprocess expr)
                               (make-builtin-macros))
                 (make-identity-continuation))))

(define (optimize expr)
  ;; TOOD optimize redundant bindings etc
  (closure-convert expr
                   (make-empty-environment)))

(define (make-empty-environment)
  '())

(define (make-identity-continuation)
  id)

(define (preprocess expr)
  ;; TODO pre-process the expression
  expr)
