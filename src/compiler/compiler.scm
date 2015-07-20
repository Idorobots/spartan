;; Tha compiler.

(load "compiler/cpc.scm")
(load "compiler/macro-expander.scm")
(load "compiler/utils.scm")

(define (compile expr)
  (optimize (cpc (macro-expand (preprocess expr)
                               (make-builtin-macros))
                 (make-identity-continuation))))

(define (optimize expr)
  ;; TOOD optimize redundant bindings etc
  (lambda-lift (closure-convert expr
                                (make-empty-environment))))

(define (lambda-lift expr)
  ;; TODO actually implement this
  expr)

(define (closure-convert expr env)
  ;;TODO actually implement this
  expr)

(define (preprocess expr)
  ;; TODO pre-process the expression
  expr)

(define (make-empty-environment)
  '())

(define (make-identity-continuation)
  id)
