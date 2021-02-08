;; Tha compiler.

(load "compiler/utils.scm")
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
               ;; FIXME This should ideally run after macro-expansion, but it breaks even?/odd? example
               ;; FIXME when both functions are called within the body. Must be a difference when handling
               ;; FIXME lambda's and let's in some other phase.
               letrec-expand
               (flip macro-expand (make-builtin-macros))
               (flip normalize (make-identity-continuation))
               (flip cpc (make-identity-continuation))
               (flip closure-convert (make-global-environment))
               optimize
               (flip mangle (make-global-environment))
               generate)))

(define (validate expr)
  ;; TODO Validate the AST.
  expr)

(define (optimize expr)
  ;; TOOD Optimize redundant bindings etc.
  expr)

(define (generate expr)
  ;; TODO Generate target-specific code.
  expr)
