#lang racket

;; Metadata handling

(require "../utils/utils.rkt")
(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require "../errors.rkt")

(provide extract-metadata)

(define extract-metadata
  (pass (schema "extract-metadata"
                'ast (ast-subset? '(const symbol
                                          if do let letrec binding lambda app
                                          primop-app def <error>))
                'intrinsics a-list?)
        (lambda (env)
          (let* ((ast (env-get env 'ast))
                 (intrinsics (env-get env 'intrinsics))
                 (result (extract-intrinsics-metadata ast))
                 (primitives (append intrinsics (car result)))
                 (updated (cdr result)))
            (env-set env
                     'ast updated
                     'intrinsics primitives)))))

(define (extract-intrinsics-metadata ast)
  (define (replace-with-nil expr)
    (let ((loc (ast-node-location expr)))
      (replace expr
               (generated
                (make-ast-symbol loc 'nil)))))

  (let* ((primitives '())
         (updated (map-ast (lambda (expr)
                             (match-ast expr
                              ;; NOTE Extract intrinsic metadata.
                              ;; FIXME This would be better served with a dedicated metadata AST node.
                              ((primop-app '&primitive-metadata (const (list meta ...)))
                               #:when (every? ast-symbol? meta)
                               (set! primitives
                                     (cons (map ast-symbol-value meta)
                                           primitives))
                               (replace-with-nil expr))

                              ((primop-app '&primitive-metadata args ...)
                               (raise-compilation-error
                                expr
                                "Invalid metadata specification:"))

                              (else
                               expr)))
                           ast)))
    (cons primitives updated)))
