#lang racket

;; Implicit body handling.

(require "../utils/utils.rkt")
(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require "../errors.rkt")

(require (only-in "../expander/elaborators.rkt"
                  unique-bindings))

(provide body-expand
         ;; FIXME For test access.
         expand-body)

(define body-expand
  (pass (schema "body-expand"
                'errors a-list?
                'ast (ast-subset? '(quote quasiquote unquote unquote-splicing
                                    number symbol string list
                                    if do let letrec binding lambda app def
                                    primop-app body <error>)))
        (lambda (env)
          (let ((result (collect-errors (env-get env 'errors)
                                        (lambda ()
                                          (expand-body (env-get env 'ast))))))
            (env-set env
                     'ast (car result)
                     'errors (cadr result))))))

(define (expand-body expr)
  (match-ast expr
   ((ast-body exprs ...)
    (let* ((defs (extract-defs exprs))
           (non-defs (extract-non-defs exprs)))
      (if (> (length defs) 0)
          (generated
           (make-ast-letrec (ast-node-location expr)
                            (unique-bindings (map expand-body defs)
                                             (ast-node-context expr))
                            (reconstruct-simple-body
                             (map expand-body non-defs)
                             expr)))
          (reconstruct-simple-body
           (map expand-body exprs)
           expr))))
   ((def name value)
    (raise-compilation-error
     ;; NOTE So that we might find more meaningful errors in the future passes.
     (walk-ast expand-body expr)
     (format "~a, not allowed in this context:" (ast-node-context* expr "Bad `define` syntax"))))
   (else
    (walk-ast expand-body expr))))

(define (extract-defs exprs)
  (foldr (lambda (e acc)
           (match-ast e
            ((def name value)
             (cons (generated
                    (make-ast-binding (ast-node-location e) name value))
                   acc))
            (else acc)))
         '()
         exprs))

(define (extract-non-defs exprs)
  (filter (compose not ast-def?)
          exprs))

(define (reconstruct-simple-body exprs parent)
  (let ((ctx (ast-node-context* parent "Bad `do` syntax")))
    (cond ((= (length exprs) 0)
           (raise-compilation-error
            parent
            (format "~a, expected at least one non-definition expression within:" ctx)))
          ((= (length exprs) 1)
           (car exprs))
          (else
           (generated
            ;; NOTE The context should be preserved.
            (set-ast-node-context
             (make-ast-do (ast-node-location parent)
                          exprs)
             ctx))))))
