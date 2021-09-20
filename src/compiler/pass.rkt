#lang racket

;; Compiler pass definition

(require "utils/set.rkt")
(require "utils/refs.rkt")
(require "utils/utils.rkt")
(require "env.rkt")
(require "errors.rkt")
(require "ast.rkt")

(provide (struct-out pass) run-pass sequence debug
         schema non-empty-string? non-empty-list? non-empty-hash? a-symbol? a-pair? a-list? a-set? a-function? ast-subset? list-of?
         schema-validation-error?)

(struct pass (schema transform) #:transparent)

(define (run-pass pass env)
  (unless (env-contains? env 'no-validation)
    ((pass-schema pass) env))
  ((pass-transform pass) env))

(define (sequence . passes)
  (pass (schema "sequence")
        (lambda (env)
          (foldl run-pass
                 env
                 passes))))

(define (schema hint . properties)
  (lambda (env)
    (let ((errors (ref '())))
      (with-handlers ((schema-validation-error?
                       (lambda (e)
                         (assign! errors (cons (cadr e)
                                               (deref errors)))
                         ((caddr e) '()))))
        (hash-map (apply hasheq properties)
              (lambda (key schema)
                (if (hash-has-key? env key)
                    (schema (hash-ref env key))
                    (schema-validation-error (format "Missing required field `~a`" key) env)))))
      (unless (empty? (deref errors))
        (compiler-bug (format "Schema `~a` validation failed" hint)
                      (deref errors))))))

(define debug
  (pass (schema "debug")
        (lambda (env)
          (pretty-print (ast->plain (env-get env 'ast)))
          env)))

(define (non-empty-string? val)
  (unless (and (string? val)
               (> (string-length val) 0))
    (schema-validation-error "Not a non-empty string" val)))

(define (non-empty-list? val)
  (unless (and (list? val)
               (> (length val) 0))
    (schema-validation-error "Not a non-empty list" val)))

(define (non-empty-hash? val)
  (unless (and (hash? val)
               (> (hash-count val) 0))
    (schema-validation-error "Not a non-empty hash" val)))

(define (a-symbol? val)
  (unless (symbol? val)
    (schema-validation-error "Not a symbol" val)))

(define (a-pair? sub1 sub2)
  (lambda (val)
    (if (pair? val)
        (begin (sub1 (car val))
               (sub2 (cdr val)))
        (schema-validation-error "Not a pair" val))))

(define (a-list? val)
  (unless (list? val)
    (schema-validation-error "Not a list" val)))

(define (a-set? val)
  (unless (set? val)
    (schema-validation-error "Not a set" val)))

(define (a-function? val)
  (unless (procedure? val)
    (schema-validation-error "Not a procedure" val)))

(define (list-of? subschema)
  (lambda (vals)
    (if (list? vals)
        (map subschema vals)
        (schema-validation-error "Not a list" vals))))

(define (ast-subset? types)
  (lambda (expr)
    (let ((t (ast-node-type expr)))
      (unless (member t types)
        (schema-validation-error (format "Unhandled AST node type `~a`" t) expr))
      ;; NOTE Contents of these are technically not part of the subset.
      (if (or (ast-error? expr)
              (ast-const? expr))
          error
          (walk-ast (ast-subset? types)
                    expr)))))

(define (schema-validation-error? e)
  (tagged-list? 'schema-validation-error e))

(define (schema-validation-error hint value)
  (call/cc
   (lambda (cont)
     (raise (list 'schema-validation-error
                  (format "~a in: ~a" hint value)
                  cont)))))
