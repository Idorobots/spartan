;; Compiler pass definition

(load "compiler/errors.scm")
(load "compiler/ast.scm")

(define (pass schema transform)
  (cons schema transform))

(define (pass-schema pass)
  (car pass))

(define (pass-transform pass)
  (cdr pass))

(define (schema . properties)
  (lambda (env)
    (hash-map (apply hasheq properties)
              (lambda (key schema)
                (if (hash-has-key? env key)
                    (schema (hash-ref env key))
                    (schema-validation-error (format "missing required field `~a`" key) env))))))

(define (non-empty-string? val)
  (unless (and (string? val)
               (> (string-length val) 0))
    (schema-validation-error "not a non-empty string" val)))

(define (non-empty-list? val)
  (unless (and (list? val)
               (> (length val) 0))
    (schema-validation-error "not a non-empty list" val)))

(define (a-list? val)
  (unless (list? val)
    (schema-validation-error "not a list" val)))

(define (ast-subset? types)
  (lambda (expr)
    (map-ast id
             (lambda (expr)
               (let ((t (get-type expr)))
                 (unless (member t types)
                   (schema-validation-error (format "unhandled AST node type `~a`" t) expr)))
               expr)
             expr)))

(define (schema-validation-error hint value)
  (compiler-bug (format "Schema validation failed: ~a" hint) value))
