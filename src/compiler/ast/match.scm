;; AST pattern matching

(load-once "compiler/ast/nodes.scm")
(load-once "compiler/ast/eqv.scm")

(load-once "compiler/errors.scm")

(define-syntax ast-case-extract-vars
  (syntax-rules (quote unquote get-var)
    ((ast-case-extract-vars bound body)
     body)
    ((ast-case-extract-vars bound body (unquote var) rest ...)
     (let ((var (get-var bound 'var)))
       (ast-case-extract-vars bound body rest ...)))
    ((ast-case-extract-vars bound body (quote symbol) rest ...)
     (ast-case-extract-vars bound body rest ...))
    ((ast-case-extract-vars bound body (parts ...) rest ...)
     (ast-case-extract-vars bound body parts ... rest ...))
    ((ast-case-extract-vars bound body unquote var rest ...)
     (let ((var (get-var bound 'var)))
       (ast-case-extract-vars bound body rest ...)))
    ((ast-case-extract-vars bound body symbol rest ...)
     (ast-case-extract-vars bound body rest ...))))

(define-syntax ast-case-match-rule
  (syntax-rules (ast-matches?)
    ((ast-case-match-rule expr (pattern body ...) rest)
     (let ((bound (ast-matches? expr 'pattern)))
       (if bound
           (ast-case-extract-vars bound
                                  (begin '() body ...)
                                  pattern)
           rest)))))

(define-syntax ast-case
  (syntax-rules (else)
    ((ast-case expr (else v ...))
     (begin v ...))
    ((ast-case expr rule rest ...)
     (let ((tmp expr))
       (ast-case-match-rule tmp
                            rule
                            (ast-case tmp rest ...))))))

(define (ast-matches? expr pattern)
  (cond ((equal? pattern '_)
         (empty-bindings))
        ((and (pair? pattern)
              (equal? (car pattern) 'unquote))
         (bindings (cadr pattern) expr))
        ((and (pair? pattern)
              (equal? (car pattern) 'quote)
              (symbol? (cadr pattern)))
         (and (is-type? expr 'symbol)
              (equal? (cadr pattern) (ast-symbol-value expr))
              (empty-bindings)))
        ((and (pair? pattern)
              (equal? (car pattern) 'quote)
              (number? (cadr pattern)))
         (and (is-type? expr 'number)
              (equal? (cadr pattern) (ast-number-value expr))
              (empty-bindings)))
        ((and (pair? pattern)
              (equal? (car pattern) 'quote)
              (string? (cadr pattern)))
         (and (is-type? expr 'number)
              (equal? (cadr pattern) (ast-string-value expr))
              (empty-bindings)))
        ((and (empty? pattern)
              (is-type? expr 'list)
              (empty? (ast-list-values expr)))
         (empty-bindings))
        ((pair? pattern)
         (case (car pattern)
           ((symbol number string)
            (and (or (symbol-node? expr)
                     (number-node? expr)
                     (string-node? expr))
                 (ast-matches? expr (cadr pattern))))
           ((list) (and (list-node? expr)
                        (ast-list-matches? (ast-list-values expr) (cdr pattern))))
           ((do) (and (do-node? expr)
                      (ast-list-matches? (ast-do-exprs expr) (cdr pattern))))
           ((body) (and (body-node? expr)
                        (ast-list-matches? (ast-body-exprs expr) (cdr pattern))))
           ((if) (and (if-node? expr)
                      (unify-bindings
                       (unify-bindings
                        (ast-matches? (ast-if-condition expr) (cadr pattern))
                        (ast-matches? (ast-if-then expr) (caddr pattern)))
                       (ast-matches? (ast-if-else expr) (cadddr pattern)))))
           ((def) (and (def-node? expr)
                       (unify-bindings (ast-matches? (ast-def-name expr) (cadr pattern))
                                       (ast-matches? (ast-def-value expr) (caddr pattern)))))
           ((app) (and (app-node? expr)
                       (unify-bindings (ast-matches? (ast-app-op expr) (cadr pattern))
                                       (ast-list-matches? (ast-app-args expr) (cddr pattern)))))
           ((primop-app) (and (primop-app-node? expr)
                              ;; NOTE Spoofs a full symbol node for the op to make matching easier.
                              (unify-bindings (ast-matches? (at (get-location expr)
                                                                (generated
                                                                 (make-symbol-node
                                                                  (ast-primop-app-op expr))))
                                                            (cadr pattern))
                                              (ast-list-matches? (ast-primop-app-args expr) (cddr pattern)))))
           ((lambda) (and (lambda-node? expr)
                          (unify-bindings (ast-list-matches? (ast-lambda-formals expr) (cadr pattern))
                                          (ast-matches? (ast-lambda-body expr) (caddr pattern)))))
           ((binding) (and (binding-node? expr)
                           (unify-bindings (ast-matches? (ast-binding-var expr) (cadr pattern))
                                           (ast-matches? (ast-binding-val expr) (caddr pattern)))))
           ((let) (and (let-node? expr)
                       (unify-bindings (ast-list-matches? (ast-let-bindings expr)
                                                          (cadr pattern))
                                       (ast-matches? (ast-let-body expr)
                                                     (caddr pattern)))))
           ((letrec) (and (letrec-node? expr)
                          (unify-bindings (ast-list-matches? (ast-letrec-bindings expr)
                                                             (cadr pattern))
                                          (ast-matches? (ast-letrec-body expr)
                                                        (caddr pattern)))))
           ((fix) (and (fix-node? expr)
                       (unify-bindings (ast-list-matches? (ast-fix-bindings expr)
                                                          (cadr pattern))
                                       (ast-matches? (ast-fix-body expr)
                                                     (caddr pattern)))))
           ;; NOTE These need to be named differently as they interfere with convenience syntax of the patterns and binding resolution.
           ((a-quote a-quasiquote an-unquote an-unquote-splicing)
            (and (or (quote-node? expr)
                     (quasiquote-node? expr)
                     (unquote-node? expr)
                     (unquote-splicing-node? expr))
                 (ast-matches? (ast-quoted-expr expr) (cadr pattern))))
           ((const)
            (and (const-node? expr)
                 (ast-matches? (ast-const-value expr) (cadr pattern))))
           ;; NOTE These are special nodes that we might still want to match for error handling etc.
           ((<error>) (and (error-node? expr)
                           (ast-matches? (ast-error-expr expr)
                                         (cadr pattern))))
           ((<location>) (location-node? expr))
           (else #f)))
        (else #f)))

(define (ast-list-matches? exprs pattern)
  (cond ((and (empty? exprs)
              (empty? pattern))
         (empty-bindings))
        ((equal? pattern '_)
         (empty-bindings))
        ((and (pair? pattern)
              (equal? (car pattern) 'unquote))
         (bindings (cadr pattern) exprs))
        ((and (pair? exprs)
              (pair? pattern))
         (unify-bindings (ast-matches? (car exprs) (car pattern))
                         (ast-list-matches? (cdr exprs) (cdr pattern))))
        (else
         #f)))

(define (empty-bindings)
  (hash))

(define (bindings . vars)
  (apply hash vars))

(define (unify-bindings a b)
  (if (or (false? a)
          (false? b))
      #f
      (let ((as (hash->list a)))
        (if (every? (lambda (kv)
                      (or (not (hash-has-key? b (car kv)))
                          (ast-eqv? (cdr kv)
                                    (hash-ref b (car kv)))))
                    as)
            (make-immutable-hash (append as
                                         (hash->list b)))
            #f))))

(define (get-var vars var)
  (hash-ref vars var))

