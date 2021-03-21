;; AST

(load "compiler/utils.scm")

;; Basic definitions

(define (ast-node? node)
  (hash? node))

(define (ast-node . properties)
  (apply hasheq properties))

(define (ast-get* node property default)
  (hash-ref node property default))

(define (ast-get node property)
  (ast-get* node property compiler-bug))

(define (ast-set node property value)
  (hash-set node property value))

(define (ast-update node property f)
  (ast-set node property (f (ast-get* node property '()))))

;; AST nodes

;; Number
(define (make-number-node value)
  (ast-node 'type 'number 'value value))

(define (number-node? node)
  (is-type? node 'number))

;; Symbol
(define (make-symbol-node value)
  (ast-node 'type 'symbol 'value value))

(define (symbol-node? node)
  (is-type? node 'symbol))

(define (ast-symbol-value node)
  (ast-get node 'value))

;; String
(define (make-string-node value)
  (ast-node 'type 'string 'value value))

(define (string-node? node)
  (is-type? node 'string))

;; List
(define (make-list-node values)
  (ast-node 'type 'list 'value values))

(define (list-node? node)
  (is-type? node 'list))

(define (ast-list-nth expr nth)
  (list-ref (ast-get expr 'value) nth))

(define (ast-list-values expr)
  (ast-get expr 'value))

(define (ast-list-car expr)
  (list-ref (ast-get expr 'value) 0))

(define (ast-list-cdr expr)
  (cdr (ast-get expr 'value)))

(define (ast-list-length expr)
  (length (ast-list-values expr)))

;; If
(define (make-if-node condition then else)
  (ast-node 'type 'if 'condition condition 'then then 'else else))

(define (if-node? node)
  (is-type? node 'if))

;; Do
(define (make-do-node exprs)
  (ast-node 'type 'do 'exprs exprs))

(define (do-node? node)
  (is-type? node 'do))

;; FIXME This is used by several different phases to store context for body expansion later on
;; FIXME to get more meaningful error messages. This should probably be done differently.
(define (wrap-with-do exprs ctx)
  (cond ((and (list? exprs)
              (> (length exprs) 1))
         ;; NOTE The body spans all the expressions within it.
         (at (location (get-location-start (car exprs))
                       (get-location-end (last exprs)))
             (generated
              (context ctx
                       (make-do-node exprs)))))
        ((list? exprs)
         (car exprs))
        (else exprs)))

;; Lambda
(define (make-lambda-node formals body)
  (ast-node 'type 'lambda 'formals formals 'body body))

(define (lambda-node? node)
  (is-type? node 'lambda))

;; Let
(define (make-let-node bindings body)
  (ast-node 'type 'let 'bindings bindings 'body body))

(define (let-node? node)
  (is-type? node 'let))

;; Letrec
(define (make-letrec-node bindings body)
  (ast-node 'type 'letrec 'bindings bindings 'body body))

(define (letrec-node? node)
  (is-type? node 'letrec))

;; Quotation
(define (make-quote-node value)
  (ast-node 'type 'quote 'value value))

(define (quote-node? node)
  (is-type? node 'quote))

(define (make-quasiquote-node value)
  (ast-node 'type 'quasiquote 'value value))

(define (quasiquote-node? node)
  (is-type? node 'quasiquote))

(define (make-unquote-node value)
  (ast-node 'type 'unquote 'value value))

(define (unquote-node? node)
  (is-type? node 'unquote))

(define (make-unquote-splicing-node value)
  (ast-node 'type 'unquote-splicing 'value value))

(define (unquote-splicing-node? node)
  (is-type? node 'unquote-splicing))

(define (ast-quoted-expr node)
  (ast-get node 'value))

;; Definition
(define (make-def-node name value)
  (ast-node 'type 'def 'name name 'value value))

(define (def-node? node)
  (is-type? node 'def))

(define (ast-def-name node)
  (ast-get node 'name))

(define (ast-def-value node)
  (ast-get node 'value))

;; Application
(define (make-app-node op args)
  (ast-node 'type 'app 'op op 'args args))

(define (app-node? node)
  (is-type? node 'app))

(define (make-primop-app-node op args)
  (ast-node 'type 'primop-app 'op op 'args args))

(define (primop-app-node? node)
  (is-type? node 'primop-app))

;; Error within parse tree
(define (make-error-node)
  (ast-node 'type '<error> 'value "<error>"))

(define (error-node? node)
  (is-type? node '<error>))

;; AST utils

(define (location start end)
  (cons start end))

(define (location-start loc)
  (car loc))

(define (location-end loc)
  (cdr loc))

(define (location<? a b)
  (< (location-start a)
     (location-start b)))

(define (get-location node)
  (ast-get node 'location))

(define (get-location-start node)
  (car (get-location node)))

(define (get-location-end node)
  (cdr (get-location node)))

(define (at location node)
  (ast-set node 'location location))

(define (replace old new)
  (at (get-location old)
      ((if (generated? old)
           generated
           id)
       new)))

(define (generated node)
  (ast-set node 'generated #t))

(define (generated? node)
  (ast-get* node 'generated #f))

(define (context ctx node)
  (ast-set node 'context ctx))

(define (get-context node)
  (ast-get* node 'context '()))

(define (get-type node)
  (ast-get node 'type))

(define (is-type? node type)
  (equal? (get-type node)
          type))

(define (maybe-map f expr)
  ((if (list? expr)
       (partial map f)
       f)
   expr))

(define (walk-ast f expr)
  (let* ((mf (partial map f))
         (maybe-mf (partial maybe-map f)))
    (case (get-type expr)
      ((number symbol string) expr)
      ((if) (foldl (lambda (field acc)
                     (ast-update acc field f))
                   expr
                   '(condition then else)))
      ((do) (ast-update expr 'exprs mf))
      ((lambda) (ast-update (ast-update expr 'formals mf)
                            'body
                            maybe-mf))
      ((let) (ast-update (ast-update expr
                                     'bindings
                                     (partial map
                                              (lambda (b)
                                                (cons (f (car b))
                                                      (f (cdr b))))))
                         'body
                         maybe-mf))
      ((letrec) (ast-update (ast-update expr
                                        'bindings
                                        (partial map
                                                 (lambda (b)
                                                   (cons (f (car b))
                                                         (f (cdr b))))))
                            'body
                            maybe-mf))
      ((quote quasiquote unquote unquote-splicing) (ast-update expr 'value f))
      ((def) (ast-update (ast-update expr 'name f) 'value f))
      ((app primop-app) (ast-update (ast-update expr 'op f) 'args mf))
      ((list) (ast-update expr 'value mf))
      ((<error>) expr)
      (else (error "Unexpected expression: " expr)))))

(define (map-ast pre post expr)
  (if (ast-node? expr)
      (let ((m (partial map-ast pre post)))
        (post (walk-ast m (pre expr))))
      (compiler-bug)))

(define (ast->plain ast)
  (map-ast id
           (lambda (expr)
             (case (get-type expr)
               ((number symbol string list) (ast-get expr 'value))
               ((if) (list 'if
                          (ast-get expr 'condition)
                          (ast-get expr 'then)
                          (ast-get expr 'else)))
               ((do) (cons 'do (ast-get expr 'exprs)))
               ((lambda) (list 'lambda (ast-get expr 'formals) (ast-get expr 'body)))
               ((let) (list 'let (map (lambda (b)
                                        (list (car b)
                                              (cdr b)))
                                      (ast-get expr 'bindings))
                            (ast-get expr 'body)))
               ((letrec) (list 'letrec (map (lambda (b)
                                        (list (car b)
                                              (cdr b)))
                                            (ast-get expr 'bindings))
                               (ast-get expr 'body)))
               ((quote) (list 'quote (ast-get expr 'value)))
               ((quasiquote) (list 'quasiquote (ast-get expr 'value)))
               ((unquote) (list 'unquote (ast-get expr 'value)))
               ((unquote-splicing) (list 'unquote-splicing (ast-get expr 'value)))
               ((def) (list 'define (ast-get expr 'name) (ast-get expr 'value)))
               ((app primop-app) (list* (ast-get expr 'op) (ast-get expr 'args)))
               (else (error "Unexpected expression: " expr))))
           ast))

;; AST destructuring

(define-syntax ast-case-extract-vars
  (syntax-rules (unquote get-var)
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
  (syntax-rules (ast-matches?2)
    ((ast-case-match-rule expr (pattern body ...) rest)
     (let ((bound (ast-matches? expr 'pattern)))
       (if bound
           (ast-case-extract-vars bound
                                  (begin body ...)
                                  pattern)
           rest)))))

(define-syntax ast-case
  (syntax-rules (else)
    ((ast-case expr (else v))
     v)
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
              (equal? (car pattern) 'quote))
         (case (get-type expr)
           ((symbol) (and (is-type? expr 'symbol)
                          (equal? (cadr pattern) (ast-symbol-value expr))
                          (empty-bindings)))
           (else #f)))
        ((and (empty? pattern)
              (is-type? expr 'list)
              (empty? (ast-list-values expr)))
         (empty-bindings))
        ((pair? pattern)
         (case (car pattern)
           ((list) (ast-list-matches? (ast-list-values expr) (cdr pattern)))
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
      (make-immutable-hash (append (hash->list a)
                                   (hash->list b)))))

(define (get-var vars var)
  (hash-ref vars var))
