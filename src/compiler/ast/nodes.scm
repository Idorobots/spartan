;; Basic definitions

(define (ast-node? node)
  (hash? node))

(define (ast-node . properties)
  (apply hasheq properties))

(define (ast-get* node property default)
  (hash-ref node property default))

(define (ast-get node property)
  (ast-get* node property (lambda ()
                            (compiler-bug "ast-get on a key with no default value: "
                                          (cons node property)))))

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

(define (ast-number-value node)
  (ast-get node 'value))

;; Symbol
(define (make-symbol-node value)
  (ast-node 'type 'symbol 'value value))

(define (make-gensym-node root)
  (generated
   (make-symbol-node (gensym root))))

(define (symbol-node? node)
  (is-type? node 'symbol))

(define (ast-symbol-value node)
  (ast-get node 'value))

(define (safe-symbol-value expr)
  (cond ((symbol-node? expr)
         (ast-symbol-value expr))
        ((and (error-node? expr)
              (symbol-node? (ast-error-expr expr)))
         (ast-symbol-value (ast-error-expr expr)))
        (else '<error>)))

;; String
(define (make-string-node value)
  (ast-node 'type 'string 'value value))

(define (string-node? node)
  (is-type? node 'string))

(define (ast-string-value node)
  (ast-get node 'value))

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

(define (ast-if-condition node)
  (ast-get node 'condition))

(define (ast-if-then node)
  (ast-get node 'then))

(define (ast-if-else node)
  (ast-get node 'else))

;; Do
(define (make-do-node exprs)
  (ast-node 'type 'do 'exprs exprs))

(define (do-node? node)
  (is-type? node 'do))

(define (ast-do-exprs node)
  (ast-get node 'exprs))

;; Implicit body
(define (make-body-node exprs ctx)
  (generated
   (context ctx
            (ast-node 'type 'body 'exprs exprs))))

(define (body-node? node)
  (is-type? node 'body))

(define (ast-body-exprs node)
  (ast-get node 'exprs))

;; Lambda
(define (make-lambda-node formals body)
  (ast-node 'type 'lambda 'formals formals 'body body))

(define (lambda-node? node)
  (is-type? node 'lambda))

(define (ast-lambda-body node)
  (ast-get node 'body))

(define (ast-lambda-formals node)
  (ast-get node 'formals))

;; Binding
(define (make-binding-node var val)
  (ast-node 'type 'binding 'var var 'val val))

(define (binding-node? node)
  (is-type? node 'binding))

(define (ast-binding-var binding)
  (ast-get binding 'var))

(define (ast-binding-val binding)
  (ast-get binding 'val))

(define (complexity complexity binding)
  (ast-set binding 'complexity complexity))

(define (get-complexity binding)
  (ast-get binding 'complexity))

(define (self-recoursive rec? binding)
  (if rec?
      (ast-set binding 'self-recoursive rec?)
      binding))

(define (get-self-recoursive binding)
  (ast-get* binding 'self-recoursive #f))

(define (recoursive? bindings)
  (or (> (length bindings) 1)
      (some? get-self-recoursive bindings)))

;; Let
(define (make-let-node bindings body)
  (ast-node 'type 'let 'bindings bindings 'body body))

(define (let-node? node)
  (is-type? node 'let))

(define (ast-let-bindings node)
  (ast-get node 'bindings))

(define (ast-let-body node)
  (ast-get node 'body))

;; Letrec
(define (make-letrec-node bindings body)
  (ast-node 'type 'letrec 'bindings bindings 'body body))

(define (letrec-node? node)
  (is-type? node 'letrec))

(define (ast-letrec-bindings node)
  (ast-get node 'bindings))

(define (ast-letrec-body node)
  (ast-get node 'body))

;; Fix
(define (make-fix-node bindings body)
  (ast-node 'type 'fix 'bindings bindings 'body body))

(define (fix-node? node)
  (is-type? node 'fix))

(define (ast-fix-bindings node)
  (ast-get node 'bindings))

(define (ast-fix-body node)
  (ast-get node 'body))

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

;; Constant
(define (make-const-node value)
  (generated
   (ast-node 'type 'const 'value value)))

(define (const-node? node)
  (is-type? node 'const))

(define (ast-const-value node)
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

(define (ast-app-op node)
  (ast-get node 'op))

(define (ast-app-args node)
  (ast-get node 'args))

;; Primop application
(define (make-primop-app-node op args)
  (generated
   (ast-node 'type 'primop-app 'op op 'args args)))

(define (primop-app-node? node)
  (is-type? node 'primop-app))

(define (ast-primop-app-op node)
  (ast-get node 'op))

(define (ast-primop-app-args node)
  (ast-get node 'args))

;; Parse location marker
(define (make-location-node)
  (ast-node 'type '<location>))

(define (location-node? node)
  (is-type? node '<location>))

;; Error within parse tree
(define (make-error-node expr)
  (ast-node 'type '<error> 'expr expr))

(define (error-node? node)
  (is-type? node '<error>))

(define (ast-error-expr node)
  (ast-get node 'expr))
