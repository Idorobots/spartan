;; AST

(load-once "compiler/utils/utils.scm")
(load-once "compiler/utils/set.scm")
(load-once "compiler/utils/gensym.scm")

(load-once "compiler/errors.scm")

;; AST Node

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

;; AST metadata

(define (location start end)
  (cons start end))

(define (location-start loc)
  (car loc))

(define (location-end loc)
  (cdr loc))

(define (location<? a b)
  (< (location-start a)
     (location-start b)))

(define (ast-node-location node)
  (ast-get node 'location))

(define (ast-node-location-start node)
  (car (ast-node-location node)))

(define (ast-node-location-end node)
  (cdr (ast-node-location node)))

(define (set-ast-node-location node location)
  (ast-set node 'location location))

(define (at location node)
  (set-ast-node-location node location))

(define (replace old new)
  (at (ast-node-location old)
      ((if (generated? old) generated id)
       new)))

(define (set-ast-node-generated node gen)
  (ast-set node 'generated gen))

(define (ast-node-generated node)
  (ast-get* node 'generated #f))

(define (generated node)
  (set-ast-node-generated node #t))

(define (generated? node)
  (ast-node-generated node))

(define (set-ast-node-context node ctx)
  (ast-set node 'context ctx))

(define (ast-node-context* node default)
  (ast-get* node 'context default))

(define (ast-node-context node)
  (ast-node-context* node '()))

(define (set-ast-node-free-vars vars node) ;; FIXME Parameter order.
  (cond ((and (set-empty? vars)
              (set-empty? (ast-node-free-vars node)))
         node)
        ((ast-symbol? node)
         ;; NOTE Symbols always are their own free var, no need to store that in the AST.
         node)
        (else
         (ast-set node 'free-vars vars))))

(define (ast-node-free-vars node)
  (if (ast-symbol? node)
      (set (ast-symbol-value node))
      (ast-get* node 'free-vars (set))))

(define (set-ast-node-bound-vars vars node) ;; FIXME Parameter order.
  (if (and (set-empty? vars)
           (set-empty? (ast-node-bound-vars node)))
      node
      (ast-set node 'bound-vars vars)))

(define (ast-node-bound-vars node)
  (ast-get* node 'bound-vars (set)))

(define (ast-node-type node)
  (ast-get node 'type))

(define (is-type? node type)
  (equal? (ast-node-type node)
          type))

;; AST nodes

;; Number
(define (make-ast-number value)
  (ast-node 'type 'number 'value value))

(define (ast-number? node)
  (is-type? node 'number))

(define (ast-number-value node)
  (ast-get node 'value))

;; Symbol
(define (make-ast-symbol value)
  (ast-node 'type 'symbol 'value value))

(define (make-ast-gensym root)
  (generated
   (make-ast-symbol (gensym root))))

(define (ast-symbol? node)
  (is-type? node 'symbol))

(define (ast-symbol-value node)
  (ast-get node 'value))

(define (safe-symbol-value expr)
  (cond ((ast-symbol? expr)
         (ast-symbol-value expr))
        ((and (ast-error? expr)
              (ast-symbol? (ast-error-expr expr)))
         (ast-symbol-value (ast-error-expr expr)))
        (else '<error>)))

;; String
(define (make-ast-string value)
  (ast-node 'type 'string 'value value))

(define (ast-string? node)
  (is-type? node 'string))

(define (ast-string-value node)
  (ast-get node 'value))

;; List
(define (make-ast-list values)
  (ast-node 'type 'list 'value values))

(define (ast-list? node)
  (is-type? node 'list))

(define (ast-list-nth expr nth)
  (list-ref (ast-get expr 'value) nth))

(define (ast-list-values expr)
  (ast-get expr 'value))

(define (set-ast-list-values expr values)
  (ast-set expr 'value values))

(define (ast-list-car expr)
  (list-ref (ast-get expr 'value) 0))

(define (ast-list-cdr expr)
  (cdr (ast-get expr 'value)))

(define (ast-list-length expr)
  (length (ast-list-values expr)))

;; If
(define (make-ast-if condition then else)
  (ast-node 'type 'if 'condition condition 'then then 'else else))

(define (ast-if? node)
  (is-type? node 'if))

(define (ast-if-condition node)
  (ast-get node 'condition))

(define (set-ast-if-condition node condition)
  (ast-set node 'condition condition))

(define (ast-if-then node)
  (ast-get node 'then))

(define (set-ast-if-then node then)
  (ast-set node 'then then))

(define (ast-if-else node)
  (ast-get node 'else))

(define (set-ast-if-else node else)
  (ast-set node 'else else))

;; Do
(define (make-ast-do exprs)
  (ast-node 'type 'do 'exprs exprs))

(define (ast-do? node)
  (is-type? node 'do))

(define (ast-do-exprs node)
  (ast-get node 'exprs))

(define (set-ast-do-exprs node exprs)
  (ast-set node 'exprs exprs))

;; Implicit body
(define (make-ast-body exprs ctx)
  (generated
   (set-ast-node-context
    (ast-node 'type 'body 'exprs exprs)
    ctx)))

(define (ast-body? node)
  (is-type? node 'body))

(define (ast-body-exprs node)
  (ast-get node 'exprs))

(define (set-ast-body-exprs node exprs)
  (ast-set node 'exprs exprs))

;; Lambda
(define (make-ast-lambda formals body)
  (ast-node 'type 'lambda 'formals formals 'body body))

(define (ast-lambda? node)
  (is-type? node 'lambda))

(define (ast-lambda-body node)
  (ast-get node 'body))

(define (set-ast-lambda-body node body)
  (ast-set node 'body body))

(define (ast-lambda-formals node)
  (ast-get node 'formals))

(define (set-ast-lambda-formals node formals)
  (ast-set node 'formals formals))

;; Binding
(define (make-ast-binding var val)
  (ast-node 'type 'binding 'var var 'val val))

(define (ast-binding? node)
  (is-type? node 'binding))

(define (ast-binding-var binding)
  (ast-get binding 'var))

(define (set-ast-binding-var binding var)
  (ast-set binding 'var var))

(define (ast-binding-val binding)
  (ast-get binding 'val))

(define (set-ast-binding-val binding val)
  (ast-set binding 'val val))

(define (set-ast-binding-complexity binding complexity)
  (ast-set binding 'complexity complexity))

(define (ast-binding-complexity binding)
  (ast-get binding 'complexity))

(define (set-ast-binding-self-recursive binding rec?)
  (if rec?
      (ast-set binding 'self-recursive rec?)
      binding))

(define (ast-node-self-recursive binding)
  (ast-get* binding 'self-recursive #f))

(define (recursive? bindings)
  (or (> (length bindings) 1)
      (some? ast-node-self-recursive bindings)))

;; Let
(define (make-ast-let bindings body)
  (ast-node 'type 'let 'bindings bindings 'body body))

(define (ast-let? node)
  (is-type? node 'let))

(define (ast-let-bindings node)
  (ast-get node 'bindings))

(define (set-ast-let-bindings node bindings)
  (ast-set node 'bindings bindings))

(define (ast-let-body node)
  (ast-get node 'body))

(define (set-ast-let-body node body)
  (ast-set node 'body body))

;; Letrec
(define (make-ast-letrec bindings body)
  (ast-node 'type 'letrec 'bindings bindings 'body body))

(define (ast-letrec? node)
  (is-type? node 'letrec))

(define (ast-letrec-bindings node)
  (ast-get node 'bindings))

(define (ast-letrec-body node)
  (ast-get node 'body))

(define (set-ast-letrec-bindings node bindings)
  (ast-set node 'bindings bindings))

(define (set-ast-letrec-body node body)
  (ast-set node 'body body))

;; Fix
(define (make-ast-fix bindings body)
  (ast-node 'type 'fix 'bindings bindings 'body body))

(define (ast-fix? node)
  (is-type? node 'fix))

(define (ast-fix-bindings node)
  (ast-get node 'bindings))

(define (ast-fix-body node)
  (ast-get node 'body))

(define (set-ast-fix-bindings node bindings)
  (ast-set node 'bindings bindings))

(define (set-ast-fix-body node body)
  (ast-set node 'body body))

;; Quote
(define (make-ast-quote value)
  (ast-node 'type 'quote 'value value))

(define (ast-quote? node)
  (is-type? node 'quote))

(define (ast-quote-expr node)
  (ast-get node 'value))

(define (set-ast-quote-expr node expr)
  (ast-set node 'value expr))

;; Quasiquote
(define (make-ast-quasiquote value)
  (ast-node 'type 'quasiquote 'value value))

(define (ast-quasiquote? node)
  (is-type? node 'quasiquote))

(define (ast-quasiquote-expr node)
  (ast-get node 'value))

(define (set-ast-quasiquote-expr node expr)
  (ast-set node 'value expr))

;; Unquote
(define (make-ast-unquote value)
  (ast-node 'type 'unquote 'value value))

(define (ast-unquote? node)
  (is-type? node 'unquote))

(define (ast-unquote-expr node)
  (ast-get node 'value))

(define (set-ast-unquote-expr node expr)
  (ast-set node 'value expr))

;; Unquote splicing
(define (make-ast-unquote-splicing value)
  (ast-node 'type 'unquote-splicing 'value value))

(define (ast-unquote-splicing? node)
  (is-type? node 'unquote-splicing))

(define (ast-unquote-splicing-expr node)
  (ast-get node 'value))

(define (set-ast-unquote-splicing-expr node expr)
  (ast-set node 'value expr))

;; FIXME This should be removed
(define (ast-quoted-expr node)
  (ast-get node 'value))

;; Constant
(define (make-ast-const value)
  (generated
   (ast-node 'type 'const 'value value)))

(define (ast-const? node)
  (is-type? node 'const))

(define (ast-const-value node)
  (ast-get node 'value))

(define (set-ast-const-value node value)
  (ast-set node 'value value))

;; Definition
(define (make-ast-def name value)
  (ast-node 'type 'def 'name name 'value value))

(define (ast-def? node)
  (is-type? node 'def))

(define (ast-def-name node)
  (ast-get node 'name))

(define (set-ast-def-name node name)
  (ast-set node 'name name))

(define (ast-def-value node)
  (ast-get node 'value))

(define (set-ast-def-value node value)
  (ast-set node 'value value))

;; Application
(define (make-ast-app op args)
  (ast-node 'type 'app 'op op 'args args))

(define (ast-app? node)
  (is-type? node 'app))

(define (ast-app-op node)
  (ast-get node 'op))

(define (set-ast-app-op node op)
  (ast-set node 'op op))

(define (ast-app-args node)
  (ast-get node 'args))

(define (set-ast-app-args node args)
  (ast-set node 'args args))

;; Primop application
(define (make-ast-primop-app op args)
  (generated
   (ast-node 'type 'primop-app 'op op 'args args)))

(define (ast-primop-app? node)
  (is-type? node 'primop-app))

(define (ast-primop-app-op node)
  (ast-get node 'op))

(define (ast-primop-app-args node)
  (ast-get node 'args))

(define (set-ast-primop-app-args node args)
  (ast-set node 'args args))

;; Parse location marker
(define (make-ast-location)
  (ast-node 'type '<location>))

(define (ast-location? node)
  (is-type? node '<location>))

;; Error within parse tree
(define (make-ast-error expr)
  (ast-node 'type '<error> 'expr expr))

(define (ast-error? node)
  (is-type? node '<error>))

(define (ast-error-expr node)
  (ast-get node 'expr))

(define (set-ast-error-expr node expr)
  (ast-set node 'expr expr))
