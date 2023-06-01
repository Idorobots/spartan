#lang racket

;; AST

(require "../utils/utils.rkt")
(require "../utils/set.rkt")
(require "../utils/gensym.rkt")

(provide (all-defined-out))

;; AST Node
(struct ast-node
  (type
   location
   generated
   context
   _free-vars ;; FIXME Setter & getter shenanigans.
   bound-vars ;; FIXME Setter shenanigans.
   data)
  #:transparent
  #:constructor-name make-ast-node)

(define (make-ast-node* type location data)
  (make-ast-node type
                 location
                 #f    ;; generated
                 #f    ;; context
                 (set) ;; free-vars
                 (set) ;; bound-vars
                 data))

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

(define (ast-node-location-start node)
  (car (ast-node-location node)))

(define (ast-node-location-end node)
  (cdr (ast-node-location node)))

(define (set-ast-node-location node loc)
  (struct-copy ast-node node (location loc)))

(define (replace old new)
  (set-ast-node-location ((if (generated? old) generated id)
                          new)
                         (ast-node-location old)))

(define (set-ast-node-generated node gen)
  (struct-copy ast-node node (generated gen)))

(define (generated node)
  (set-ast-node-generated node #t))

(define (generated? node)
  (ast-node-generated node))

(define (set-ast-node-context node ctx)
  (struct-copy ast-node node (context ctx)))

(define (ast-node-context* node default)
  (or (ast-node-context node)
      default))

(define (set-ast-node-free-vars vars node) ;; FIXME Parameter order.
  (cond ((and (set-empty? vars)
              (set-empty? (ast-node-free-vars node)))
         node)
        ((ast-symbol? node)
         ;; NOTE Symbols always are their own free var, no need to store that in the AST.
         node)
        (else
         (struct-copy ast-node node (_free-vars vars)))))

(define (ast-node-free-vars node)
  (if (ast-symbol? node)
      (set (ast-symbol-value node))
      (ast-node-_free-vars node)))

(define (set-ast-node-bound-vars vars node) ;; FIXME Parameter order.
  (if (and (set-empty? vars)
           (set-empty? (ast-node-bound-vars node)))
      node
      (struct-copy ast-node node (bound-vars vars))))

(define (set-ast-node-data node data)
  (struct-copy ast-node node (data data)))

(define (is-type? node type)
  (eq? (ast-node-type node)
          type))

;; AST nodes

;; Number
(define (make-ast-number loc value)
  (make-ast-node* 'number loc value))

(define (ast-number? node)
  (is-type? node 'number))

(define (ast-number-value node)
  (ast-node-data node))

(define (set-ast-number-value node val)
  (set-ast-node-data node val))

;; Symbol
(define (make-ast-symbol loc value)
  (make-ast-node* 'symbol loc value))

(define (make-ast-gensym loc root)
  (generated
   (make-ast-symbol loc (gensym root))))

(define (ast-symbol? node)
  (is-type? node 'symbol))

(define (ast-symbol-value node)
  (ast-node-data node))

(define (set-ast-symbol-value node val)
  (set-ast-node-data node val))

(define (safe-symbol-value node)
  (cond ((ast-symbol? node)
         (ast-symbol-value node))
        ((and (ast-error? node)
              (ast-symbol? (ast-error-expr node)))
         (ast-symbol-value (ast-error-expr node)))
        (else '<error>)))

;; String
(define (make-ast-string loc value)
  (make-ast-node* 'string loc value))

(define (ast-string? node)
  (is-type? node 'string))

(define (ast-string-value node)
  (ast-node-data node))

(define (set-ast-string-value node val)
  (set-ast-node-data node val))

;; List
(define (make-ast-list loc values)
  (make-ast-node* 'list loc values))

(define (ast-list? node)
  (is-type? node 'list))

(define (ast-list-values node)
  (ast-node-data node))

(define (ast-list-nth node nth)
  (list-ref (ast-list-values node) nth))

(define (set-ast-list-values node vals)
  (set-ast-node-data node vals))

(define (ast-list-car node)
  (car (ast-list-values node)))

(define (ast-list-cdr node)
  (cdr (ast-list-values node)))

(define (ast-list-length node)
  (length (ast-list-values node)))

;; If
(struct ast-if-data (condition then else) #:transparent #:constructor-name make-ast-if-data)

(define (make-ast-if loc condition then else)
  (make-ast-node* 'if loc (make-ast-if-data condition then else)))

(define (ast-if? node)
  (is-type? node 'if))

(define (ast-if-condition node)
  (ast-if-data-condition (ast-node-data node)))

(define (set-ast-if-condition node cnd)
  (set-ast-node-data node (struct-copy ast-if-data (ast-node-data node) (condition cnd))))

(define (ast-if-then node)
  (ast-if-data-then (ast-node-data node)))

(define (set-ast-if-then node thn)
  (set-ast-node-data node (struct-copy ast-if-data (ast-node-data node) (then thn))))

(define (ast-if-else node)
  (ast-if-data-else (ast-node-data node)))

(define (set-ast-if-else node els)
  (set-ast-node-data node (struct-copy ast-if-data (ast-node-data node) (else els))))

;; Do
(define (make-ast-do loc exprs)
  (make-ast-node* 'do loc exprs))

(define (ast-do? node)
  (is-type? node 'do))

(define (ast-do-exprs node)
  (ast-node-data node))

(define (set-ast-do-exprs node exprs)
  (set-ast-node-data node exprs))

;; Implicit body
(define (make-ast-body loc exprs ctx)
  (generated
   (set-ast-node-context
    (make-ast-node* 'body loc exprs)
    ctx)))

(define (ast-body? node)
  (is-type? node 'body))

(define (ast-body-exprs node)
  (ast-node-data node))

(define (set-ast-body-exprs node exprs)
  (set-ast-node-data node exprs))

;; Lambda
(struct ast-lambda-data (formals body) #:transparent #:constructor-name make-ast-lambda-data)

(define (make-ast-lambda loc formals body)
  (make-ast-node* 'lambda loc (make-ast-lambda-data formals body)))

(define (ast-lambda? node)
  (is-type? node 'lambda))

(define (ast-lambda-body node)
  (ast-lambda-data-body (ast-node-data node)))

(define (set-ast-lambda-body node body)
  (set-ast-node-data node (struct-copy ast-lambda-data (ast-node-data node) (body body))))

(define (ast-lambda-formals node)
  (ast-lambda-data-formals (ast-node-data node)))

(define (set-ast-lambda-formals node formals)
  (set-ast-node-data node (struct-copy ast-lambda-data (ast-node-data node) (formals formals))))

;; Binding
(struct ast-binding-data (var val complexity self-recursive) #:transparent #:constructor-name make-ast-binding-data)

(define (make-ast-binding loc var val)
  (make-ast-node* 'binding loc (make-ast-binding-data var val #f #f)))

(define (ast-binding? node)
  (is-type? node 'binding))

(define (ast-binding-var node)
  (ast-binding-data-var (ast-node-data node)))

(define (set-ast-binding-var node var)
  (set-ast-node-data node (struct-copy ast-binding-data (ast-node-data node) (var var))))

(define (ast-binding-val node)
  (ast-binding-data-val (ast-node-data node)))

(define (set-ast-binding-val node val)
  (set-ast-node-data node (struct-copy ast-binding-data (ast-node-data node) (val val))))

(define (ast-binding-complexity node)
  (ast-binding-data-complexity (ast-node-data node)))

(define (set-ast-binding-complexity node complexity)
  (set-ast-node-data node (struct-copy ast-binding-data (ast-node-data node) (complexity complexity))))

(define (ast-binding-self-recursive node)
  (ast-binding-data-self-recursive (ast-node-data node)))

(define (set-ast-binding-self-recursive node rec?)
  (set-ast-node-data node (struct-copy ast-binding-data (ast-node-data node) (self-recursive rec?))))

(define (recursive? bindings)
  (or (> (length bindings) 1)
      (some? ast-binding-self-recursive bindings)))

;; Let
(struct ast-let-data (bindings body) #:transparent #:constructor-name make-ast-let-data)

(define (make-ast-let loc bindings body)
  (make-ast-node* 'let loc (make-ast-let-data bindings body)))

(define (ast-let? node)
  (is-type? node 'let))

(define (ast-let-bindings node)
  (ast-let-data-bindings (ast-node-data node)))

(define (set-ast-let-bindings node bindings)
  (set-ast-node-data node (struct-copy ast-let-data (ast-node-data node) (bindings bindings))))

(define (ast-let-body node)
  (ast-let-data-body (ast-node-data node)))

(define (set-ast-let-body node body)
  (set-ast-node-data node (struct-copy ast-let-data (ast-node-data node) (body body))))

;; Letrec
(struct ast-letrec-data (bindings body) #:transparent #:constructor-name make-ast-letrec-data)

(define (make-ast-letrec loc bindings body)
  (make-ast-node* 'letrec loc (make-ast-letrec-data bindings body)))

(define (ast-letrec? node)
  (is-type? node 'letrec))

(define (ast-letrec-bindings node)
  (ast-letrec-data-bindings (ast-node-data node)))

(define (set-ast-letrec-bindings node bindings)
  (set-ast-node-data node (struct-copy ast-letrec-data (ast-node-data node) (bindings bindings))))

(define (ast-letrec-body node)
  (ast-letrec-data-body (ast-node-data node)))

(define (set-ast-letrec-body node body)
  (set-ast-node-data node (struct-copy ast-letrec-data (ast-node-data node) (body body))))

;; Fix
(struct ast-fix-data (bindings body) #:transparent #:constructor-name make-ast-fix-data)

(define (make-ast-fix loc bindings body)
  (make-ast-node* 'fix loc (make-ast-fix-data bindings body)))

(define (ast-fix? node)
  (is-type? node 'fix))

(define (ast-fix-bindings node)
  (ast-fix-data-bindings (ast-node-data node)))

(define (set-ast-fix-bindings node bindings)
  (set-ast-node-data node (struct-copy ast-fix-data (ast-node-data node) (bindings bindings))))

(define (ast-fix-body node)
  (ast-fix-data-body (ast-node-data node)))

(define (set-ast-fix-body node body)
  (set-ast-node-data node (struct-copy ast-fix-data (ast-node-data node) (body body))))

;; Quote
(define (make-ast-quote loc expr)
  (make-ast-node* 'quote loc expr))

(define (ast-quote? node)
  (is-type? node 'quote))

(define (ast-quote-expr node)
  (ast-node-data node))

(define (set-ast-quote-expr node expr)
  (set-ast-node-data node expr))

;; Quasiquote
(define (make-ast-quasiquote loc expr)
  (make-ast-node* 'quasiquote loc expr))

(define (ast-quasiquote? node)
  (is-type? node 'quasiquote))

(define (ast-quasiquote-expr node)
  (ast-node-data node))

(define (set-ast-quasiquote-expr node expr)
  (set-ast-node-data node expr))

;; Unquote
(define (make-ast-unquote loc expr)
  (make-ast-node* 'unquote loc expr))

(define (ast-unquote? node)
  (is-type? node 'unquote))

(define (ast-unquote-expr node)
  (ast-node-data node))

(define (set-ast-unquote-expr node expr)
  (set-ast-node-data node expr))

;; Unquote splicing
(define (make-ast-unquote-splicing loc expr)
  (make-ast-node* 'unquote-splicing loc expr))

(define (ast-unquote-splicing? node)
  (is-type? node 'unquote-splicing))

(define (ast-unquote-splicing-expr node)
  (ast-node-data node))

(define (set-ast-unquote-splicing-expr node expr)
  (set-ast-node-data node expr))

;; FIXME This should be removed
(define (ast-quoted-expr node)
  (ast-node-data node))

;; Constant
(define (make-ast-const loc value)
  (generated
   (make-ast-node* 'const loc value)))

(define (ast-const? node)
  (is-type? node 'const))

(define (ast-const-value node)
  (ast-node-data node))

(define (set-ast-const-value node value)
  (set-ast-node-data node value))

;; Definition
(struct ast-def-data (name value) #:transparent #:constructor-name make-ast-def-data)

(define (make-ast-def loc name value)
  (make-ast-node* 'def loc (make-ast-def-data name value)))

(define (ast-def? node)
  (is-type? node 'def))

(define (ast-def-name node)
  (ast-def-data-name (ast-node-data node)))

(define (set-ast-def-name node name)
  (set-ast-node-data node (struct-copy ast-def-data (ast-node-data node) (name name))))

(define (ast-def-value node)
  (ast-def-data-value (ast-node-data node)))

(define (set-ast-def-value node value)
  (set-ast-node-data node (struct-copy ast-def-data (ast-node-data node) (value value))))

;; Application
(struct ast-app-data (op args) #:transparent #:constructor-name make-ast-app-data)

(define (make-ast-app loc op args)
  (make-ast-node* 'app loc (make-ast-app-data op args)))

(define (ast-app? node)
  (is-type? node 'app))

(define (ast-app-op node)
  (ast-app-data-op (ast-node-data node)))

(define (set-ast-app-op node op)
  (set-ast-node-data node (struct-copy ast-app-data (ast-node-data node) (op op))))

(define (ast-app-args node)
  (ast-app-data-args (ast-node-data node)))

(define (set-ast-app-args node args)
  (set-ast-node-data node (struct-copy ast-app-data (ast-node-data node) (args args))))

;; Primop application
(struct ast-primop-app-data (op args) #:transparent #:constructor-name make-ast-primop-app-data)

(define (make-ast-primop-app loc op args)
  (generated
   (make-ast-node* 'primop-app loc (make-ast-primop-app-data op args))))

(define (ast-primop-app? node)
  (is-type? node 'primop-app))

(define (ast-primop-app-op node)
  (ast-primop-app-data-op (ast-node-data node)))

(define (set-ast-primop-app-op node op)
  (set-ast-node-data node (struct-copy ast-primop-app-data (ast-node-data node) (op op))))

(define (ast-primop-app-args node)
  (ast-primop-app-data-args (ast-node-data node)))

(define (set-ast-primop-app-args node args)
  (set-ast-node-data node (struct-copy ast-primop-app-data (ast-node-data node) (args args))))

;; Parse location marker
(define (make-ast-location loc)
  (make-ast-node* '<location> loc loc))

(define (ast-location? node)
  (is-type? node '<location>))

;; Error within parse tree
(define (make-ast-error loc expr)
  (make-ast-node* '<error> loc expr))

(define (ast-error? node)
  (is-type? node '<error>))

(define (ast-error-expr node)
  (ast-node-data node))

(define (set-ast-error-expr node expr)
  (set-ast-node-data node expr))

;; Syntactic closure
(struct ast-syntactic-closure-data (env free-vars expr) #:transparent #:constructor-name make-ast-syntactic-closure-data)

(define (make-ast-syntactic-closure loc env free-vars expr)
  (generated
   (make-ast-node* 'syntactic-closure loc (make-ast-syntactic-closure-data env free-vars expr))))

(define (ast-syntactic-closure? node)
  (is-type? node 'syntactic-closure))

(define (ast-syntactic-closure-env node)
  (ast-syntactic-closure-data-env (ast-node-data node)))

(define (set-ast-syntactic-closure-env node env)
  (set-ast-node-data node (struct-copy ast-syntactic-closure-data (ast-node-data node) (env env))))

(define (ast-syntactic-closure-free-vars node)
  (ast-syntactic-closure-data-free-vars (ast-node-data node)))

(define (set-ast-syntactic-closure-free-vars node free-vars)
  (set-ast-node-data node (struct-copy ast-syntactic-closure-data (ast-node-data node) (free-vars free-vars))))

(define (ast-syntactic-closure-expr node)
  (ast-syntactic-closure-data-expr (ast-node-data node)))

(define (set-ast-syntactic-closure-expr node expr)
  (set-ast-node-data node (struct-copy ast-syntactic-closure-data (ast-node-data node) (expr expr))))
