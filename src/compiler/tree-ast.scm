;; AST

(load "compiler/utils/set.scm")
(load "compiler/utils/utils.scm")

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
  (ast-node 'type 'primop-app 'op op 'args args))

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
      ((if (generated? old) generated id)
       new)))

(define (generated node)
  (ast-set node 'generated #t))

(define (generated? node)
  (ast-get* node 'generated #f))

(define (context ctx node)
  (ast-set node 'context ctx))

(define (get-context* node default)
  (ast-get* node 'context default))

(define (get-context node)
  (get-context* node '()))

(define (free-vars vars node)
  (if (set-empty? vars)
      node
      (ast-set node 'free-vars vars)))

(define (get-free-vars node)
  (ast-get* node 'free-vars (set)))

(define (bound-vars vars node)
  (if (set-empty? vars)
      node
      (ast-set node 'bound-vars vars)))

(define (get-bound-vars node)
  (ast-get* node 'bound-vars (set)))

(define (get-type node)
  (ast-get node 'type))

(define (is-type? node type)
  (equal? (get-type node)
          type))

(define (walk-ast f expr)
  (let* ((mf (partial map f)))
    (case (get-type expr)
      ((number symbol string <location>)
       expr)
      ((if)
       (foldl (lambda (field acc)
                (ast-update acc field f))
              expr
              '(condition then else)))
      ((do)
       (ast-update expr 'exprs mf))
      ((lambda)
       (ast-update (ast-update expr 'formals mf) 'body f))
      ((let letrec fix)
       (ast-update (ast-update expr 'body f) 'bindings mf))
      ((binding)
       (ast-update (ast-update expr 'var f) 'val f))
      ((quote quasiquote unquote unquote-splicing)
       (ast-update expr 'value f))
      ((def)
       (ast-update (ast-update expr 'name f) 'value f))
      ((app primop-app)
       (ast-update (ast-update expr 'op f) 'args mf))
      ((list)
       (ast-update expr 'value mf))
      ((<error>)
       (ast-update expr 'expr f))
      (else (compiler-bug "Unexpected expression: " expr)))))

(define (map-ast pre post expr)
  (if (ast-node? expr)
      (let ((m (partial map-ast pre post)))
        (post (walk-ast m (pre expr))))
      (compiler-bug "Non-AST object passed to map-ast:" expr)))

(define (ast->plain ast)
  (map-ast id
           (lambda (expr)
             (case (get-type expr)
               ((number symbol string list) (ast-get expr 'value))
               ((if) (list 'if
                           (ast-if-condition expr)
                           (ast-if-then expr)
                           (ast-if-else expr)))
               ((do) (cons 'do (ast-do-exprs expr)))
               ((lambda) (list 'lambda (ast-lambda-formals expr) (ast-lambda-body expr)))
               ((let) (list 'let (ast-let-bindings expr)
                            (ast-let-body expr)))
               ((letrec) (list 'letrec (ast-letrec-bindings expr)
                               (ast-letrec-body expr)))
               ((fix) (list 'fix (ast-fix-bindings expr)
                            (ast-fix-body expr)))
               ((binding) (list (ast-binding-var expr)
                                (ast-binding-val expr)))
               ((quote) (list 'quote (ast-quoted-expr expr)))
               ((quasiquote) (list 'quasiquote (ast-quoted-expr expr)))
               ((unquote) (list 'unquote (ast-quoted-expr expr)))
               ((unquote-splicing) (list 'unquote-splicing (ast-quoted-expr expr)))
               ((def) (list 'define (ast-get expr 'name) (ast-quoted-expr expr)))
               ((app primop-app) (list* (ast-app-op expr) (ast-app-args expr)))
               (else (compiler-bug "Unexpected expression: " expr))))
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
           ((list) (and (list-node? expr)
                        (ast-list-matches? (ast-list-values expr) (cdr pattern))))
           ((do) (and (do-node? expr)
                      (ast-list-matches? (ast-do-exprs expr) (cdr pattern))))
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
                              (unify-bindings (ast-matches? (ast-primop-app-op expr) (cadr pattern))
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
                          (unify-bindings (ast-list-matches? (ast-let-bindings expr)
                                                             (cadr pattern))
                                          (ast-matches? (ast-let-body expr)
                                                        (caddr pattern)))))
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
