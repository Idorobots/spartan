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
  (cond ((set-empty? vars)
         node)
        ((symbol-node? node)
         ;; NOTE Symbols always are their own free var, no need to store that in the AST.
         node)
        (else
         (ast-set node 'free-vars vars))))

(define (get-free-vars node)
  (if (symbol-node? node)
      (set (ast-symbol-value node))
      (ast-get* node 'free-vars (set))))

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

;; Walk

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
      ((do body)
       (ast-update expr 'exprs mf))
      ((lambda)
       (ast-update (ast-update expr 'formals mf) 'body f))
      ((let letrec fix)
       (ast-update (ast-update expr 'body f) 'bindings mf))
      ((binding)
       (ast-update (ast-update expr 'var f) 'val f))
      ((quote quasiquote unquote unquote-splicing const)
       (ast-update expr 'value f))
      ((def)
       (ast-update (ast-update expr 'name f) 'value f))
      ((app)
       (ast-update (ast-update expr 'op f) 'args mf))
      ((primop-app)
       (ast-update expr 'args mf))
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
               ((body) (cons 'do (ast-body-exprs expr)))
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
               ((const) (list 'quote (ast-const-value expr)))
               ((quasiquote) (list 'quasiquote (ast-quoted-expr expr)))
               ((unquote) (list 'unquote (ast-quoted-expr expr)))
               ((unquote-splicing) (list 'unquote-splicing (ast-quoted-expr expr)))
               ((def) (list 'define (ast-get expr 'name) (ast-quoted-expr expr)))
               ((app primop-app) (list* (ast-app-op expr) (ast-app-args expr)))
               (else (compiler-bug "Unexpected expression: " expr))))
           ast))
