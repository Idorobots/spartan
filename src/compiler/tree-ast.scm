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

;; Symbol
(define (make-symbol-node value)
  (ast-node 'type 'symbol 'value value))

;; String
(define (make-string-node value)
  (ast-node 'type 'string 'value value))

;; List
(define (make-list-node values)
  (ast-node 'type 'list 'value values))

;; If
(define (make-if-node condition then else)
  (ast-node 'type 'if 'condition condition 'then then 'else else))

;; Do
(define (make-do-node exprs)
  (ast-node 'type 'do 'exprs exprs))

;; Lambda
(define (make-lambda-node formals body)
  (ast-node 'type 'lambda 'formals formals 'body body))

;; Let
(define (make-let-node bindings body)
  (ast-node 'type 'let 'bindings bindings 'body body))

;; Letrec
(define (make-letrec-node bindings body)
  (ast-node 'type 'letrec 'bindings bindings 'body body))

;; Quotation
(define (make-quote-node value)
  (ast-node 'type 'plain-quote 'value value))

(define (make-quasiquote-node value)
  (ast-node 'type 'quasiquote 'value value))

(define (make-unquote-node value)
  (ast-node 'type 'unquote 'value value))

(define (make-unquote-splicing-node value)
  (ast-node 'type 'unquote-splicing 'value value))

;; Error within parse tree
(define (make-error-node)
  (ast-node 'type 'error 'value "<error>"))

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

(define (get-type node)
  (ast-get node 'type))

(define (is-type? node type)
  (equal? (get-type node)
          type))

(define (ast-list-nth expr nth)
  (list-ref (ast-get expr 'value) nth))

(define (map-ast pre post expr)
  (if (ast-node? expr)
      (let ((m (partial map-ast pre post))
            (expr (pre expr)))
        (post
         (case (get-type expr)
           ((number) expr)
           ((symbol) expr)
           ((string) expr)
           ((if) (foldl (lambda (field acc)
                         (ast-update acc field m))
                       expr
                       '(condition then else)))
           ((do) (ast-update expr 'exprs (partial map m)))
           ((lambda) (ast-update (ast-update expr 'formals (partial map m))
                                'body
                                m))
           ((let) (ast-update (ast-update expr
                                          'bindings
                                          (partial map (lambda (b)
                                                         (cons (m (car b))
                                                               (m (cdr b))))))
                             'body
                             m))
           ((letrec) (ast-update (ast-update expr
                                             'bindings
                                             (partial map (lambda (b)
                                                         (cons (m (car b))
                                                               (m (cdr b))))))
                                'body
                                m))
           ((plain-quote) (ast-update expr 'value m))
           ((quasiquote) (ast-update expr 'value m))
           ((unquote) (ast-update expr 'value m))
           ((unquote-splicing) (ast-update expr 'value m))
           ((list) (ast-update expr 'value (partial map m)))
           ((error) expr)
           (else (error "Unexpected expression: " expr)))))
      (error "Unexpected value: " expr)))

(define (ast-matches? expr pattern)
  (cond ((and (or (empty? pattern) (pair? pattern))
              (is-type? expr 'list))
         (ast-list-matches? (ast-get expr 'value) pattern))
        ((equal? pattern '_)
         #t)
        ((and (symbol? pattern) (is-type? expr 'symbol))
         (equal? pattern (ast-get expr 'value)))
        (else
         #f)))

(define (ast-list-matches? subexprs pattern)
  (cond ((and (empty? subexprs)
              (empty? pattern))
         #t)
        ((equal? pattern '_)
         #t)
        ((and (pair? pattern)
              (pair? subexprs))
         (and (ast-matches? (car subexprs) (car pattern))
              (ast-list-matches? (cdr subexprs) (cdr pattern))))
        (else
         #f)))

(define (ast->plain ast)
  (map-ast id
           (lambda (expr)
             (case (get-type expr)
               ((number) (ast-get expr 'value))
               ((symbol) (ast-get expr 'value))
               ((string) (ast-get expr 'value))
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
               ((list) (ast-get expr 'value))
               ((plain-quote) (list 'quote (ast-get expr 'value)))
               ((quasiquote) (list 'quasiquote (ast-get expr 'value)))
               ((unquote) (list 'unquote (ast-get expr 'value)))
               ((unquote-splicing) (list 'unquote-splicing (ast-get expr 'value)))
               (else (error "Unexpected expression: " expr))))
           ast))
