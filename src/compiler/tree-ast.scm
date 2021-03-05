;; AST

(load "compiler/utils.scm")

;; Basic definitions

(define (ast-node? node)
  (hash? node))

(define (ast-node . properties)
  (apply hasheq properties))

(define (ast-get node property default)
  (hash-ref node property default))

(define (ast-set node property value)
  (hash-set node property value))

(define (ast-update node property f)
  (ast-set node property (f (ast-get node property '()))))

;; AST nodes

(define (make-number-node value)
  (ast-node 'type 'number 'value value))

(define (make-symbol-node value)
  (ast-node 'type 'symbol 'value value))

(define (make-string-node value)
  (ast-node 'type 'string 'value value))

(define (make-quote-node value)
  (ast-node 'type 'plain-quote 'value value))

(define (make-quasiquote-node value)
  (ast-node 'type 'quasiquote 'value value))

(define (make-unquote-node value)
  (ast-node 'type 'unquote 'value value))

(define (make-unquote-splicing-node value)
  (ast-node 'type 'unquote-splicing 'value value))

(define (make-list-node value)
  (ast-node 'type 'list 'value value))

(define (make-error-node)
  (ast-node 'type 'error 'value "<error>"))

(define (make-unterminated-string-node value)
  (ast-node 'type 'unterminated-string 'value value))

(define (make-unterminated-list-node value)
  (ast-node 'type 'unterminated-list 'value value))

(define (make-unterminated-quote-node value)
  (ast-node 'type 'unterminated-quote 'value value))

(define (make-unmatched-token-node value)
  (ast-node 'type 'unmatched-token 'value value))

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
  (ast-get node 'location compiler-bug))

(define (at location object)
  (ast-set object 'location location))

(define (generated object)
  (ast-set object 'generated #t))

(define (map-ast pre post expr)
  (if (ast-node? expr)
      (let ((m (partial map-ast pre post))
            (expr (pre expr)))
        (post
         (case (ast-get expr 'type 'undefined)
           ('number expr)
           ('symbol expr)
           ('string expr)
           ('plain-quote (ast-update expr 'value m))
           ('quasiquote (ast-update expr 'value m))
           ('unquote (ast-update expr 'value m))
           ('unquote-splicing (ast-update expr 'value m))
           ('list (ast-update expr 'value (partial map m)))
           ('error expr)
           ('unmatched-token expr)
           ('unterminated-string expr)
           ('unterminated-quote expr)
           ('unterminated-list (ast-update expr 'value (partial map m)))
           (else (error "Unexpected expression: " expr)))))
      (error "Unexpected value: " expr)))

(define (ast->plain ast)
  (map-ast id
           (lambda (expr)
             (case (ast-get expr 'type 'undefined)
               ('plain-quote (list 'quote (ast-get expr 'value '())))
               ('quasiquote (list 'quasiquote (ast-get expr 'value '())))
               ('unquote (list 'unquote (ast-get expr 'value '())))
               ('unquote-splicing (list 'unquote-splicing (ast-get expr 'value '())))
               (else (ast-get expr 'value '()))))
           ast))
