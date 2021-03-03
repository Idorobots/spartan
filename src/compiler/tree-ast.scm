;; AST

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

(define (parse-location start end)
  (ast-node 'start start 'end end))

(define (location module start-line start-col end-line end-col)
  (ast-node 'start-line start-line 'start-col start-col 'end-line end-line 'end-col end-col))

(define (get-location node)
  ;; NODE Location is flat within the node.
  node)

(define (location-errorer location)
  (lambda ()
    (error "Invalid location specified: " location)))

(define (at location object)
  (let* ((start (ast-set object 'start (ast-get location 'start (location-errorer location))))
         (end (ast-set start 'end (ast-get location 'end (location-errorer location)))))
    (if (ast-get location 'generated #f)
        (generated end)
        end)))

(define (generated object)
  (ast-set object 'generated #t))

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
  (generated
   (ast-node 'type 'error 'value "<error>")))

(define (make-unterminated-string-node value)
  (ast-node 'type 'unterminated-string 'value value))

(define (make-unterminated-list-node value)
  (ast-node 'type 'unterminated-list 'value value))

(define (make-unterminated-quote-node value)
  (ast-node 'type 'unterminated-quote 'value value))

(define (make-unmatched-token-node value)
  (ast-node 'type 'unmatched-token 'value value))

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
