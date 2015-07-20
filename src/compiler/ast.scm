;; AST handling routines

(load "compiler/utils.scm")

(define (simple-expression? expr)
  (or (symbol? expr)
     (number? expr)
     (string? expr)
     (vector? expr)
     (nil? expr)
     (char? expr)
     (quote? expr)
     (lambda? expr)))

;; (quote expr)
(define (quote? expr)
  (tagged-list? 'quote expr))

(define (make-quote expr)
  `',expr)

(define (quoted-expr expr)
  (cadr expr))

;; (lambda (args ...) body ...)
(define (lambda? expr)
  (tagged-list? 'lambda expr))

(define (make-lambda args body)
  `(lambda ,args
     ,body))

(define (make-lambda-1 arg body)
  `(lambda (,arg)
     ,body))

(define (make-lambda-2 arg0 arg1 body)
  `(lambda (,arg0 ,arg1)
     ,body))

(define (lambda-args expr)
  (cadr expr))

(define (lambda-body expr)
  (cddr expr))

;; (define (name args ...) body)
;; (define name value)
(define (define? expr)
  (tagged-list? 'define expr))

(define (value-define? expr)
  (and (define? expr) (symbol? (cadr expr))))

(define (make-define name args body)
  `(define (,name ,@args)
     ,body))

(define (make-define-1 name value)
  `(define ,name
     ,value))

(define (define-name expr)
  (if (value-define? expr)
      (cadr expr)
      (caadr expr)))

(define (define-value expr)
  (caddr expr))

(define (define-args expr)
  (cdadr expr))

(define (define-body expr)
  (let ((b (cddr expr)))
    (if (> (length b) 1)
        (make-do b)
        (car b))))

;; (do statements ...)
(define (do? expr)
  (tagged-list? 'do expr))

(define (make-do statements)
  (if (= (length statements) 1)
      (car statements)
      `(do ,@statements)))

(define (do-statements expr)
  (cdr expr))

(define (first-statement statements)
  (car statements))

(define (rest-statements statements)
  (cdr statements))

;; (if condition then else)
(define (if? expr)
  (tagged-list? 'if expr))

(define (make-if condition then else)
  `(if ,condition
       ,then
       ,else))

(define (if-predicate expr)
  (cadr expr))

(define (if-then expr)
  (caddr expr))

(define (if-else expr)
  (cadddr expr))

;; (let ((variable value) ...) body)
(define (let? expr)
  (tagged-list? 'let expr))

(define (make-let bindings body)
  `(let ,bindings
     ,body))

(define (make-let-1 variable value body)
  `(let ((,variable ,value))
     ,body))

;; (letrec ((variable value) ...) body)
(define (letrec? expr)
  (tagged-list? 'letrec expr))

(define (make-letrec bindings body)
  `(letrec ,bindings
     ,body))

;; (letcc continuation body)
(define (letcc? expr)
  (tagged-list? 'letcc expr))

(define (make-letcc variable body)
  `(let ,variable
     ,body))

(define (let-bindings expr)
  (cadr expr))

(define (let-body expr)
  (cddr expr))

;; (reset expression)
(define (reset? expr)
  (tagged-list? 'reset expr))

(define (make-reset expression)
  `(reset ,expression))

(define (reset-expr expr)
  (cadr expr))

;; (shift continuation expression)
(define (shift? expr)
  (tagged-list? 'shift expr))

(define (make-shift variable expression)
  `(shift ,variable
          ,expression))

(define (shift-cont expr)
  (cadr expr))

(define (shift-expr expr)
  (caddr expr))

;; (operator args ...)
(define (application? expr)
  (and (list? expr) (not-nil? expr)))

(define (make-app op args)
  `(,op ,@args))

(define (make-app-1 op arg)
  `(,op ,arg))

(define (app-op expr)
  (car expr))

(define (app-args expr)
  (cdr expr))
