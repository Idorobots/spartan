;; AST handling routines

(load "compiler/utils.scm")

(define (walk pre post expression)
  (let ((w (partial walk pre post))
        (expr (pre expression)))
    (post
     (cond ((symbol? expr) expr)
           ((simple? expr) expr)
           ((do? expr) (make-do (map w (do-statements expr))))
           ((if? expr) (make-if (w (if-predicate expr))
                                (w (if-then expr))
                                (w (if-else expr))))
           ((lambda? expr) (make-lambda (map w (lambda-args expr))
                                        (w (lambda-body expr))))
           ((application? expr) (make-app (w (app-op expr))
                                          (map w (app-args expr))))
           ((let? expr) (make-let (map (partial map w)
                                       (let-bindings expr))
                                  (w (let-body expr))))
           ((letrec? expr) (make-letrec (map (partial map w)
                                             (letrec-bindings expr))
                                        (w (letrec-body expr))))
           ((fix? expr) (make-fix (map (partial map w)
                                       (fix-bindings expr))
                                  (w (fix-body expr))))
           (else (error "Unexpected expression: " expr))))))

(define +syntax-keys+
  '(quote
    if
    lambda
    quasiquote
    unquote
    unquote-splicing
    do
    let
    letrec
    fix))

(define (simple? expr)
  (or (nil? expr)
      (number? expr)
      (string? expr)
      (char? expr)
      (vector? expr)
      (quote? expr)))

(define (value? expr)
  (or (lambda? expr)
      (simple? expr)))

;; ((var val) ...)
(define binding-var car)

(define binding-val cadr)

(define (bindings-vars bindings)
  (map binding-var bindings))

(define (bindings-vals bindings)
  (map binding-val bindings))

;; (quote expr)
(define (quote? expr)
  (tagged-list? 'quote expr))

(define (make-quote expr)
  `',expr)

(define (quoted-expr expr)
  (cadr expr))

;; (lambda (args ...) body)
(define (lambda? expr)
  (tagged-list? 'lambda expr))

(define (make-lambda args body)
  `(lambda ,args
     ,body))

(define (make-lambda-0 body)
  `(lambda ()
     ,body))

(define (make-lambda-1 arg body)
  `(lambda (,arg)
     ,body))

(define (make-lambda-2 arg0 arg1 body)
  `(lambda (,arg0 ,arg1)
     ,body))

(define (lambda-args expr)
  (cadr expr))

(define (lambda-body* expr)
  (cddr expr))

(define (lambda-body expr)
  (car (lambda-body* expr)))

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

(define (let-bindings expr)
  (cadr expr))

(define (let-body* expr)
  (cddr expr))

(define (let-body expr)
  (car (let-body* expr)))

;; (letrec ((variable value) ...) body)
(define (letrec? expr)
  (tagged-list? 'letrec expr))

(define (make-letrec bindings body)
  `(letrec ,bindings
     ,body))

(define letrec-bindings let-bindings)

(define letrec-body let-body)

;; (fix ((variable value) ...) body)
(define (fix? expr)
  (tagged-list? 'fix expr))

(define (make-fix bindings body)
  `(fix ,bindings
        ,body))

(define fix-bindings let-bindings)

(define fix-body let-body)

;; (operator args ...)
(define (application? expr)
  (and (list? expr)
       (not-nil? expr)
       (not (member (car expr)
                    +syntax-keys+))))

(define (primop? symbol)
  (and (symbol? symbol)
       (equal? #\& (car (string->list (symbol->string symbol))))))

(define (primop-application? expr)
  (and (application? expr)
       (primop? (app-op expr))))

(define (make-app op args)
  `(,op ,@args))

(define (make-app-0 op)
  `(,op))

(define (make-app-1 op arg)
  `(,op ,arg))

(define (app-op expr)
  (car expr))

(define (app-args expr)
  (cdr expr))
