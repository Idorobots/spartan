;; AST handling routines

(load "compiler/utils.scm")

(define (walk pre post expression)
  (let ((w (partial walk pre post))
        (expr (pre expression)))
    (post
     (cond ((symbol? expr) expr)
           ((number? expr) expr)
           ((string? expr) expr)
           ((vector? expr) expr)
           ((nil? expr) expr)
           ((char? expr) expr)
           ((quote? expr) expr)
           ((do? expr) (make-do (map w (do-statements expr))))
           ((if? expr) (make-if (w (if-predicate expr))
                                (w (if-then expr))
                                (w (if-else expr))))
           ((value-define? expr) (make-define-1 (w (define-name expr))
                                          (w (define-value expr))))
           ((define? expr) (make-define (w (define-name expr))
                                        (map w (define-args expr))
                                        (w (define-body expr))))
           ((set!? expr) (make-set! (w (set!-var expr))
                                    (w (set!-val expr))))
           ((lambda? expr) (make-lambda (map w (lambda-args expr))
                                        (make-do (map w (lambda-body expr)))))
           ((let? expr) (make-let (map (partial map w)
                                       (let-bindings expr))
                                  (make-do (map w (let-body expr)))))
           ((letrec? expr) (make-letrec (map (partial map w)
                                             (let-bindings expr))
                                        (make-do (map w (let-body expr)))))
           ((letcc? expr) (make-letcc (w (let-bindings expr))
                                      (make-do (map w (let-body expr)))))
           ((reset? expr) (make-reset (w (reset-expr expr))))
           ((shift? expr) (make-shift (w (shift-cont expr))
                                      (w (shift-expr expr))))
           ((handle? expr) (make-handle (w (handle-expr expr))
                                        (w (handle-handler expr))))
           ((raise? expr) (make-raise (w (raise-expr expr))))
           ((application? expr) (make-app (w (app-op expr))
                                          (map w (app-args expr))))
           ((module? expr) (make-module (w (module-name expr))
                                        (map w (module-deps expr))
                                        (map w (module-body expr))))
           ((structure? expr) (make-structure (map w (structure-defs expr))))
           ('else (error "Unexpected expression: " expr))))))

(define +syntax-keys+
  '(quote
    if
    lambda
    quasiquote
    unquote
    unquote-splicing
    define
    do
    let
    letrec
    letcc
    set!
    reset
    shift
    handle
    raise
    module
    structure))

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
  `(letcc ,variable
     ,body))

(define (let-bindings expr)
  (cadr expr))

(define (let-body expr)
  (cddr expr))

;; Mutation:
(define (set!? expr)
  (tagged-list? 'set! expr))

(define (make-set! variable value)
  `(set! ,variable ,value))

(define (set!-var expr)
  (cadr expr))

(define (set!-val expr)
  (caddr expr))

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
  (and (list? expr)
       (not-nil? expr)
       (not (member (car expr)
                    +syntax-keys+))))

(define (make-app op args)
  `(,op ,@args))

(define (make-app-1 op arg)
  `(,op ,arg))

(define (app-op expr)
  (car expr))

(define (app-args expr)
  (cdr expr))

;; (handle expr hadler)
(define (handle? expr)
  (tagged-list? 'handle expr))

(define (make-handle expr handler)
  `(handle ,expr
           ,handler))

(define (handle-expr expr)
  (cadr expr))

(define (handle-handler expr)
  (caddr expr))

;; (raise error)
(define (raise? expr)
  (tagged-list? 'raise expr))

(define (make-raise expr)
  `(raise ,expr))

(define (raise-expr expr)
  (cadr expr))

;; (module (name deps ...) body ...)
(define (module? expr)
  (tagged-list? 'module expr))

(define (module-name expr)
  (caadr expr))

(define (module-deps expr)
  (cdadr expr))

(define (module-body expr)
  (cddr expr))

(define (make-module name deps body)
  `(module (,name ,@deps) ,@body))

;; (structure defs ...)
(define (structure? expr)
  (tagged-list? 'structure expr))

(define (structure-defs expr)
  (cdr expr))

(define (make-structure defs)
  `(structure ,@defs))
