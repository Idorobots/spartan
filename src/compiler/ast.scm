;; AST handling routines

(load "compiler/utils.scm")

(define (walk pre post expression)
  (let ((w (partial walk pre post))
        (expr (pre expression)))
    (post
     (cond ((symbol? expr) expr)
           ((simple? expr) expr)
           ((quasiquote? expr) (make-quasiquote (w (quoted-expr expr))))
           ((unquote? expr) (make-unquote (w (quoted-expr expr))))
           ((unquote-splicing? expr) (make-unquote-splicing (w (quoted-expr expr))))
           ((do? expr) (make-do (map w (do-statements expr))))
           ((if? expr) (make-if (w (if-predicate expr))
                                (w (if-then expr))
                                (w (if-else expr))))
           ((value-define? expr) (make-val-define (w (define-name expr))
                                                  (w (define-value expr))))
           ((define? expr) (make-define (w (define-name expr))
                                        (map w (define-args expr))
                                        (w (define-body expr))))
           ((set!? expr) (make-set! (w (set!-var expr))
                                    (w (set!-val expr))))
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
           ((module? expr) (make-module (w (module-name expr))
                                        (map w (module-deps expr))
                                        (map w (module-body expr))))
           ((structure? expr) (make-structure (map w (structure-defs expr))))
           (else (error "Unexpected expression: " expr))))))

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
    fix
    set!
    module
    structure))

(define (simple? expr)
  (or (nil? expr)
      (number? expr)
      (string? expr)
      (char? expr)
      (vector? expr)
      (quote? expr)))

(define (value? expr)
  (or (lambda? expr)
      (simple? expr)
      (structure? expr)))

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

;; (quasiquote expr)
(define (quasiquote? expr)
  (tagged-list? 'quasiquote expr))

(define (make-quasiquote expr)
  (list 'quasiquote expr))

;; (unquote expr)
(define (unquote? expr)
  (tagged-list? 'unquote expr))

(define (make-unquote expr)
  (list 'unquote expr))

;; (unquote-splicing expr)
(define (unquote-splicing? expr)
  (tagged-list? 'unquote-splicing expr))

(define (make-unquote-splicing expr)
  (list 'unquote-splicing expr))

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

;; (define (name args ...) body)
;; (define name value)
(define (define? expr)
  (tagged-list? 'define expr))

(define (value-define? expr)
  (and (define? expr) (symbol? (cadr expr))))

(define (make-define name args body)
  `(define (,name ,@args)
     ,body))

(define (make-val-define name value)
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

(define (define-body* expr)
  (cddr expr))

(define (define-body expr)
  (car (define-body* expr)))

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

;; (letcc continuation body)
(define (letcc? expr)
  (tagged-list? 'letcc expr))

(define (make-letcc variable body)
  `(letcc ,variable
     ,body))

(define letcc-var let-bindings)

(define letcc-body let-body)

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

(define (primop-application? expr)
  (and (application? expr)
       ;; FIXME Don't rely on make-internal-applicatives
       (member (app-op expr) (make-internal-applicatives))))

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
