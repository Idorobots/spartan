#lang racket

;; Fixing-letrec-like letrec conversion.

(require "../utils/utils.rkt")
(require "../utils/set.rkt")

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require "../substitute.rkt")
(require (only-in "../propagate.rkt"
                  reconstruct-let-node reconstruct-fix-node))

(provide fix-letrec
         ;; FIXME For test access.
         waddell let-ref-assign derefy)

(define fix-letrec
  (pass (schema "fix-letrec"
                'ast (ast-subset? '(const symbol if do let letrec binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast fixing-letrec))
        (schema "fix-letrec output"
                'ast (ast-subset? '(const symbol if do let fix binding lambda app primop-app)))))

(define (fixing-letrec expr)
  (map-ast (lambda (expr)
             (match-ast expr
              ((letrec bindings body)
               (replace expr
                        (waddell reconstruct-fix-node let-ref-assign expr bindings body)))
              (else
               expr)))
           expr))

;; This conversion distributes the bindings into three groups - simple, lambdas & complex, and converts them accordingly:

;; (letrec ((simple 23)
;;          (complex (func simple))
;;          (func (lambda (x) (+ x simple))))
;;   complex)

;; Simple bindings can be pulled out into a wrapping let expression.

;; (let ((simple 23))
;;   (letrec ((complex (func simple))
;;            (func (lambda (x) (+ x simple))))
;;     complex))

;; Lambdas are converted into a fix expression that can be efficiently handled later.

;; (let ((simple 23))
;;   (letrec ((complex (func simple)))
;;     (fix ((func (lambda (x) (+ x simple))))
;;          complex)))

;; Complex bindings are converted using the let-ref-assign method.

;; (let ((simple 23))
;;   (let ((complex (ref '())))
;;     (fix ((func (lambda (x) (+ x simple))))
;;       (assign complex (func simple))
;;       (deref complex)))

;; The fix expression can be handled directly during the closure conversion phase by allocating a fat closure for all of these functions.

(define (waddell fix let-void-set parent bindings body)
  (let* ((simple (filter (compose (partial equal? 'simple) ast-binding-complexity)
                         bindings))
         (lambdas (filter (compose (partial equal? 'lambda) ast-binding-complexity)
                          bindings))
         (complex (filter (compose (partial equal? 'complex) ast-binding-complexity)
                          bindings))
         (lambdas-builder (if (empty? lambdas)
                              id
                              (compose generated
                                       (if (recursive? lambdas)
                                           (partial fix parent lambdas)
                                           (partial reconstruct-let-node parent lambdas)))))
         (complex-builder (if (empty? complex)
                              lambdas-builder
                              (lambda (body)
                                (let ((inner (let-void-set parent complex body)))
                                  (set-ast-let-body inner
                                                    (lambdas-builder (ast-let-body inner))))))))
    (generated
     (reconstruct-let-node parent
                           simple
                           (complex-builder body)))))

;; This conversion relies on boxing & assignments to implement assignment conversion on the variables that require it.

(define (let-ref-assign parent bindings body)
  (if (empty? bindings)
      body
      (let* ((vars (map (compose safe-symbol-value ast-binding-var) bindings))
             (refs (map (lambda (b)
                          (set-ast-binding-complexity
                           (make-ast-binding (ast-node-location b)
                                             (ast-binding-var b)
                                             (let* ((val (ast-binding-val b))
                                                    (val-loc (ast-node-location val)))
                                               (make-ast-primop-app val-loc
                                                                    'ref
                                                                    (list (generated
                                                                           (make-ast-const val-loc
                                                                                           (generated
                                                                                            (make-ast-list val-loc '()))))))))
                           'simple))
                        bindings))
             (setters (map (lambda (b)
                             (let ((val (derefy vars (ast-binding-val b)))
                                   (var (ast-binding-var b)))
                               (set-ast-node-free-vars (set-insert (ast-node-free-vars val) (safe-symbol-value var))
                                                       (make-ast-primop-app (ast-node-location val)
                                                                            'assign!
                                                                            (list var val)))))
                           bindings))
             (body (derefy vars body)))
        (generated
         (reconstruct-let-node parent
                               refs
                               (if (empty? setters)
                                   body
                                   (set-ast-node-free-vars (set-union (ast-node-free-vars body)
                                                                      (set-sum (map ast-node-free-vars setters)))
                                                           (generated
                                                                (make-ast-do (ast-node-location body)
                                                                             (append setters (list body)))))))))))

(define (derefy refs expr)
  (substitute-symbols
   (make-subs
    (map (lambda (ref)
           (cons ref
                 (lambda (expr)
                   (set-ast-node-free-vars (set ref)
                                           (make-ast-primop-app (ast-node-location expr)
                                                                'deref
                                                                (list expr))))))
         refs))
   expr))
