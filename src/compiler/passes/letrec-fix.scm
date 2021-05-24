;; Fixing-letrec-like letrec conversion.

(load-once "compiler/utils/utils.scm")

(load-once "compiler/substitute.scm")
(load-once "compiler/env.scm")
(load-once "compiler/pass.scm")
(load-once "compiler/ast.scm")
(load-once "compiler/propagate.scm") ;; FIXME For reconstruct-*-node

(define fix-letrec
  (pass (schema "fix-letrec"
                'ast (ast-subset? '(const symbol
                                          if do let letrec binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast fixing-letrec))))

(define (fixing-letrec expr)
  (map-ast (lambda (expr)
             (ast-case expr
                       ((letrec ,bindings ,body)
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
  (let* ((simple (filter (compose (partial equal? 'simple) get-complexity)
                         bindings))
         (lambdas (filter (compose (partial equal? 'lambda) get-complexity)
                          bindings))
         (complex (filter (compose (partial equal? 'complex) get-complexity)
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
                                (ast-update (let-void-set parent complex body)
                                            'body
                                            lambdas-builder)))))
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
                          (at (ast-node-location b)
                              (complexity
                               'simple
                               (make-binding-node
                                (ast-binding-var b)
                                (let* ((val (ast-binding-val b))
                                       (val-loc (ast-node-location val)))
                                  (at val-loc
                                      (make-primop-app-node 'ref
                                                            (list (at val-loc
                                                                      (generated
                                                                       (make-const-node
                                                                        (at val-loc
                                                                            (generated
                                                                             (make-list-node '()))))))))))))))
                        bindings))
             (setters (map (lambda (b)
                             (let ((val (derefy vars (ast-binding-val b)))
                                   (var (ast-binding-var b)))
                               (set-ast-node-free-vars (set-insert (ast-node-free-vars val) (safe-symbol-value var))
                                                       (at (ast-node-location val)
                                                           (make-primop-app-node 'assign! (list var val))))))
                           bindings))
             (body (derefy vars body)))
        (generated
         (reconstruct-let-node parent
                               refs
                               (if (empty? setters)
                                   body
                                   (set-ast-node-free-vars (set-union (ast-node-free-vars body)
                                                                      (set-sum (map ast-node-free-vars setters)))
                                                           (at (ast-node-location body)
                                                               (generated
                                                                (make-do-node (append setters (list body))))))))))))

(define (derefy refs expr)
  (substitute-symbols
   (make-subs
    (map (lambda (ref)
           (cons ref
                 (lambda (expr)
                   (set-ast-node-free-vars (set ref)
                                           (at (ast-node-location expr)
                                               (make-primop-app-node 'deref (list expr)))))))
         refs))
   expr))
