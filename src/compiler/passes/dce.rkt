#lang racket

;; Dead code elimination.

(require "../utils/utils.rkt")
(require "../utils/set.rkt")

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require (only-in "../propagate.rkt"
                  reconstruct-let-node reconstruct-letrec-node reconstruct-fix-node))

(provide eliminate-dead-code
         ;; FIXME For test access.
         dce effectful? truthy? falsy?)

(define eliminate-dead-code
  (pass (schema "eliminate-dead-code"
                'ast (ast-subset? '(const symbol if do let letrec fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (partial dce (set))))
        (schema "eliminate-dead-code output"
                'ast (ast-subset? '(const symbol if do let letrec fix binding lambda app primop-app)))))

(define (dce eta-disallow expr)
  (match-ast expr
   ;; NOTE These are introduced by CPC.
   ((let ((binding var val)) var)
    (dce (set) val))
   ;; NOTE Eta reduction.
   ((lambda formals (app op args ...))
    #:when (ast-list-eqv? formals args)
    (if (and (ast-symbol? op)
             (set-member? (set-union eta-disallow
                                     ;; NOTE Or else (letcc k (k k)) eta reduces to k, which is then undefined.
                                     (ast-node-bound-vars expr))
                          (ast-symbol-value op)))
        (traverse-ast dce (set) expr)
        (dce (set) op)))
   ((lambda formals (primop-app '&yield-cont cont args ...))
    #:when (ast-list-eqv? formals args)
    (dce (set) cont))
   ;; Actual dead code elimination
   ((do exprs ...)
    (let ((final (last exprs))
          (filtered (filter effectful?
                            (take exprs (- (length exprs) 1)))))
      (if (empty? filtered)
          (dce (set) final)
          (replace expr
                   (make-ast-do (ast-node-location expr)
                                (map (partial dce (set))
                                     (append filtered
                                             (list final))))))))

   ;; NOTE This is purely to facilitate:
   ;; (if (and a b) t e) => (if (if a b false) t e) => (if a (if b t e) (if false t e)) => (if a (if b t e) e)
   ;; (if (or a b) t e) => (if (if a true b) t e) => (if a (if true t e) (if b t e)) => (if a t (if b t e))
   ((if (if a b c) t e)
    (let ((tt (dce (set) t))
          (te (dce (set) e)))
      (replace expr
             (make-ast-if (ast-node-location expr)
                          (dce (set) a)
                          (make-ast-if (ast-node-location b)
                                       (dce (set) b)
                                       tt
                                       te)
                          (make-ast-if (ast-node-location c)
                                       (dce (set) c)
                                       tt
                                       te)))))
   ((if condition then else)
    (cond ((falsy? condition) (dce (set) else))
          ((truthy? condition) (dce (set) then))
          (else (traverse-ast dce (set) expr))))

   ((let bindings body)
    (let* ((free (ast-node-free-vars body))
           (filtered (filter (flip used? free) bindings)))
      (reconstruct-let-node expr
                            (map (partial dce (set)) filtered)
                            (dce (set) body))))
   ((letrec bindings body)
    (let ((filtered (filter (lambda (b)
                              (used? b (set-union (ast-node-free-vars body)
                                                  (set-sum (map ast-node-free-vars
                                                                (filter (compose not (partial equal? b))
                                                                        bindings))))))
                            bindings)))
      (reconstruct-letrec-node expr
                               (map (lambda (b)
                                      (traverse-ast dce (ast-node-bound-vars expr) b))
                                    filtered)
                               (dce (set) body))))
   ((fix bindings body)
    (let ((filtered (filter (lambda (b)
                              (used? b (set-union (ast-node-free-vars body)
                                                  (set-sum (map ast-node-free-vars
                                                                (filter (compose not (partial equal? b))
                                                                        bindings)))))) bindings)))
      (reconstruct-fix-node expr
                            (map (lambda (b)
                                   (traverse-ast dce (ast-node-bound-vars expr) b))
                                 filtered)
                            (dce (set) body))))
   (else
    (traverse-ast dce (set) expr))))

(define (effectful? node)
  (not (or (ast-const? node)
           (ast-symbol? node)
           (ast-lambda? node))))

(define (falsy? node)
  (and (ast-symbol? node)
       (equal? 'false (ast-symbol-value node))))

(define (truthy? node)
  (or (ast-const? node)
      (ast-lambda? node)
      (and (ast-symbol? node)
           (equal? 'true (ast-symbol-value node)))))

(define (used? b free)
  (or (effectful? (ast-binding-val b))
      (not (set-empty? (set-intersection (ast-node-bound-vars b)
                                         free)))))
