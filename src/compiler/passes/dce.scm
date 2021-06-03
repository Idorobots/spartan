;; Dead code elimination.

(load-once "compiler/utils/utils.scm")
(load-once "compiler/utils/set.scm")

(load-once "compiler/env.scm")
(load-once "compiler/pass.scm")
(load-once "compiler/ast.scm")
(load-once "compiler/propagate.scm") ;; NOTE For reconstruct-*-node

(define eliminate-dead-code
  (pass (schema "eliminate-dead-code"
                'ast (ast-subset? '(const symbol if do let letrec fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (partial dce (set))))))

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
