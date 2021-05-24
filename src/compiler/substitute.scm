;; Binding aware variable substitution.

(load-once "compiler/utils/utils.scm")
(load-once "compiler/utils/set.scm")

(define (substitute-symbols subs expr)
  (substitute (lambda (subs expr kont)
                (if (ast-symbol? expr)
                    (apply-sub subs
                               (ast-symbol-value expr)
                               expr
                               (constantly expr))
                    (kont expr)))
              subs
              expr))

(define (substitute f subs expr)
  (if (empty-subs? subs)
      expr
      (f subs
         expr
         (lambda (expr)
           (case (ast-node-type expr)
             ((const)
              expr)
             ((lambda)
              (set-ast-lambda-body expr
                                   (substitute f
                                               (filter-subs subs
                                                            (ast-node-bound-vars expr))
                                               (ast-lambda-body expr))))
             ((let)
              (let ((unbound-subs (filter-subs subs
                                               (ast-node-bound-vars expr))))
                (-> expr
                    (set-ast-let-body (substitute f unbound-subs (ast-let-body expr)))
                    (set-ast-let-bindings (map (partial substitute f subs)
                                               (ast-let-bindings expr))))))
             ((letrec fix)
              (let ((unbound-subs (filter-subs subs
                                               (ast-node-bound-vars expr))))
                (-> expr
                    (set-ast-letrec-body (substitute f unbound-subs (ast-letrec-body expr)))
                    (set-ast-letrec-bindings (map (partial substitute f unbound-subs)
                                                  (ast-letrec-bindings expr))))))
             ((binding)
              (set-ast-binding-val expr (substitute f subs (ast-binding-val expr))))
             (else
              (walk-ast (partial substitute f subs) expr)))))))

(define (make-subs assocs)
  (make-immutable-hasheq assocs))

(define (extend-subs assocs subs)
  (make-subs (append assocs
                     (hash->list subs))))

(define (empty-subs? subs)
  (hash-empty? subs))

(define (filter-subs subs vars)
  (foldl (lambda (var subs)
           (hash-remove subs var))
         subs
         (set->list vars)))

(define (apply-sub subs name value default)
  (if (hash-has-key? subs name)
      ((hash-ref subs name) value)
      (default)))

(define (replace-sub subs name default)
  (if (hash-has-key? subs name)
      (hash-ref subs name)
      (default)))
