;; Binding aware variable substitution.

(load-once "compiler/utils/utils.scm")
(load-once "compiler/utils/set.scm")

(define (substitute-symbols subs expr)
  (substitute (lambda (subs expr kont)
                (if (symbol-node? expr)
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
           (case (get-type expr)
             ((const)
              expr)
             ((lambda)
              (ast-update expr
                          'body (partial substitute
                                         f
                                         (filter-subs subs
                                                      (get-bound-vars expr)))))
             ((let)
              (let ((unbound-subs (filter-subs subs
                                               (get-bound-vars expr))))
                (ast-update (ast-update expr 'body (partial substitute f unbound-subs))
                            'bindings
                            (partial map (partial substitute f subs)))))
             ((letrec fix)
              (let ((unbound-subs (filter-subs subs
                                               (get-bound-vars expr))))
                (ast-update (ast-update expr 'body (partial substitute f unbound-subs))
                            'bindings
                            (partial map (partial substitute f unbound-subs)))))
             ((binding)
              (ast-update expr 'val (partial substitute f subs)))
             (else
              (walk-ast (partial substitute f subs) expr)))))))

(define (apply-sub subs name value default)
  (if (hash-has-key? subs name)
      ((hash-ref subs name) value)
      (default)))

(define (empty-subs? subs)
  (= (hash-count subs) 0))

(define (make-subs assocs)
  (make-immutable-hasheq assocs))

(define (filter-subs subs vars)
  (foldl (lambda (var subs)
           (hash-remove subs var))
         subs
         vars))
