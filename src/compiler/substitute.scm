;; Binding aware variable substitution.

(load "compiler/utils/utils.scm")
(load "compiler/utils/set.scm")

(define (substitute-symbols subs expr)
  (substitute (lambda (subs expr kont)
                (if (symbol-node? expr)
                    (let ((s (assoc (ast-symbol-value expr) subs)))
                      (if s
                          ((cdr s) expr)
                          expr))
                    (kont expr)))
              subs
              expr))

(define (substitute f subs expr)
  (if (empty? subs)
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

(define (filter-subs subs vars)
  (filter (lambda (s)
            (not (set-member? vars (car s))))
          subs))
