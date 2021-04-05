;; Binding aware variable substitution.

(load "compiler/utils/utils.scm")

(define (substitute subs expr)
  (if (empty? subs)
      expr
      (case (get-type expr)
        ((symbol)
         (let ((s (assoc (safe-symbol-value expr) subs)))
           (if s
               ((cdr s) expr)
               expr)))
        ((lambda)
         (ast-update expr
                     'body (partial substitute
                                    (filter-subs subs
                                                 (get-bound-vars expr)))))
        ((let)
         (let ((unbound-subs (filter-subs subs
                                          (get-bound-vars expr))))
           (ast-update (ast-update expr 'body (partial substitute unbound-subs))
                       'bindings
                       (partial map (partial substitute subs)))))
        ((letrec fix)
         (let ((unbound-subs (filter-subs subs
                                          (get-bound-vars expr))))
           (ast-update (ast-update expr 'body (partial substitute unbound-subs))
                       'bindings
                       (partial map (partial substitute unbound-subs)))))
        ((binding)
         (ast-update expr 'val (partial substitute subs)))
        (else
         (walk-ast (partial substitute subs) expr)))))

(define (filter-subs subs vars)
  (filter (lambda (s)
            (not (set-member? vars (car s))))
          subs))
