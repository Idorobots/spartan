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

(load "compiler/ast.scm")

;; FIXME Rewrite in terms of ast/walk.
(define (old-substitute subs expr)
  (cond ((empty? subs) expr)
        ((simple? expr) expr)
        ((symbol? expr) (let ((a (assoc expr subs)))
                          (if a
                              (cdr a)
                              expr)))
        ((lambda? expr) (let ((vars (lambda-args expr)))
                          (make-lambda vars
                                       (old-substitute (filter-subs subs vars)
                                                       (lambda-body expr)))))
        ((let? expr) (let* ((bindings (let-bindings expr))
                            (subbed-bindings (map (lambda (b)
                                                    (list (car b)
                                                          (old-substitute subs (cadr b))))
                                                  bindings)))
                       (make-let subbed-bindings
                                 (old-substitute (filter-subs subs (bindings-vars bindings))
                                                 (let-body expr)))))
        ((or (letrec? expr)
             (fix? expr)) (let* ((bindings (let-bindings expr))
                                 (unbound-subs (filter-subs subs (bindings-vars bindings)))
                                 (subbed-bindings (map (lambda (b)
                                                         (list (car b)
                                                               (old-substitute unbound-subs (cadr b))))
                                                       bindings)))
                            (make-letrec subbed-bindings
                                         (old-substitute unbound-subs (let-body expr)))))
        ((do? expr) (make-do (map (lambda (s)
                                    (old-substitute subs s))
                                  (do-statements expr))))
        ((if? expr) (make-if (old-substitute subs (if-predicate expr))
                             (old-substitute subs (if-then expr))
                             (old-substitute subs (if-else expr))))
        ((application? expr) (make-app (old-substitute subs (app-op expr))
                                       (map (lambda (a)
                                              (old-substitute subs a))
                                            (app-args expr))))
        (else expr)))
