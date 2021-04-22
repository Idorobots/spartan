;; Constant propagation.

(load "compiler/utils/utils.scm")
(load "compiler/substitute.scm") ;; FIXME For filter subs.

(load "compiler/env.scm")
(load "compiler/pass.scm")
(load "compiler/ast.scm")

(define propagate-constants
  (pass (schema "propagate-constants"
                'ast (ast-subset? '(const symbol
                                    if do let letrec binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (partial constant-propagation '())))))

(define (constant-propagation subs expr)
  (propagate const-binding?
             make-const-sub
             (lambda (subs expr kont)
               (ast-case expr
                ((symbol _)
                 (let ((s (assoc (ast-symbol-value expr) subs)))
                   (if s
                       (cdr s) ;; NOTE Completely replaces the expr, together with its location.
                       (kont expr))))
                (else
                 (kont expr))))
             subs
             expr))

(define (propagate partition-by make-sub replace-with subs expr)
  (define (loop subs expr)
    (replace-with subs
                  expr
                  (lambda (expr)
                    (ast-case expr
                     ((lambda _ _)
                      (ast-update expr
                                  'body
                                  (partial loop
                                           (filter-subs subs
                                                        (get-bound-vars expr)))))
                     ((let ,bindings _)
                      (let* ((bs (partition-bindings partition-by bindings))
                             (updated-subs (append (map make-sub (car bs))
                                                   (filter-subs subs (get-bound-vars expr))))
                             (updated-bindings (map (partial loop subs)
                                                    (cdr bs))))
                        (ast-update (ast-update expr 'bindings (constantly updated-bindings))
                                    'body
                                    (partial loop updated-subs))))
                     ((letrec ,bindings _)
                      (let* ((bs (partition-bindings partition-by bindings))
                             (updated-subs (append (map make-sub (car bs))
                                                   (filter-subs subs (get-bound-vars expr))))
                             (updated-bindings (map (partial loop updated-subs)
                                                    (cdr bs))))
                        (ast-update (ast-update expr 'bindings (constantly updated-bindings))
                                    'body
                                    (partial loop updated-subs))))
                     ((binding _ _)
                      (ast-update expr 'val (partial loop subs)))
                     (else
                      (walk-ast (partial loop subs) expr))))))
  (loop subs expr))

(define (const-binding? binding)
  (const-node? (ast-binding-val binding)))

(define (make-const-sub binding)
  (cons (ast-symbol-value (ast-binding-var binding))
        (ast-binding-val binding)))

(define (partition-bindings pred bindings)
  (foldr (lambda (b acc)
           (if (pred b)
               (cons (cons b (car acc))
                     (cdr acc))
               (cons (car acc)
                     (cons b (cdr acc)))))
         (cons '() '())
         bindings))
