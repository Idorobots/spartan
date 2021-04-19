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
  (ast-case expr
   ((symbol _)
    (let ((s (assoc (ast-symbol-value expr) subs)))
      (if s
          (cdr s) ;; NOTE Completely replaces the expr, together with its location.
          expr)))
   ((lambda _ _)
    (ast-update expr
                'body
                (partial constant-propagation
                         (filter-subs subs
                                      (get-bound-vars expr)))))
   ((let ,bindings _)
    (let* ((consts (filter (compose (partial equal? 'simple) get-complexity) bindings))
           (new-subs (map (lambda (b)
                            (cons (ast-symbol-value (ast-binding-var b))
                                  (ast-binding-val b)))
                          consts))
           (updated-subs (append new-subs
                                 (filter-subs subs
                                              (get-bound-vars expr))))
           (updated-bindings (map (partial constant-propagation subs)
                                  (filter (compose not (partial equal? 'simple) get-complexity)
                                          bindings))))
      (ast-update (ast-update expr 'bindings (constantly updated-bindings))
                  'body
                  (partial constant-propagation updated-subs))))
   ((letrec ,bindings _)
    (let* ((consts (filter (compose (partial equal? 'simple) get-complexity) bindings))
           (new-subs (map (lambda (b)
                            (cons (ast-symbol-value (ast-binding-var b))
                                  (ast-binding-val b)))
                          consts))
           (updated-subs (append new-subs
                                 (filter-subs subs
                                              (get-bound-vars expr))))
           (updated-bindings (map (partial constant-propagation updated-subs)
                                  (filter (compose not (partial equal? 'simple) get-complexity)
                                          bindings))))
      (ast-update (ast-update expr 'bindings (constantly updated-bindings))
                  'body
                  (partial constant-propagation updated-subs))))
   ((binding _ _)
    (ast-update expr 'val (partial constant-propagation subs)))
   (else
    (walk-ast (partial constant-propagation subs) expr))))
