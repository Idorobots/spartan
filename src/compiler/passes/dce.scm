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
  (ast-case expr
   ;; NOTE These are introduced by CPC.
   ((let ((binding ,var ,val)) ,var)
    (dce (set) val))
   ;; NOTE Eta reduction.
   ((lambda ,args (app ,op . ,args))
    (if (and (symbol-node? op)
             (set-member? eta-disallow (ast-symbol-value op)))
        (walk-ast (partial dce (set)) expr)
        (dce (set) op)))
   ((lambda ,args (primop-app '&yield-cont ,cont . ,args))
    (dce (set) cont))
   ((letrec ,bindings _)
    (disallow-recoursive-eta expr))
   ((fix ,bindings _)
    (disallow-recoursive-eta expr))
   ;; Actual dead code elimination
   ((do . ,exprs)
    (let ((final (last exprs))
          (filtered (filter effectful?
                            (take exprs (- (length exprs) 1)))))
      (if (empty? filtered)
          (dce (set) final)
          (replace expr
                   (make-do-node
                    (map (partial dce (set))
                         (append filtered
                                 (list final))))))))
   ((if ,condition ,then ,else)
    (cond ((falsy? condition) (dce (set) else))
          ((truthy? condition) (dce (set) then))
          (else (walk-ast (partial dce (set)) expr))))
   ((let ,bindings ,body)
    (let* ((free (get-free-vars body))
           (filtered (filter (lambda (b)
                               (or (effectful? (ast-binding-val b))
                                   (not (set-empty? (set-intersection (get-bound-vars b)
                                                                      free)))))
                             bindings)))
      (reconstruct-let-node expr
                            (map (partial dce (set)) filtered)
                            (dce (set) body))))
   (else
    (walk-ast (partial dce (set)) expr))))

(define (disallow-recoursive-eta expr)
  (ast-update (ast-update expr 'body (partial dce (set)))
              'bindings
              (lambda (bs)
                ;; NOTE Disallows eta-reduction on self-recoursive functions.
                (map (lambda (b)
                       (walk-ast (partial dce (get-bound-vars expr))
                                 b))
                     bs))))

(define (effectful? node)
  (not (or (const-node? node)
           (symbol-node? node)
           (lambda-node? node))))

(define (falsy? node)
  ;; FIXME Implement proper booleans.
  (and (const-node? node)
       (list-node? (ast-const-value node))
       (equal? 0 (ast-list-length (ast-const-value node)))))

(define (truthy? node)
  (and (not (falsy? node))
       (or (const-node? node)
           (lambda-node? node))))
