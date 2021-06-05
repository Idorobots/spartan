;; Common subexpression elimination.

(require "../utils/utils.rkt")
(require "../utils/set.rkt")

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")

(define eliminate-common-subexpressions
  (pass (schema "eliminate-common-subexpressions"
                'ast (ast-subset? '(const symbol if do let letrec fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (partial cse '())))))

(define (cse subexprs expr)
  (match-ast expr
   ((lambda _ body)
    ;; NOTE CSE is performed locally within a procedure not to inflate closure envs too much,
    ;; NOTE so this essentially cuts of all the propagated expressions thus far.
    (set-ast-lambda-body expr (cse '() body)))
   ((primop-app _ rest ...)
    (let ((e (common-subexpr subexprs expr)))
      (if e
          (replace expr
                   (ast-binding-var e))
          (traverse-ast cse subexprs expr))))
   ((let bindings body)
    (let* ((updated (append (extract-subexprs bindings)
                            subexprs))
           (filtered (filter-subexprs updated (ast-node-bound-vars expr))))
      (-> expr
          (set-ast-let-body (cse filtered body))
          (set-ast-let-bindings (map (partial cse subexprs) bindings)))))
   ((letrec bindings body)
    (let* ((updated (append (extract-subexprs bindings)
                            subexprs))
           (filtered (filter-subexprs updated (ast-node-bound-vars expr))))
      (-> expr
          (set-ast-letrec-body (cse filtered body))
          (set-ast-letrec-bindings (map (lambda (b)
                                          ;; NOTE Can't use the current expression as it'll match itself and optimize out.
                                          (cse (filter (compose not (partial equal? b))
                                                       filtered)
                                               b))
                                        bindings)))))
   ((fix bindings _)
    (let* ((filtered (filter-subexprs subexprs (ast-node-bound-vars expr))))
      ;; NOTE These are only lambdas, so there's nothing to eliminate.
      (traverse-ast cse filtered expr)))
   (else
    (traverse-ast cse subexprs expr))))

(define (extract-subexprs bindings)
  (filter (compose eliminatable-expr? ast-binding-val)
          bindings))

(define (filter-subexprs subexprs redefined)
  (filter (compose set-empty? (flip set-intersection redefined) ast-node-free-vars)
          subexprs))

(define (common-subexpr subexprs expr)
  (cond ((empty? subexprs)
         #f)
        ((ast-eqv? (ast-binding-val (car subexprs))
                   expr)
         (car subexprs))
        (else
         (common-subexpr (cdr subexprs) expr))))

(define +eliminatable-primops+
  '(car cadr cdr cddr list cons append concat
    equal? nil? empty? not
    * + - / >= > = < <= remainder quotient modulo zero?
    self))

(define (eliminatable-expr? node)
  (and (ast-primop-app? node)
       (member (ast-primop-app-op node)
               +eliminatable-primops+)))
