;; Common subexpression elimination.

(load-once "compiler/utils/utils.scm")
(load-once "compiler/utils/set.scm")

(load-once "compiler/env.scm")
(load-once "compiler/pass.scm")
(load-once "compiler/ast.scm")

(define eliminate-common-subexpressions
  (pass (schema "eliminate-common-subexpressions"
                'ast (ast-subset? '(const symbol if do let letrec fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (partial cse '())))))

(define (cse subexprs expr)
  (ast-case expr
   ((lambda _ ,body)
    ;; NOTE CSE is performed locally within a procedure not to inflate closure envs too much,
    ;; NOTE so this essentially cuts of all the propagated expressions thus far.
    (ast-update expr 'body (partial cse '())))
   ((primop-app _ . ,rest)
    (let ((e (common-subexpr subexprs expr)))
      (if e
          (replace expr (ast-binding-var e))
          (walk-ast (partial cse subexprs) expr))))
   ((let ,bindings _)
    (let* ((filtered (filter-subexprs subexprs (get-bound-vars expr)))
           (updated (append (extract-subexprs bindings)
                            filtered)))
      (ast-update (ast-update expr 'bindings (partial map (partial cse subexprs)))
                'body
                (partial cse updated))))
   ((letrec ,bindings _)
    (let* ((filtered (filter-subexprs subexprs (get-bound-vars expr)))
           (updated (append (extract-subexprs bindings)
                            filtered)))
      (walk-ast (partial cse updated) expr)))
   ((fix ,bindings _)
    (let* ((filtered (filter-subexprs subexprs (get-bound-vars expr))))
      (walk-ast (partial cse filtered) expr)))
   (else
    (walk-ast (partial cse subexprs) expr))))

(define (extract-subexprs bindings)
  (filter (compose eliminatable-expr? ast-binding-val)
          bindings))

(define (filter-subexprs subexprs redefined)
  (filter (compose set-empty? (flip set-intersection redefined) get-free-vars)
          subexprs))

(define (common-subexpr subexprs expr)
  (cond ((empty? subexprs)
         #f)
        ((ast-eqv? (ast-binding-val (car subexprs))
                   expr)
         (car subexprs))
        (else
         (common-subexpr (cdr subexprs) expr))))

(define (eliminatable-expr? node)
  (and (primop-app-node? node)
       (member (ast-primop-app-op node)
               '(car cadr cdr cddr list cons append concat
                 equal? nil? not
                 * + - / = < zero?
                 self))))
