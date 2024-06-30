#lang racket

;; Common subexpression elimination.

(require "../utils/utils.rkt")
(require "../utils/set.rkt")

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")

(provide eliminate-common-subexpressions
         ;; FIXME For test access.
         cse)

(define eliminate-common-subexpressions
  (pass (schema "eliminate-common-subexpressions"
                'intrinsics a-list?
                'ast (ast-subset? '(const symbol if do let letrec fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (lambda (expr)
                                   (cse (subexpr-extractor (env-get env 'intrinsics)) '() expr))))))

(define (cse extract-subexprs subexprs expr)
  (let loop ((subexprs subexprs)
             (expr expr))
    (match-ast expr
     ((lambda _ body)
      ;; NOTE CSE is performed locally within a procedure not to inflate closure envs too much,
      ;; NOTE so this essentially cuts of all the propagated expressions thus far.
      (set-ast-lambda-body expr (loop '() body)))
     ((primop-app _ rest ...)
      (let ((e (common-subexpr subexprs expr)))
        (if e
            (replace expr
                     (ast-binding-var e))
            (traverse-ast loop subexprs expr))))
     ((let bindings body)
      (let* ((updated (append (extract-subexprs bindings)
                              subexprs))
             (filtered (filter-subexprs updated (ast-node-bound-vars expr))))
        (-> expr
            (set-ast-let-body (loop filtered body))
            (set-ast-let-bindings (map (partial loop subexprs) bindings)))))
     ((letrec bindings body)
      (let* ((updated (append (extract-subexprs bindings)
                              subexprs))
             (filtered (filter-subexprs updated (ast-node-bound-vars expr))))
        (-> expr
            (set-ast-letrec-body (loop filtered body))
            (set-ast-letrec-bindings (map (lambda (b)
                                            ;; NOTE Can't use the current expression as it'll match itself and optimize out.
                                            (loop (filter (compose not (partial equal? b))
                                                         filtered)
                                                 b))
                                          bindings)))))
     ((fix bindings _)
      (let* ((filtered (filter-subexprs subexprs (ast-node-bound-vars expr))))
        ;; NOTE These are only lambdas, so there's nothing to eliminate.
        (traverse-ast loop filtered expr)))
     (else
      (traverse-ast loop subexprs expr)))))

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

(define (subexpr-extractor primops)
  (let ((pure-primops (map car
                           (filter (lambda (p)
                                     (member 'pure p))
                                   primops))))
    (lambda (bindings)
      (filter (lambda (b)
                (let ((node (ast-binding-val b)))
                  (and (ast-primop-app? node)
                       (member (ast-primop-app-op node)
                               pure-primops))))
              bindings))))
