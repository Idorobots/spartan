#lang racket

;; Builtins inliner

(require "../utils/utils.rkt")
(require "../utils/set.rkt")

(require "../substitute.rkt")
(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")

(provide inline-builtins
         ;; FIXME For test access.
         inline-app-ops)

(define inline-builtins
  (pass (schema "inline-builtins"
                'globals a-set?
                'ast (ast-subset? '(const symbol if do let letrec binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (partial inline-app-ops (env-get env 'globals))))))

(define (inline-app-ops builtins expr)
  (substitute (lambda (subs expr kont)
                (kont (match-ast expr
                       ((app (symbol op) args ...)
                        (apply-sub subs
                                   op
                                   expr
                                   (constantly expr)))
                       (else
                        expr))))
              (make-subs
               (map (lambda (b)
                      (cons b
                            (lambda (expr)
                              (let ((args (ast-app-args expr)))
                                (replace expr
                                         ;; NOTE The op no longer needs to be stored in free vars.
                                         (set-ast-node-free-vars
                                          (set-sum (map ast-node-free-vars args))
                                          (make-ast-primop-app (ast-node-location expr)
                                                               b
                                                               args)))))))
                    (set->list
                     (set-intersection
                      builtins
                      (apply set
                             '(car cadr cdr cddr list cons append concat
                                   equal? nil? empty? not
                                   * + - / = < <= > >=
                                   remainder quotient modulo zero? random
                                   ref deref assign!
                                   self send spawn sleep
                                   assert! signal! retract! select notify-whenever
                                   display newline debug
                                   ;; NOTE These ones use the continuations, so they cannot be inlined.
                                   ;; call/current-continuation call/reset call/shift call/handler raise recv
                                   ))))))
              expr))
