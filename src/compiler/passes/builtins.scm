;; Builtins inliner

(load "compiler/utils/utils.scm")
(load "compiler/substitute.scm")

(load "compiler/env.scm")
(load "compiler/ast.scm")

(define (inline-builtins env)
  (env-update env 'ast (partial inline-app-ops (env-get env 'globals))))

(define (inline-app-ops builtins expr)
  (substitute (lambda (subs expr kont)
                (kont (ast-case expr
                       ((app (symbol ,op) . ,args)
                        (let ((b (assoc (ast-symbol-value op) subs)))
                          (if b
                              ((cdr b) expr)
                              expr)))
                       (else
                        expr))))
              (map (lambda (b)
                     (cons b
                           (lambda (expr)
                             (replace expr
                                      (generated
                                       (make-primop-app-node b (ast-app-args expr)))))))
                   (set-intersection
                    builtins
                    (apply set
                           '(car cadr cdr cddr list cons append concat
                             equal? nil? not
                             * + - / = < zero?
                             ref deref assign!
                             self send spawn
                             assert! signal! retract! select notify-whenever
                             display newline debug
                             ;; NOTE These ones use the continuations, so they cannot be inlined.
                             ;; call/current-continuation call/reset call/shift call/handler raise recv
                             ;; FIXME These ones are overriden by the tests, so for the time being they can't be inlined.
                             ;; sleep random
                             ))))
              expr))
