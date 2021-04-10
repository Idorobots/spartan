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
                                 (if (assoc (ast-symbol-value op) subs)
                                     (replace expr
                                              (generated
                                               (make-primop-app-node op args)))
                                     expr))
                                (else
                                 expr))))
              (map (lambda (b)
                     (cons b id))
                   (set-intersection
                    builtins
                    (apply set
                           '(car cadr cdr cddr list cons append concat
                             equal? nil? not
                             * + - / = < zero? ;; random ;; FIXME Some tests override this function.
                             ref deref assign!
                             ;; call/current-continuation call/reset call/shift call/handler raise ;; NOTE These are not defined.
                             self send spawn ;; sleep recv ;; FIXME sleep is overriden by some tests.
                             assert! signal! retract! select ;; notify-whenever ;; NOTE This is not defined as a separate function.
                             display newline debug
                             ))))
              expr))
