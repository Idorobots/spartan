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
                   (apply set
                          '(car cadr cdr cddr list cons append concat
                            equal? nil? not
                            * + - / = < zero? ;; random ;; FIXME Some tests override this function.
                            ref deref ;; assign! ;; FIXME Causes loops in letrec-conversion tests.
                            ;; call/current-continuation call/reset call/shift call/handler raise
                            self ;; sleep send spawn recv ;; FIXME Cause odd behaviour in Actor Model tests.
                            ;; assert! signal! retract! select notify-whenever ;; FIXME Causes weird behaviour of the RBS tests.
                            ;; display newline debug ;; FIXME Causes nothing to print for some examples.
                            )))
              expr))
