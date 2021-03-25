;; Pseudo macro-expander for convenience

(load "compiler/utils/utils.scm")

(load "compiler/env.scm")
(load "compiler/errors.scm")
(load "compiler/tree-ast.scm")
(load "compiler/body.scm")

(define (macro-expand env)
  (let ((result (collect-errors (env-get env 'errors)
                                (lambda ()
                                  (expand-macros (env-get env 'ast)
                                                 (env-get env 'macros))))))
    (env-set env
             'ast (car result)
             'errors (cadr result))))

(define (expand-macros expr macros)
  (walk-ast (lambda (subexpr)
              (if (is-quoted? subexpr)
                  subexpr
                  (expand-macros subexpr macros)))
            (if (and (is-type? expr 'list)
                     (> (ast-list-length expr) 0)
                     (is-type? (ast-list-car expr) 'symbol))
                (expand-macro expr macros)
                expr)))

(define (is-quoted? expr)
  (case (get-type expr)
    ((quote quasiquote) #t)
    ((list) (and (> (ast-list-length expr) 0)
                 (is-type? (ast-list-car expr) 'symbol)
                 (equal? (ast-symbol-value (ast-list-car expr)) 'quote)))
    (else #f)))

(define (expand-macro expr macros)
  (let ((macro (assoc (ast-symbol-value (ast-list-car expr)) macros)))
    (if macro
        (expand-macros ((cdr macro) expr)
                       macros)
        expr)))

(define (make-builtin-macros)
  (list (cons 'when when-macro)
        (cons 'unless unless-macro)
        (cons 'cond cond-macro)
        (cons 'and and-macro)
        (cons 'or or-macro)
        (cons 'let* let*-macro)
        (cons 'letcc letcc-macro)
        (cons 'handle handle-macro)
        (cons 'shift shift-macro)
        (cons 'reset reset-macro)
        ;; FIXME These should be moved to semantic elaboration phase.
        (cons 'structure structure-macro)
        (cons 'module module-macro)))

(define (when-macro expr)
  (ast-case expr
   ((list 'when ,cond ,first . ,rest)
    (replace expr
             (make-if-node cond
                           (wrap-with-do (cons first rest) "Bad `when` body syntax")
                           (at (get-location expr)
                               (make-symbol-node 'nil)))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `when` syntax, expected a condition and a body to follow:")))))

(define (unless-macro expr)
  (ast-case expr
   ((list 'unless ,cond ,first . ,rest)
    (replace expr
             (make-if-node cond
                           (at (get-location expr)
                               (make-symbol-node 'nil))
                           (wrap-with-do (cons first rest) "Bad `unless` body syntax"))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `unless` syntax, expected a condition and a body to follow:")))))

(define (cond-macro expr)
  (ast-case expr
   ((list 'cond (list 'else ,else-first . ,else-rest))
    (wrap-with-do (cons else-first else-rest) "Bad `cond` else clause"))
   ((list 'cond (list ,cond ,branch-first . ,branch-rest) . ,rest)
    (replace expr
             (make-if-node cond
                           (wrap-with-do (cons branch-first branch-rest) "Bad `cond` clause")
                           (at (get-location expr)
                               (make-list-node
                                (cons (ast-list-car expr)
                                      rest))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `cond` syntax, expected a list of conditional branches with a final else branch to follow:")))))

(define (and-macro expr)
  (ast-case expr
   ((list 'and ,last)
    last)
   ((list ,and ,first . ,rest)
    (replace expr
             (make-if-node first
                           (at (get-location expr)
                               (make-list-node
                                (cons and rest)))
                           (at (get-location expr)
                               (make-symbol-node 'false)))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `and` syntax, expected a list of expressions to follow:")))))

(define (or-macro expr)
  (ast-case expr
   ((list 'or ,last)
    last)
   ((list ,or ,first . ,rest)
    (replace expr
             (make-if-node first
                           (at (get-location expr)
                               (make-symbol-node 'true))
                           (at (get-location expr)
                               (make-list-node
                                (cons or rest))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `or` syntax, expected a list of expressions to follow:")))))

(define (let*-macro expr)
  (ast-case expr
   ((list 'let* () ,first . ,rest)
    (wrap-with-do (cons first rest) "Bad `let*` body syntax"))
   ((list 'let* (list ,first-binding . ,rest-bindings) . ,body)
    (replace expr
             (make-let-node (valid-bindings (list first-binding) "Bad `let*` bindings syntax")
                           (at (get-location expr)
                               (make-list-node
                                (list* (at (get-location expr)
                                           (make-symbol-node 'let*))
                                       (at (get-location expr) ;; FIXME Could use a better location.
                                           (make-list-node rest-bindings))
                                       body))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `let*` syntax, expected a list of bindings and a body to follow:")))))

(define (letcc-macro expr)
  (ast-case expr
   ((list 'letcc ,name ,first . ,rest)
    (replace expr
             (make-app-node (at (get-location expr)
                                (make-symbol-node 'call/current-continuation))
                            (list (at (get-location expr)
                                      (make-lambda-node (list (valid-symbol name "Bad `letcc` syntax"))
                                                        (wrap-with-do (cons first rest) "Bad `letcc` body syntax")))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `letcc` syntax, expected an identifier and a body to follow:")))))

(define (shift-macro expr)
  (ast-case expr
   ((list 'shift ,name ,first . ,rest)
    (replace expr
             (make-app-node (at (get-location expr)
                                (make-symbol-node 'call/shift))
                            (list (at (get-location expr)
                                      (make-lambda-node (list (valid-symbol name "Bad `shift` syntax"))
                                                        (wrap-with-do (cons first rest) "Bad `shift` body syntax")))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `shift` syntax, expected an identifier and a body to follow:")))))

(define (reset-macro expr)
  (ast-case expr
   ((list 'reset ,first . ,rest)
    (replace expr
             (make-app-node (at (get-location expr)
                                (make-symbol-node 'call/reset))
                            (list (at (get-location expr)
                                      (make-lambda-node '()
                                                        (wrap-with-do (cons first rest) "Bad `reset` body syntax")))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `reset` syntax, expected exactly one expression to follow:")))))

(define (handle-macro expr)
  (ast-case expr
   ((list 'handle ,subexpr ,handler)
    (replace expr
             (make-app-node (at (get-location expr)
                                (make-symbol-node 'call/handler))
                            (list handler
                                  (at (get-location expr)
                                      (make-lambda-node '()
                                                        subexpr))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `handle` syntax, expected exactly two expressions to follow:")))))

(define (structure-macro expr)
  (ast-case expr
   ((list 'structure . ,defs)
    (let ((names (map extract-definition-name defs)))
      (replace expr
               (wrap-with-do (append defs
                                     (list (at (get-location expr)
                                               (make-primop-app-node
                                                (at (get-location expr)
                                                    (make-symbol-node '&make-structure))
                                                (map (lambda (n)
                                                       (at (get-location n)
                                                           (make-primop-app-node
                                                            (at (get-location n)
                                                                (make-symbol-node '&structure-binding))
                                                            (list (at (get-location n)
                                                                      (make-quote-node n))
                                                                  n))))
                                                     names)))))
                             "Bad `structure` syntax"))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `structure` syntax, expected a module specification followed by a body:")))))

(define (extract-definition-name expr)
  (ast-case expr
   ((list 'define (list ,name . ,rest) . ,body)
    name)
   ((list 'define ,name ,value)
    name)
   (else
    (raise-compilation-error
     expr
     "Bad `structure` syntax, expected a definition:"))))

(define (module-macro expr)
  (ast-case expr
   ((list 'module (list ,name . ,deps) . ,body)
    (replace expr
             (context "Bad `module` syntax"
                      (make-def-node name
                                     (at (get-location expr)
                                         (make-lambda-node (map (flip valid-symbol "Bad `module` dependencies syntax") deps)
                                                           (at (get-location expr)
                                                               (make-list-node
                                                                (cons (at (get-location expr)
                                                                          (make-symbol-node 'structure))
                                                             body)))))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `module` syntax, expected a module specification followed by a body:")))))
