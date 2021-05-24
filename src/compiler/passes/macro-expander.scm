;; Pseudo macro-expander for convenience

(load-once "compiler/utils/utils.scm")

(load-once "compiler/env.scm")
(load-once "compiler/pass.scm")
(load-once "compiler/ast.scm")
(load-once "compiler/errors.scm")

(define macro-expand
  (pass (schema "macro-expand"
                'macros non-empty-hash?
                'errors a-list?
                'ast (ast-subset? '(quote quasiquote unquote unquote-splicing
                                    number symbol string list
                                    primop-app body <error>)))
        (lambda (env)
          (let ((result (collect-errors (env-get env 'errors)
                                        (lambda ()
                                          (expand-macros (env-get env 'ast)
                                                         (env-get env 'macros))))))
            (env-set env
                     'ast (car result)
                     'errors (cadr result))))))

(define (expand-macros expr macros)
  (ast-case expr
   ((a-quote _)
    expr)
   ((a-quasiquote _)
    expr)
   ((list 'quote . ,rest)
    expr)
   ((list 'quasiquote . ,rest)
    expr)
   ((list (symbol ,head) . ,rest)
    (let ((sym (ast-symbol-value head)))
      (if (hash-has-key? macros sym)
          (expand-macros ((hash-ref macros sym) expr)
                         macros)
          (walk-ast (flip expand-macros macros)
                    expr))))
   (else
    (walk-ast (flip expand-macros macros)
              expr))))

(define (make-builtin-macros)
  (hasheqv 'when when-macro
           'unless unless-macro
           'cond cond-macro
           'and and-macro
           'or or-macro
           'let* let*-macro
           'letcc letcc-macro
           'handle handle-macro
           'shift shift-macro
           'reset reset-macro
           'structure structure-macro
           'module module-macro))

(define (when-macro expr)
  (ast-case expr
   ((list 'when ,cond ,first . ,rest)
    (let ((loc (ast-node-location expr)))
      (replace expr
               (make-ast-if cond
                             (at loc
                                 (make-ast-body (cons first rest) "Bad `when` body syntax"))
                             (at loc
                                 (make-ast-symbol 'nil))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `when` syntax, expected a condition and a body to follow:")))))

(define (unless-macro expr)
  (ast-case expr
   ((list 'unless ,cond ,first . ,rest)
    (let ((loc (ast-node-location expr)))
      (replace expr
               (make-ast-if cond
                             (at loc
                                 (make-ast-symbol 'nil))
                             (at loc
                                 (make-ast-body (cons first rest) "Bad `unless` body syntax"))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `unless` syntax, expected a condition and a body to follow:")))))

(define (cond-macro expr)
  (ast-case expr
   ((list 'cond (list 'else ,else-first . ,else-rest))
    (at (ast-node-location expr)
        (make-ast-body (cons else-first else-rest) "Bad `cond` else clause")))
   ((list 'cond (list ,cond ,branch-first . ,branch-rest) . ,rest)
    (let ((loc (ast-node-location expr)))
      (replace expr
               (make-ast-if cond
                             (at loc
                                 (make-ast-body (cons branch-first branch-rest) "Bad `cond` clause"))
                             (at loc
                                 (make-ast-list
                                  (cons (ast-list-car expr)
                                        rest)))))))
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
             (make-ast-if first
                           (at (ast-node-location expr)
                               (make-ast-list
                                (cons and rest)))
                           (at (ast-node-location expr)
                               (make-ast-symbol 'false)))))
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
             (make-ast-if first
                           (at (ast-node-location expr)
                               (make-ast-symbol 'true))
                           (at (ast-node-location expr)
                               (make-ast-list
                                (cons or rest))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `or` syntax, expected a list of expressions to follow:")))))

(define (let*-macro expr)
  (ast-case expr
   ((list 'let* () ,first . ,rest)
    (at (ast-node-location expr)
        (make-ast-body (cons first rest) "Bad `let*` body syntax")))
   ((list 'let* (list ,first-binding . ,rest-bindings) . ,body)
    (replace expr
             (make-ast-let (valid-bindings (list first-binding) "Bad `let*` bindings syntax")
                           (at (ast-node-location expr)
                               (make-ast-list
                                (list* (at (ast-node-location expr)
                                           (make-ast-symbol 'let*))
                                       (at (ast-node-location expr) ;; FIXME Could use a better location.
                                           (make-ast-list rest-bindings))
                                       body))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `let*` syntax, expected a list of bindings and a body to follow:")))))

(define (letcc-macro expr)
  (ast-case expr
   ((list 'letcc ,name ,first . ,rest)
    (let ((loc (ast-node-location expr)))
      (replace expr
               (make-ast-app (at loc
                                  (make-ast-symbol 'call/current-continuation))
                              (list (at loc
                                        (make-ast-lambda (list (valid-symbol name "Bad `letcc` syntax"))
                                                          (at loc
                                                              (make-ast-body (cons first rest) "Bad `letcc` body syntax")))))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `letcc` syntax, expected an identifier and a body to follow:")))))

(define (shift-macro expr)
  (ast-case expr
   ((list 'shift ,name ,first . ,rest)
    (let ((loc (ast-node-location expr)))
      (replace expr
               (make-ast-app (at loc
                                  (make-ast-symbol 'call/shift))
                              (list (at loc
                                        (make-ast-lambda (list (valid-symbol name "Bad `shift` syntax"))
                                                          (at loc
                                                              (make-ast-body (cons first rest) "Bad `shift` body syntax")))))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `shift` syntax, expected an identifier and a body to follow:")))))

(define (reset-macro expr)
  (ast-case expr
   ((list 'reset ,first . ,rest)
    (let ((loc (ast-node-location expr)))
      (replace expr
               (make-ast-app (at loc
                                  (make-ast-symbol 'call/reset))
                              (list (at loc
                                        (make-ast-lambda '()
                                                          (at loc
                                                              (make-ast-body (cons first rest) "Bad `reset` body syntax")))))))))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `reset` syntax, expected exactly one expression to follow:")))))

(define (handle-macro expr)
  (ast-case expr
   ((list 'handle ,subexpr ,handler)
    (replace expr
             (make-ast-app (at (ast-node-location expr)
                                (make-ast-symbol 'call/handler))
                            (list handler
                                  (at (ast-node-location expr)
                                      (make-ast-lambda '()
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
               (make-ast-body (append defs
                                       (list (at (ast-node-location expr)
                                                 (make-ast-primop-app
                                                  '&make-structure
                                                  (map (lambda (n)
                                                         (at (ast-node-location n)
                                                             (make-ast-primop-app
                                                              '&structure-binding
                                                              (list (at (ast-node-location n)
                                                                        (make-ast-quote n))
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
             (set-ast-node-context
              (make-ast-def name
                             (at (ast-node-location expr)
                                 (make-ast-lambda (map (flip valid-symbol "Bad `module` dependencies syntax") deps)
                                                   (at (ast-node-location expr)
                                                       (make-ast-list
                                                        (cons (at (ast-node-location expr)
                                                                  (make-ast-symbol 'structure))
                                                              body))))))
              "Bad `module` syntax")))
   (else
    (let ((node (ast-list-car expr)))
      (raise-compilation-error
       node
       "Bad `module` syntax, expected a module specification followed by a body:")))))
