;; The compiler

(load "compiler/env.scm")
(load "compiler/pass.scm")
(load "compiler/ast.scm")

;; The frontend
(load "compiler/passes/parser.scm")
(load "compiler/passes/macro-expander.scm")
(load "compiler/passes/elaboration.scm")
(load "compiler/passes/body.scm")
(load "compiler/passes/qq.scm")
(load "compiler/passes/const.scm")
(load "compiler/passes/validate.scm")
(load "compiler/passes/errors.scm")

(load "compiler/passes/bindings.scm")
(load "compiler/passes/freevars.scm")

;; The backend
(load "compiler/passes/builtins.scm")
(load "compiler/passes/copy-propagation.scm")
(load "compiler/passes/const-propagation.scm")
(load "compiler/passes/const-folding.scm")
(load "compiler/passes/letrec-bindings.scm")
(load "compiler/passes/letrec-fix.scm")
(load "compiler/passes/cpc.scm")
(load "compiler/passes/closures.scm")
(load "compiler/passes/rename.scm")

(define +optimization-loops+ 10)

(define (compile env)
  (foldl run-pass
         (env-set env
                  'errors '()
                  'macros (make-builtin-macros)
                  'globals (make-global-definitions-list))
         (list parse
               macro-expand
               elaborate
               body-expand
               quasiquote-expand
               annotate-constants
               annotate-free-vars
               annotate-bindings
               validate
               report-errors
               inline-builtins
               (optimize
                (list propagate-copies
                      propagate-constants
                      fold-constants))
               reorder-letrec-bindings
               fix-letrec
               continuation-passing-convert
               annotate-free-vars
               closure-convert
               symbol-rename
               generate-target-code)))

(define (optimize passes)
  (pass (schema "optimize")
        (lambda (env)
          (let loop ((i +optimization-loops+)
                     (acc env)
                     (prev '()))
            (if (or (= i 0)
                    (equal? prev acc))
                acc
                (loop (- i 1)
                      (foldl run-pass
                             acc
                             passes)
                      acc))))))

(define generate-target-code
  (pass (schema "generate-target-code"
                'ast (ast-subset? '(const symbol if do let binding lambda primop-app)))
        (lambda (env)
          ;; FIXME Actually implement a proper code-gen.
          (ast->plain (env-get env 'ast)))))
