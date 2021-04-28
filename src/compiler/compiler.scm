;; The compiler

(load-once "compiler/env.scm")
(load-once "compiler/pass.scm")
(load-once "compiler/ast.scm")

;; The frontend
(load-once "compiler/passes/parser.scm")
(load-once "compiler/passes/macro-expander.scm")
(load-once "compiler/passes/elaboration.scm")
(load-once "compiler/passes/body.scm")
(load-once "compiler/passes/qq.scm")
(load-once "compiler/passes/const.scm")
(load-once "compiler/passes/validate.scm")
(load-once "compiler/passes/errors.scm")

(load-once "compiler/passes/bindings.scm")
(load-once "compiler/passes/freevars.scm")

;; The backend
(load-once "compiler/passes/builtins.scm")
(load-once "compiler/passes/copy-propagation.scm")
(load-once "compiler/passes/const-propagation.scm")
(load-once "compiler/passes/const-folding.scm")
(load-once "compiler/passes/dce.scm")
(load-once "compiler/passes/letrec-bindings.scm")
(load-once "compiler/passes/letrec-fix.scm")
(load-once "compiler/passes/cpc.scm")
(load-once "compiler/passes/closures.scm")
(load-once "compiler/passes/rename.scm")

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
               (optimize
                (list inline-builtins
                      propagate-copies
                      propagate-constants
                      fold-constants
                      eliminate-dead-code))
               annotate-free-vars
               annotate-bindings
               reorder-letrec-bindings
               fix-letrec
               continuation-passing-convert
               annotate-free-vars ;; FIXME Needed for proper tail-call ellimination.
               (optimize
                (list eliminate-dead-code))
               annotate-free-vars
               closure-convert
               symbol-rename
               generate-target-code)))

(define (optimize passes)
  (pass (schema "optimize") ;; NOTE Schema depends on the passes.
        (lambda (env)
          (let loop ((i +optimization-loops+)
                     (acc env)
                     (prev '()))
            (if (or (= i 0)
                    (equal? prev acc)) ;; FIXME This is needlessly slow.
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
