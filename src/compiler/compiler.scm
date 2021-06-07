;; The compiler

(require "env.rkt")
(require "pass.rkt")
(require "ast.rkt")

;; The frontend
(require "passes/parser.rkt")
(require "passes/macro-expander.rkt")
(require "passes/elaboration.rkt")
(require "passes/body.rkt")
(require "passes/qq.rkt")
(require "passes/const.rkt")
(require "passes/validate.rkt")
(require "passes/errors.rkt")

;; Optimizations
(require "passes/bindings.rkt")
(require "passes/freevars.rkt")
(require "passes/builtins.rkt")
(require "passes/lambdas.rkt")
(require "passes/copy-propagation.rkt")
(require "passes/const-propagation.rkt")
(require "passes/const-folding.rkt")
(require "passes/dce.rkt")
(require "passes/cse.rkt")
(require "passes/letrec-bindings.rkt")
(require "passes/letrec-fix.rkt")
(require "passes/cpc.rkt")

;; The backend
(load-once "compiler/passes/closures.scm")
(load-once "compiler/passes/rename.scm")

(define +optimization-loops+ 23)

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
                (list annotate-free-vars
                      inline-lambdas
                      annotate-free-vars
                      inline-builtins
                      propagate-constants
                      fold-constants
                      eliminate-common-subexpressions
                      propagate-copies
                      eliminate-dead-code))
               annotate-free-vars
               annotate-bindings
               reorder-letrec-bindings
               fix-letrec
               continuation-passing-convert
               (optimize
                (list annotate-free-vars
                      inline-lambdas
                      annotate-free-vars
                      propagate-constants
                      fold-constants
                      eliminate-common-subexpressions
                      propagate-copies
                      eliminate-dead-code))
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

(define debug-ast
  (pass (schema "debug")
        (lambda (env)
          (pretty-print (ast->plain (env-get env 'ast)))
          env)))

(define generate-target-code
  (pass (schema "generate-target-code"
                'ast (ast-subset? '(const symbol if do let binding lambda primop-app)))
        (lambda (env)
          ;; FIXME Actually implement a proper code-gen.
          (ast->plain (env-get env 'ast)))))
