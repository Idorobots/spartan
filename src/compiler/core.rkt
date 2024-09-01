#lang racket

(require "ast.rkt")
(require "env.rkt")
(require "utils/set.rkt")
(require "utils/utils.rkt")
(require "utils/assets.rkt")
(require "compiler.rkt")

(provide make-global-definitions-list compile-core-spartan)

;; Core module bootstrap

(define *global-definitions* '())

(define (make-global-definitions-list)
  (apply set *global-definitions*))

(define (extract-global-definitions ast)
  ;; FIXME Assumes that the module is turned into a large letrec expression
  (match-ast ast
    ((letrec bindings body)
     (map (lambda (b)
            (set! *global-definitions*
                  (cons (ast-symbol-value (ast-binding-var b))
                        *global-definitions*)))
          bindings)
     ast)
    (else
     ;; NOTE Should be a compilation error, but since this is a built-in module it should always work.
     ast)))

(define (compile-core-spartan init)
  (set! *global-definitions* '(list yield))
  (let ((inst (compile
               (env-set init
                        'module "core"
                        'input (format "(structure ~a)" (embed-file-contents "../runtime/core.sprtn"))
                        ;; FIXME This still needs the two "built-in" global procedures.
                        'globals (make-global-definitions-list)
                        'instrument extract-global-definitions
                        'last-phase 'instrument))))
    (compile (env-set inst
                      'first-phase 'instrument
                      'last-phase 'codegen
                      ;; FIXME Needs the full globals and intrinsics lists for the optimizations do do anything.
                      ;; FIXME These global functions are still added to the closure envs despite technically being global.
                      'globals (make-global-definitions-list)))))
