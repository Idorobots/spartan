#lang racket

;; The compiler
(require "env.rkt")
(require "pass.rkt")
(require "utils/utils.rkt")
(require "utils/set.rkt")

;; The frontend
(require "passes/parser.rkt")
(require "passes/macro-expander.rkt")
(require "passes/qq.rkt")
(require "passes/const.rkt")
(require "passes/validate.rkt")
(require "passes/errors.rkt")

;; Optimizations
(require "passes/alpha-conversion.rkt")
(require "passes/optimize.rkt")
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
(require "passes/instrument.rkt")
(require "passes/closures.rkt")
(require "passes/globalization.rkt")
(require "passes/rename.rkt")
(require "passes/generator.rkt")

(provide compile add-defaults assemble-pipeline run-pipeline)

(define (add-defaults env)
  (env-set env
           ;; Compiler specific
           'errors '()
           'first-phase (env-get* env 'first-phase 'start)
           'last-phase (env-get* env 'last-phase 'codegen)
           ;; Expander & compilation envs.
           'intrinsics (env-get* env 'intrinsics (make-intrinsics-list))
           'static-env (env-get* env 'static-env (make-static-environment))
           'globals (env-get* env 'globals (make-global-definitions-list))
           ;; Parse transforms
           'instrument (env-get* env 'instrument id)
           ;; Optimization
           'optimizer (env-get* env 'optimizer 'naive)
           'optimization-level (env-get* env 'optimization-level 2)
           ;; Code generation
           'target (env-get* env 'target 'r7rs)))

(define (make-intrinsics-list)
  ;; FIXME To be replaced by the declare-primop's from core.
  '((suspend pure) (resumable? pure) (resume) (trampoline)
    (car pure) (cdr pure) (cons pure)
    (eq? pure) (equal? pure)
    (* pure) (+ pure) (- pure) (/ pure) (= pure) (< pure) (<= pure) (> pure) (>= pure)
    (remainder pure) (quotient pure) (modulo pure) (random)
    (ref) (deref) (assign!)
    (make-uproc) (uproc-pid pure) (uproc-priority pure)
    (uproc-rtime pure) (set-uproc-rtime!) (uproc-vtime pure)
    (uproc-state pure) (set-uproc-state!)
    (uproc-continuation pure) (set-uproc-continuation!)
    (uproc-delimited-continuations pure) (set-uproc-delimited-continuations!)
    (uproc-error-handler pure) (set-uproc-error-handler)
    (uproc-msg-queue-empty? pure) (uproc-dequeue-msg!) (uproc-enqueue-msg!)
    (assert!) (signal!) (retract!) (select pure) (whenever-trampoline)
    (display) (current-milliseconds) (delay-milliseconds)))

(define (make-global-definitions-list)
  ;; FIXME To be replaced by the list of interned global symbols in the runtime.
  (apply set
         '(yield suspend resume resumable? trampoline nice
           nil true false
           car cadr cdr cddr list cons append concat length map foldl foldr find last
           eq? equal? nil? empty? not
           * + - / = < <= > >=
           quotient remainder modulo random zero?
           current-task all-tasks find-task wake-task! spawn-task! spawn task-info monitor
           make-uproc uproc-pid uproc-priority uproc-rtime set-uproc-rtime! uproc-vtime
           uproc-continuation set-uproc-continuation!
           uproc-delimited-continuations set-uproc-delimited-continuations!
           uproc-error-handler set-uproc-error-handler!
           uproc-state set-uproc-state! uproc-msg-queue-empty? uproc-dequeue-msg! uproc-enqueue-msg!
           ref deref assign!
           call/current-continuation call/reset call/shift call/handler raise
           sleep self send recv
           assert! signal! retract! select whenever-trampoline notify-whenever
           display newline current-milliseconds delay-milliseconds)))

(define (full-pipeline opts-early opts-late opts-final)
  (list 'start
        parse
        'parse
        macro-expand
        quasiquote-expand
        'expand
        annotate-constants
        annotate-free-vars
        annotate-bindings
        instrument
        'instrument
        validate
        'validate
        report-errors
        alpha-convert
        'alpha
        (optimize opts-early)
        'optimize-early
        annotate-free-vars
        annotate-bindings
        reorder-letrec-bindings
        fix-letrec
        'letrec
        continuation-passing-convert
        'cps
        (optimize opts-late)
        'optimize-late
        annotate-free-vars
        closure-convert
        'closures
        (optimize opts-final)
        'optimize-final
        globalize
        'hoist
        symbol-rename
        'rename
        generate-target-code
        'codegen))

(define (trim-pipeline pipeline first-phase last-phase)
  (let trim-start ((remaining pipeline))
    (cond ((empty? remaining)
           '())
          ((equal? (car remaining) first-phase)
           (let trim-end ((p remaining))
             (cond ((empty? p)
                    '())
                   ((equal? (car p) last-phase)
                    '())
                   ((symbol? (car p))
                    (trim-end (cdr p)))
                   (else
                    (cons (car p)
                          (trim-end (cdr p)))))))
          (else
           (trim-start (cdr remaining))))))

(define (assemble-pipeline env)
  (let* ((opt-level (env-get env 'optimization-level))
         (opts-early (if (> opt-level 0)
                         (list (sequence annotate-free-vars
                                         inline-lambdas)
                               inline-builtins
                               propagate-constants
                               fold-constants
                               (sequence annotate-free-vars
                                         eliminate-common-subexpressions)
                               propagate-copies
                               (sequence annotate-free-vars
                                         eliminate-dead-code))
                         '()))
         (opts-late (if (> opt-level 1)
                        (list (sequence annotate-free-vars
                                        inline-lambdas)
                              propagate-constants
                              fold-constants
                              (sequence annotate-free-vars
                                        eliminate-common-subexpressions)
                              propagate-copies
                              (sequence annotate-free-vars
                                        eliminate-dead-code))
                        '()))
         (opts-final (if (> opt-level 2)
                         (list propagate-constants
                               fold-constants
                               (sequence annotate-free-vars
                                         eliminate-common-subexpressions)
                               propagate-copies
                               (sequence annotate-free-vars
                                         eliminate-dead-code))
                         '()))
         (pipeline (full-pipeline opts-early opts-late opts-final))
         (last-phase (env-get env 'last-phase))
         (first-phase (env-get env 'first-phase)))
    (trim-pipeline pipeline first-phase last-phase)))

(define (run-pipeline pipeline env)
  (foldl run-pass env pipeline))

(define (compile env)
  (let* ((e (add-defaults env))
         (pipeline (assemble-pipeline e)))
    (run-pipeline pipeline e)))
