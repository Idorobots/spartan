#lang racket

(require "../compiler/passes/rename.rkt") ;; FIXME For symbol->safe

;; The runtime.

(require "closures.rkt")
(require "continuations.rkt")
(require "processes.rkt")
(require "bootstrap.rkt")

;; Also part of the runtime primops:
(require "../rete/rete.rkt")
(require "../compiler/utils/refs.rkt")

(provide rt-intern!
         rt-define!
         rt-export
         rt-import!
         rt-execute!
         rt-execute-no-init!
         bootstrap-rt!
         +use-global-namespace+)

(define +use-global-namespace+ #t)

(define-namespace-anchor anc)

(define (bootstrap-rt!)
  (let* ((rt (if +use-global-namespace+
                 (namespace-anchor->namespace anc)
                 (make-base-namespace))))

    (unless +use-global-namespace+
      ;; FIXME This is about 2-4 times slower than using the current namespace.
      ;; Primitive values
      (for-each (lambda (p)
                  (rt-define! rt (car p) (cdr p)))
                (list (cons 'yield __yield)
                      (cons 'list __list)
                      ))
      ;; Primitive operations
      (for-each (lambda (p)
                  (rt-define-primop! rt (car p) (cdr p)))
                (list (cons 'ref ref)
                      (cons 'deref deref)
                      (cons 'assign! assign!)
                      (cons 'make-uproc make-uproc)
                      (cons 'uproc-pid uproc-pid)
                      (cons 'uproc-priority uproc-priority)
                      (cons 'set-uproc-state! set-uproc-state!)
                      (cons 'uproc-state uproc-state)
                      (cons 'uproc-rtime uproc-rtime)
                      (cons 'set-uproc-rtime! set-uproc-rtime!)
                      (cons 'uproc-vtime uproc-vtime)
                      (cons 'set-uproc-continuation! set-uproc-continuation!)
                      (cons 'uproc-continuation uproc-continuation)
                      (cons 'set-uproc-delimited-continuations! set-uproc-delimited-continuations!)
                      (cons 'uproc-delimited-continuations uproc-delimited-continuations)
                      (cons 'set-uproc-error-handler! set-uproc-error-handler!)
                      (cons 'uproc-error-handler uproc-error-handler)
                      (cons 'uproc-msg-queue-empty? uproc-msg-queue-empty?)
                      (cons 'uproc-dequeue-msg! uproc-dequeue-msg!)
                      (cons 'uproc-enqueue-msg! uproc-enqueue-msg!)
                      (cons 'make-closure make-closure)
                      (cons 'closure-fun closure-fun)
                      (cons 'closure-env closure-env)
                      (cons 'set-closure-env! set-closure-env!)
                      (cons 'make-resumable make-resumable)
                      (cons 'suspend suspend)
                      (cons 'resume resume)
                      (cons 'trampoline trampoline)
                      (cons 'resumable? resumable?)
                      (cons 'kont-counter kont-counter)
                      (cons 'dec-kont-counter! dec-kont-counter!)
                      (cons 'reset-kont-counter! reset-kont-counter!)
                      (cons 'current-milliseconds current-milliseconds)
                      (cons 'delay-milliseconds delay-milliseconds)
                      (cons 'assert! assert!)
                      (cons 'signal! signal!)
                      (cons 'retract! retract!)
                      (cons 'select select)
                      (cons 'whenever whenever-trampoline)
                      )))

    (reset-rete!)
    rt))

(define (rt-intern! rt expr)
  (eval expr rt))

(define (rt-define-primop! rt name value)
  (namespace-set-variable-value! name value #t rt))

(define (rt-define! rt name value)
  (namespace-set-variable-value! (symbol->safe name) value #f rt #t))

(define (rt-import! rt structure)
  ;; FIXME This is very hacky, should be replaced when the module system is fleshed out more.
  (map (lambda (b)
         (rt-define! rt (car b) (cdr b)))
       (hash->list structure)))

(define (rt-export rt name)
  (namespace-variable-value (symbol->safe name) #f #f rt))

(define (rt-execute-no-init! rt expr)
  ;; FIXME Needed to evaluate the core module before interning all of its functions.
  (rt-intern! rt
              `(trampoline
                (suspend
                 (make-closure
                  '()
                  (lambda (e cont)
                    ,expr))))))

(define (rt-execute! rt expr)
  ;; NOTE Uses the core-provided startup routing that boots the scheduler and spawns the initial task.
  (rt-intern! rt
              `((closure-fun __rt_start)
                (closure-env __rt_start)
                (make-closure
                 '()
                 (lambda (_ cont)
                   ,expr))
                (make-closure
                 '()
                 (lambda (_ v)
                   v)))))
