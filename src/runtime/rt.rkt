#lang racket

(require "../compiler/passes/rename.rkt") ;; FIXME For symbol->safe

;; The runtime.

(require "closures.rkt")
(require "continuations.rkt")
(require "processes.rkt")
(require "actor.rkt")
(require "scheduler.rkt")
(require "monitor.rkt")
(require "bootstrap.rkt")

;; Also part of the runtime primops:
(require "../rete/rete.rkt")
(require "../compiler/utils/refs.rkt")

(provide rt-intern!
         rt-define!
         rt-export
         rt-import!
         rt-execute!
         bootstrap-rt!)

(define (bootstrap-rt!)
  ;; FIXME This is about 2-4 times slower than using the current-namespace.

  (let ((rt (make-base-namespace)))
    ;; Primitive values
    (for-each (lambda (p)
                (rt-define! rt (car p) (cdr p)))
              (list (cons 'nil '())
                    (cons 'true #t)
                    (cons 'false #f)
                    (cons 'yield __yield)
                    (cons 'recur __recur)
                    (cons 'list __list)))
    ;; Primitive operations
    (for-each (lambda (p)
                (rt-define-primop! rt (car p) (cdr p)))
              (list (cons 'nil? null?)
                    (cons 'ref ref)
                    (cons 'deref deref)
                    (cons 'assign! assign!)
                    (cons 'current-task current-task)
                    (cons 'uproc-error-handler uproc-error-handler)
                    (cons 'set-uproc-error-handler! set-uproc-error-handler!)
                    (cons 'set-uproc-delimited-continuations! set-uproc-delimited-continuations!)
                    (cons 'uproc-delimited-continuations uproc-delimited-continuations)
                    (cons 'uproc-error-handler uproc-error-handler)
                    (cons 'set-uproc-error-handler! set-uproc-error-handler!)
                    (cons 'uproc-pid uproc-pid)
                    (cons 'inc-uproc-rtime! inc-uproc-rtime!)
                    (cons 'uproc-state uproc-state)
                    (cons 'set-uproc-state! set-uproc-state!)
                    (cons 'uproc-msg-queue-empty? uproc-msg-queue-empty?)
                    (cons 'uproc-dequeue-msg! uproc-dequeue-msg!)
                    (cons 'send send)
                    (cons 'spawn spawn)
                    (cons 'task-info task-info)
                    (cons 'assert! assert!)
                    (cons 'signal! signal!)
                    (cons 'retract! retract!)
                    (cons 'select select)
                    (cons 'notify-whenever notify-whenever)
                    (cons 'make-closure make-closure)
                    (cons 'closure-fun closure-fun)
                    (cons 'closure-env closure-env)
                    (cons 'set-closure-env! set-closure-env!)
                    (cons 'kont-counter kont-counter)
                    (cons 'dec-kont-counter! dec-kont-counter!)
                    (cons 'reset-kont-counter! reset-kont-counter!)
                    (cons 'make-resumable make-resumable)
                    ))

    (reset-rete!)
    (reset-tasks! '())
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
       (cdr structure)))

(define (rt-export rt name)
  (namespace-variable-value (symbol->safe name) #f #f rt))

(define (rt-execute! rt expr)
    (spawn-task! (make-resumable
                  (make-closure
                   '()
                   (lambda (_ expr)
                     (rt-intern! rt expr)))
                  expr)
                 (make-closure
                  '()
                  (lambda (e err restart _)
                    (display ";; Execution finished due to an unhandled error: ")
                    (display err)
                    (newline)
                    err)))
    ;; NOTE Returns only the last result.
    (last (execute!)))
