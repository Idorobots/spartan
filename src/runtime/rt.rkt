#lang racket

;; The runtime.
(require "../compiler/utils/assets.rkt") ;; FIXME For embed-file-contents
(require "../compiler/passes/rename.rkt") ;; FIXME For symbol->safe

(require "closures.rkt")
(require "continuations.rkt")
(require "delimited.rkt")
(require "processes.rkt")
(require "actor.rkt")
(require "scheduler.rkt")
(require "monitor.rkt")
(require "bootstrap.rkt")

;; (provide (all-from-out "closures.rkt"))
;; (provide (all-from-out "continuations.rkt"))
;; (provide (all-from-out "delimited.rkt"))
;; (provide (all-from-out "processes.rkt"))
;; (provide (all-from-out "actor.rkt"))
;; (provide (all-from-out "scheduler.rkt"))
;; (provide (all-from-out "monitor.rkt"))
;; (provide (all-from-out "bootstrap.rkt"))

;; Also part of the runtime primops:
(require "../rete/rete.rkt")
;;(provide (all-from-out "../rete/rete.rkt"))

(require "../compiler/utils/refs.rkt")
;;(provide ref deref assign!)

(provide rt-intern!
         rt-define!
         rt-export
         rt-execute!
         bootstrap-rt-once!)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define *core-interned* #f)
(define +core-spartan+ (embed-file-contents "./core.sprtn"))

;; FIXME The run is needed to avoid cyclic dependencies.
(define (bootstrap-rt-once! run)
  ;; FIXME This is very hacky, should be replaced when the module system is fleshed out more.
  (unless *core-interned*
    (set! *core-interned* #t)
    (let ((core (run +core-spartan+)))
      (map (lambda (b)
             (rt-define! ns (car b) (cdr b)))
           (cdr core))))
  (reset-rete!)
  (reset-tasks! '())
  ns)

(define (rt-intern! rt expr)
  (eval expr rt))

(define (rt-define! rt name value)
  (namespace-set-variable-value! (symbol->safe name) value #f rt))

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
