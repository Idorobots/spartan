;; Actor model:

(load "compiler/utils.scm")
(load "runtime/continuations.scm")

(define (&micro-proc cont)
  (list &micro-proc (array 1 cont)))

(define (micro-proc? thing)
  (tagged-list? &micro-proc thing))

(define (micro-proc-continuation uproc)
  (array-ref (cadr uproc) 0))

(define (update-continuation! uproc cont)
  (array-assign! (cadr uproc) 0 cont))
