;; Actor model:

(load "compiler/utils.scm")
(load "runtime/continuations.scm")

(define (&uproc fields)
  (list &uproc fields))

(define (uproc cont)
  (&uproc (array 1 cont)))

(define (uproc? thing)
  (tagged-list? &uproc thing))

(define (uproc-continuation uproc)
  (array-ref (cadr uproc) 0))

(define (set-uproc-continuation! uproc cont)
  (array-assign! (cadr uproc) 0 cont))
