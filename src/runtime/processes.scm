;; Actor model:

(load "compiler/utils.scm")
(load "runtime/continuations.scm")

(define (&uproc fields)
  (list &uproc fields))

(define (uproc cont vtime)
  (let ((f (array 2 nil)))
    (array-assign! f 0 cont)
    (array-assign! f 1 vtime)
    (&uproc f)))

(define (uproc? thing)
  (tagged-list? &uproc thing))

(define (uproc-continuation uproc)
  (array-ref (cadr uproc) 0))

(define (set-uproc-continuation! uproc cont)
  (array-assign! (cadr uproc) 0 cont))

(define (uproc-vtime uproc)
  (array-ref (cadr uproc) 1))

(define (set-uproc-vtime! uproc vtime)
  (array-assign! (cadr uproc) 1 vtime))

(define (inc-uproc-vtime! uproc by)
  (set-uproc-vtime! uproc (+ by (uproc-vtime uproc))))
