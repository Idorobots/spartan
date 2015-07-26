;; Actor model:

(load "compiler/utils.scm")
(load "runtime/continuations.scm")

(define (&uproc fields)
  (list &uproc fields))

(define (uproc priority cont rtime)
  (let ((f (array 4 nil)))
    (array-assign! f 0 cont)
    (array-assign! f 1 rtime)
    (array-assign! f 2 priority)
    (array-assign! f 3 (gensym 'pid))
    (&uproc f)))

(define (uproc? thing)
  (tagged-list? &uproc thing))

(define (uproc-continuation uproc)
  (array-ref (cadr uproc) 0))

(define (set-uproc-continuation! uproc cont)
  (array-assign! (cadr uproc) 0 cont))

(define (uproc-rtime uproc)
  (array-ref (cadr uproc) 1))

(define (set-uproc-rtime! uproc time)
  (array-assign! (cadr uproc) 1 time))

(define (inc-uproc-rtime! uproc by)
  (set-uproc-rtime! uproc (+ by (uproc-rtime uproc))))

(define (uproc-priority uproc)
  (array-ref (cadr uproc) 2))

(define (uproc-vtime uproc)
  (* (uproc-rtime uproc)
     (uproc-priority uproc)))

(define (uproc-pid uproc)
  (array-ref (cadr uproc) 3))
