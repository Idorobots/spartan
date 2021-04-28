;; Actor model:

(load-once "compiler/utils/utils.scm")
(load-once "runtime/continuations.scm")

(define (&uproc fields)
  (list &uproc fields))

(define (uproc priority cont handler rtime state)
  (let ((f (array 7 nil)))
    (array-assign! f 0 cont)
    (array-assign! f 1 rtime)
    (array-assign! f 2 priority)
    (array-assign! f 3 (gensym 'pid))
    (array-assign! f 4 nil) ;; NOTE message queue
    (array-assign! f 5 state)
    (array-assign! f 6 handler)
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

(define (uproc-msg-queue uproc)
  (array-ref (cadr uproc) 4))

(define (uproc-enqueue-msg! uproc msg)
  ;; FIXME Gross...
  (array-assign! (cadr uproc)
                 4
                 (append (uproc-msg-queue uproc)
                         (list msg))))

(define (uproc-dequeue-msg! uproc)
  (let* ((q (uproc-msg-queue uproc))
         (msg (car q)))
    (array-assign! (cadr uproc)
                   4
                   (cdr q))
    msg))

(define (uproc-msg-queue-empty? uproc)
  (empty? (uproc-msg-queue uproc)))

(define (uproc-state uproc)
  (array-ref (cadr uproc) 5))

(define (set-uproc-state! uproc state)
  (array-assign! (cadr uproc) 5 state))

(define (uproc-error-handler uproc)
  (array-ref (cadr uproc) 6))

(define (set-uproc-error-handler! uproc handler)
  (array-assign! (cadr uproc) 6 handler))
