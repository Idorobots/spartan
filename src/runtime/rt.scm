;; The runtime.

(load "compiler/utils.scm")

;; Continuations:
(define (&yield-cont cont hole)
  (list &yield-cont cont hole))

(define (resumable? thing)
  (tagged-list? &yield-cont thing))

(define (resumable-cont thing)
  (cadr thing))

(define (resumable-arg thing)
  (caddr thing))

(define (resume thing)
  ((resumable-cont thing) (resumable-arg thing)))

;; Actor model:
(define (&micro-proc cont)
  (list &micro-proc (array 1 cont)))

(define (micro-proc? thing)
  (tagged-list? &micro-proc thing))

(define (micro-proc-continuation uproc)
  (array-ref (cadr uproc) 0))

(define (update-continuation! uproc cont)
  (array-assign! (cadr uproc) 0 cont))

;; The scheduler:
(define (task-list . tasks)
  tasks)

(define (add-task tasks task)
  (append tasks (list task)))

(define (pop-task tasks)
  (cdr tasks))

(define (next-task tasks)
  (car tasks))

(define (step uproc)
  (update-continuation! uproc
                        (resume (micro-proc-continuation uproc)))
  uproc)

(define (executable? task)
  (resumable? (micro-proc-continuation task)))

(define (execute tasks)
  (if (empty? tasks)
      nil
      (let ((n (next-task tasks))
            (rest (pop-task tasks)))
        (if (executable? n)
            (execute (add-task rest (step n)))
            (let ((result (micro-proc-continuation n)))
              (display ";; uProc finished with result:")
              (newline)
              (display result)
              (newline)
              (cons result (execute rest)))))))

;; FIXME This should accept task list istead.
(define (run cont)
  (let ((tasks (task-list (&micro-proc cont))))
    ;; FIXME Returns only the first result.
    (car (execute tasks))))
