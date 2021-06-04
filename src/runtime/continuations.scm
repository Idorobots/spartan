;; Continuations:

(require "../compiler/utils/utils.rkt")
(load-once "runtime/closures.scm")
(load-once "runtime/processes.scm")

(define (&push-delimited-continuation! cont)
  (set-uproc-delimited-continuations! (current-task)
                                      (cons cont (uproc-delimited-continuations (current-task)))))

(define (&pop-delimited-continuation!)
  (let* ((stack (uproc-delimited-continuations (current-task)))
         (top (car stack)))
    (set-uproc-delimited-continuations! (current-task) (cdr stack))
    top))

(define (&yield-cont cont hole) ;; NOTE Used in compiler-generated code not via bootstrap.
  (list &yield-cont cont hole))

(define (resumable? thing)
  (tagged-list? &yield-cont thing))

(define (resumable-cont thing)
  (cadr thing))

(define (resumable-arg thing)
  (caddr thing))

(define (resume thing)
  (&apply (resumable-cont thing) (resumable-arg thing)))

(define (can-resume? thing)
  (closure? (resumable-cont thing)))
