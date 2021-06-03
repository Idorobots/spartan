;; Continuations:

(load-once "compiler/utils/utils.scm")
(load-once "runtime/closures.scm")

;; FIXME This really needs to be done on a per-uProc basis.
(define *delimited-continuations* '())

(define (&push-delimited-continuation! cont)
  (set! *delimited-continuations* (cons cont *delimited-continuations*)))

(define (&pop-delimited-continuation!)
  (let ((h (car *delimited-continuations*)))
    (set! *delimited-continuations* (cdr *delimited-continuations*))
    h))

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
