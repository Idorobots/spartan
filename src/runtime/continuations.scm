;; Continuations:

(load "compiler/utils.scm")

(define (&yield-cont cont hole) ;; NOTE Used in compiler-generated code not via bootstrap.
  (list &yield-cont cont hole))

(define (resumable? thing)
  (tagged-list? &yield-cont thing))

(define (resumable-cont thing)
  (cadr thing))

(define (resumable-arg thing)
  (caddr thing))

(define (resume thing)
  ((resumable-cont thing) (resumable-arg thing)))

(define (can-resume? thing)
  (procedure? (resumable-cont thing)))
