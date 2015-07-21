;; The runtime.

(load "compiler/utils.scm")

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

(define (run code)
  (if (resumable? code)
      (run (resume code))
      code))
