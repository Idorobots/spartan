;; The runtime.

(define (&yield-cont cont hole)
  (list &yield-cont cont hole))

(define (resumable? thing)
  (and (list? thing)
       (equal? (car thing) &yield-cont)))

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
