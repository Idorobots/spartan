;; Runtime closure creation.

(define &make-env vector)

(define &env-ref vector-ref)

(define &set-env! vector-set!)

(define (&make-closure env fun)
  (vector &make-closure env fun))

(define (&set-closure-env! closure env)
  (vector-set! closure 1 env))

(define (&apply closure . args)
  (apply (vector-ref closure 2) (vector-ref closure 1) args))

(define (closure? thing)
  (equal? (vector-ref thing 0) &make-closure))
