;; Runtime closure creation.

(define &env-ref vector-ref)

(define &make-env vector)

(define (&make-closure env fun)
  (list &make-closure env fun))

(define (&apply closure . args)
  (apply (caddr closure) (cadr closure) args))

(define (closure? thing)
  (equal? (car thing) &make-closure))
