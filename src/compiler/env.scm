;; Static compilation env.

(define (env . properties)
  (apply hasheq properties))

(define (env-get env property)
  (hash-ref env property))

(define (env-set env . setters)
  (apply hash-set* env setters))

(define (env-update env property f)
  (env-set env property (f (env-get env property))))
