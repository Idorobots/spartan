;; Static compilation env.

(load "compiler/utils.scm")

(define (env . properties)
  (apply hasheq properties))

(define (env-get env property)
  (hash-ref env property))

(define (env-set env property value)
  (hash-set env property value))

(define (env-update env property f)
  (env-set env property (f (env-get env property))))
