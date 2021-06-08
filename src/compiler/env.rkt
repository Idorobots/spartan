#lang racket

;; Static compilation env.

(provide env env-get env-get* env-contains? env-set env-update)

(define (env . properties)
  (apply hasheq properties))

(define (env-get env property)
  (hash-ref env property))

(define (env-get* env property default)
  (if (env-contains? env property)
      (hash-ref env property)
      default))

(define (env-contains? env property)
  (hash-has-key? env property))

(define (env-set env . setters)
  (apply hash-set* env setters))

(define (env-update env property f)
  (env-set env property (f (env-get env property))))
