#lang racket

;; IR instrumentation that runs before the backend.

(require "../env.rkt")
(require "../pass.rkt")

(provide instrument)

(define instrument
  (pass (schema "instrument"
                'instrument a-function?)
        (lambda (env)
          (env-update env 'ast (env-get env 'instrument)))))
