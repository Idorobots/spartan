#lang racket

(require "../env.rkt")
(require "../pass.rkt")

(provide optimize)

(define (optimize passes)
  (pass (schema "optimize") ;; NOTE Schema depends on the passes.
        (lambda (env)
          (let loop ((i +optimization-loops+)
                     (acc env)
                     (prev '()))
            (if (or (= i 0)
                    (equal? prev acc)) ;; FIXME This is needlessly slow.
                acc
                (loop (- i 1)
                      (foldl run-pass
                             acc
                             passes)
                      acc))))))

(define +optimization-loops+ 23)
