#lang racket

(require racket/future)

(provide pmap)

(define +parallel?+ #t)

(define (pmap fun lst)
  (if +parallel?+
      (future-map fun lst)
      (map fun lst)))

(define (future-map fun lst)
  (map touch
       (map (lambda (pass)
              (future
               (lambda ()
                 (fun pass))))
            lst)))
