;; Set of symbols

(load-once "compiler/utils/utils.scm")

(define (set-sum sets)
  (apply set-union (set) sets))

(define (set-insert s value)
  (set-add s value))

(define (set-intersection as bs)
  (set-intersect as bs))

(define (set-difference as bs)
  (set-subtract as bs))
