;; Set of symbols

(load-once "compiler/utils/utils.scm")

;; NOTE This is actually performing better than the built-in set implementation.

(define (set? s)
  (list? s))

(define (set . args)
  (sort args symbol<?))

(define (set-empty? set)
  (null? set))

(define (set-difference as bs)
  (filter (lambda (a)
            (not (member a bs)))
          as))

(define (set-union as bs)
  (sort (append as (set-difference bs as))
        symbol<?))

(define (set-sum sets)
  (foldl set-union (set) sets))

(define (set-intersection as bs)
  (filter (partial set-member? as) bs))

(define (set-member? set value)
  (and (member value set) #t))

(define (set-insert s value)
  (set-union s (set value)))

(define (set->list s)
  s)
