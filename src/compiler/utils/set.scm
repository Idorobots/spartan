;; Set

(define (set . args)
  (sort args symbol<?))

(define (set-difference as bs)
  (filter (lambda (a)
            (not (member a bs)))
          as))

(define (set-union as bs)
  (sort (append as (set-difference bs as))
        symbol<?))

(define (set-sum sets)
  (foldl set-union (set) sets))

